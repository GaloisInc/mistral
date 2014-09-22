module Mistral.Parser.Finalize (
    finalize
  ) where

import Mistral.Driver
import Mistral.ModuleSystem.Interface ( Iface(..) )
import Mistral.ModuleSystem.Name
import Mistral.ModuleSystem.Prelude ( prelude )
import Mistral.Parser.AST
import Mistral.Utils.PP
import Mistral.Utils.Panic ( panic )
import Mistral.Utils.Source ( Source, Located(..), at, getSource )

import           Control.Applicative ( Applicative(..), (<$>) )
import           Control.Monad ( unless )
import           Data.List ( sortBy )
import qualified Data.Map as Map
import           Data.Monoid ( Monoid(..) )
import           Data.Ord ( comparing )
import           Data.Traversable ( traverse )


type Update a = a -> Driver a

-- | Finalization takes a parsed module, and performs a few tasks to turn it
-- into something suitable for further processing.  Currently this involves:
--
--  * Removing all signatures, and associating them with the bindings they
--    describe
--  * Raising errors if the signatures don't mention something with a binding,
--    or mention the same binding multiple times
--
finalize :: Update Module
finalize m = failErrs $ do
  decls' <- fTopDecls (modDecls m)
  return Module { modName    = modName m
                , modImports = addPrelude (getSource (modName m)) (modImports m)
                , modDecls   = decls' }


-- Import Declarations ---------------------------------------------------------

-- | Insert an import for the Prelude if it wasn't mentioned in the import list.
addPrelude :: Source -> [Import] -> [Import]
addPrelude src is | any importsPrelude is = is
                  | otherwise             = impPrelude : is
  where
  importsPrelude i = impModule i == ifaceModName prelude
  impPrelude       = Import { impModule = ifaceModName prelude
                            , impSource = src }



-- Declaration Fixing ----------------------------------------------------------
--
-- Partition declarations into groups of signatures and bindings.  Resolve names
-- referenced in signatures with their corresponding bindings, adding errors for
-- duplicate signatures or declarations.


fTopDecls :: Update [TopDecl]
fTopDecls decls =
  do let tds = splitTopDecls decls
     binds' <- fDecls fExpr (topDecls tds)
     return (joinTopDecls (tds { topDecls = binds' }))


fDecls :: Update rhs -> Update [Decl rhs]
fDecls rhs ds = resolveSigs =<< traverse (fDecl rhs) ds

resolveSigs :: Update [Decl a]
resolveSigs ds =
  do bs' <- mapM resolveSig bs

     let bindingsFound = Map.fromList [ (bName (locThing b), []) | b <- bs' ]
         remainingSigs = sigs Map.\\ bindingsFound
     unless (Map.null remainingSigs)
         (mapM_ noBinding (Map.toList remainingSigs))

     return [ DSource (getSource lb) (DBind (locThing lb)) | lb <- bs' ]
  where
  (bs,ss) = partitionDecls ds

  -- signature map, from name to list of schema.  Multiple signature definitions
  -- will yield multiple entries in the map.
  sigs = Map.fromListWith (++)
            [ (n,[sigSchema sig `at` locSource n]) | lsig <- ss
                                                   , let sig = locThing lsig
                                                   , n    <- sigNames sig ]

  -- resolve the signature for a single binding
  resolveSig lb@Located { locThing = b } =
    case Map.lookup (bName b) sigs of
      Just [sig] -> return lb { locThing = b { bSig = Just (locThing sig) } }
      Just []    -> panic "Mistral.Parser.Finalize" [ "invalid signature" ]
      Just scs   -> do duplicateSignatures (bName b) scs
                       return lb
      Nothing    -> return lb

-- | Split out declarations into signatures and bindings.  Locations are
-- preserved to help with error reporting.
partitionDecls :: [Decl a] -> ([Located (Bind a)], [Located Signature])
partitionDecls  = foldr sortDecl ([],[])
  where
  sortDecl d0 (bs,ss) = go mempty d0
    where
    go src d =
      case d of
        DBind b      -> (b `at` src:bs,ss)
        DSig s       -> (bs,s `at` src:ss)
        DSource s d' -> go s d'

-- | Report that multiple signatures have been given to the same binding.
duplicateSignatures :: Located Name -> [Located Schema] -> Driver ()
duplicateSignatures name sigs = addErrAt msg (getSource name)
  where
  msg        = hang (text "multiple signatures for binding"
                     <+> backquotes (pp (locThing name)))
                  2 (vcat (map sigDoc (sortBy (comparing locSource) sigs)))
  sigDoc sig = pp (getSource sig)

-- | Report that a signature exists, but no corresponding binding.
noBinding :: (Located Name, a) -> Driver ()
noBinding (name,_) = addErrAt msg (locSource name)
  where
  msg = text "signature, but no binding, for symbol"
    <+> backquotes (pp (locThing name))


-- Traversal -------------------------------------------------------------------

fDecl :: Update a -> Update (Decl a)
fDecl fInner = go
  where
  go d =
    case d of
      DBind b      -> DBind     <$> fBind fInner b
      DSig{}       -> return d
      DSource s d' -> DSource s <$> go d'

fBind :: Update a -> Update (Bind a)
fBind fInner b =
  do body'  <- fInner (bBody b)
     return Bind { bName   = bName b
                 , bSig    = bSig b
                 , bParams = bParams b
                 , bBody   = body' }

fPattern :: Update Pattern
fPattern pat =
  case pat of
    PCon n ps      -> PCon n    <$> traverse fPattern ps
    PTuple ps      -> PTuple    <$> traverse fPattern ps
    PSource s pat' -> PSource s <$> fPattern pat'
    PVar {}        -> pure pat
    PLit {}        -> pure pat
    PWildcard      -> pure pat

fExpr :: Update Expr
fExpr e =
  case e of
    EApp f xs       -> EApp         <$> fExpr f
                                    <*> traverse fExpr xs
    ELet ds b       -> ELet         <$> fDecls fExpr ds
                                    <*> fExpr b
    ELam ps b       -> ELam ps      <$> fExpr b
    ETuple es       -> ETuple       <$> traverse fExpr es
    EList es        -> EList        <$> traverse fExpr es
    EIf c t f       -> EIf          <$> fExpr c
                                    <*> fExpr t
                                    <*> fExpr f
    EActions as     -> EActions     <$> traverse fStmt as
    ETransitions ts -> ETransitions <$> traverse fTransition ts
    ECase s arms    -> ECase        <$> fExpr s <*> traverse fCaseArm arms
    ESource s e'    -> ESource s    <$> fExpr e'
    ETopology t     -> ETopology    <$> fTopology t
    ETaskSet ts     -> ETaskSet     <$> fTaskSet ts
    ESchedule s     -> ESchedule    <$> fSchedule s

    EWildPat{} -> pure e
    ELit{}     -> pure e
    EVar{}     -> pure e
    ECon{}     -> pure e

fCaseArm :: Update CaseArm
fCaseArm ca =
  do pat'  <- fPattern (cPat ca)
     body' <- fExpr (cBody ca)
     return CaseArm { cPat = pat', cBody = body' }

fStmt :: Update Stmt
fStmt s =
  case s of
    SBind p e     -> SBind p    <$> fExpr e
    SAssign p e   -> SAssign p  <$> fExpr e
    SSeq e        -> SSeq       <$> fExpr e
    SLet ds       -> SLet       <$> fDecls fExpr ds
    StSource c s' -> StSource c <$> fStmt s'

fComp :: Update stmt -> Update (Comp stmt)
fComp stmt comp =
  do r'    <- stmt (compResult comp)
     arms' <- traverse fCompArm (compArms comp)
     return Comp { compResult = r'
                 , compArms   = arms'
                 , compTags   = compTags comp
                 , compSource = compSource comp }

fCompArm :: Update CompArm
fCompArm  = traverse fCompStmt

fCompStmt :: Update CompStmt
fCompStmt stmt =
  case stmt of
    CSGen pat e      -> CSGen      <$> fPattern pat <*> fExpr e
    CSGuard e        -> CSGuard    <$> fExpr e
    CSSource s stmt' -> CSSource s <$> fCompStmt stmt'

fTransition :: Update Transition
fTransition t =
  do tpat' <- fTPattern (trPattern t)
     body' <- fExpr (trBody t)
     return Transition { trPattern = tpat'
                       , trBody    = body' }

fTPattern :: Update TPattern
fTPattern tpat =
  case tpat of
    Receive mbE pat  -> Receive mbE <$> fPattern pat
    TPSource s tpat' -> TPSource s  <$> fTPattern tpat'
    Timeout _        -> pure tpat
    At _             -> pure tpat

fTopology :: Update Topology
fTopology t = Topology <$> traverse fTopoStmt (topoElements t)

fTopoStmt :: Update TopoStmt
fTopoStmt t =
  case t of
    TSNode mb n   -> TSNode mb  <$> fNode n
    TSLink l      -> TSLink     <$> fLink l
    TSComp tc     -> TSComp     <$> fComp fTopoStmt tc
    TSUsing e ts  -> TSUsing    <$> fExpr e <*> pure ts
    TSSource s t' -> TSSource s <$> fTopoStmt t'

fNode :: Update Node
fNode n =
  do spec' <- fExpr (nSpec n)
     return Node { nSpec = spec'
                 , nTags = nTags n }

fLink :: Update Link
fLink l = pure l


fTaskSet :: Update TaskSet
fTaskSet ts = TaskSet <$> traverse fTaskStmt (tsTasks ts)

fTaskStmt :: Update TaskStmt
fTaskStmt ts =
  case ts of
    TStTask mb t   -> TStTask mb  <$> fTask t
    TStComp c      -> TStComp     <$> fComp fTaskStmt c
    TStSource s t' -> TStSource s <$> fTaskStmt t'

fTask :: Update Task
fTask t =
  do b' <- fExpr (tBody t)
     return Task { tBody        = b'
                 , tConstraints = tConstraints t
                 , tTags        = tTags t }

fSchedule :: Update Schedule
fSchedule s = Schedule <$> traverse fSchedStmt (sStmts s)

fSchedStmt :: Update SchedStmt
fSchedStmt stmt =
  case stmt of
    Using tasks topo -> Using     <$> fExpr tasks      <*> fExpr topo
    STag stmt' ts    -> STag      <$> fSchedStmt stmt' <*> pure ts
    SExpr e          -> SExpr     <$> fExpr e
    SSource s stmt'  -> SSource s <$> fSchedStmt stmt'
