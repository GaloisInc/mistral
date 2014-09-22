{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

module Mistral.TypeCheck.AST (
    module Mistral.TypeCheck.AST
  , Name(..), nReal
  , RealName(..)
  , Literal(..)
  , Time(..)
  , Atom(..)
  ) where

import Mistral.Driver ( Driver, getBytes, saveBytes )
import Mistral.ModuleSystem.Export ( Export(..) )
import Mistral.ModuleSystem.Name
           ( Name(..), RealName(..), nReal, mkGlobal, objectName )
import Mistral.Parser.AST ( Literal(..), Atom(..), Time(..) )
import Mistral.Utils.Names
import Mistral.Utils.PP
import Mistral.Utils.SCC ( Group(..) )
import Mistral.Utils.Source ( Source(Unknown) )

import           Control.Applicative hiding (empty)
import           Data.Foldable (Foldable)
import           Data.Monoid ( Monoid(..) )
import           Data.Serialize ( Serialize, encodeLazy, decodeLazy )
import qualified Data.Set as Set
import           Data.Traversable (Traversable(traverse))
import           GHC.Generics ( Generic )
import           MonadLib ( BaseM )


data Named a = Named { nName  :: Name
                     , nValue :: a
                     } deriving (Generic,Show,Eq,Ord,Functor,Foldable
                                    ,Traversable)

instance Serialize a => Serialize (Named a)

-- Programs (collections of linked modules) ------------------------------------

data Program = Program { progName  :: Name
                       , progNodes :: [Named Node]
                       , progLinks :: [Link]
                       , progBinds :: [Group Decl]
                       , progDatas :: [Group Data]
                       } deriving (Show,Generic)

instance Serialize Program


-- Modules ---------------------------------------------------------------------

data Module = Module { modName      :: Name
                     , modDeps      :: [Name]
                     , modDatas     :: [Group Data]
                     , modBinds     :: [Group Decl]
                     , modInsts     :: [Inst]
                     } deriving (Show,Generic)

instance Serialize Module

-- | Load a module.
loadModule :: BaseM drv Driver => Name -> drv (Either String Module)
loadModule n =
  do let path = objectName n
     mb <- getBytes path
     case mb of
       Just bytes -> return (decodeLazy bytes)
       Nothing    -> return (Left ("Unable to find object: " ++ path))

-- | Write out a module..
saveModule :: BaseM drv Driver => Module -> drv Bool
saveModule m = saveBytes (objectName (modName m)) (encodeLazy m)


-- Data ------------------------------------------------------------------------

data Data = Data { dName    :: Name
                 , dParams  :: [TParam]
                 , dConstrs :: [Constr]
                 } deriving (Generic,Show)

instance Serialize Data

data Constr = Constr { cName   :: Name
                     , cParams :: [Type]
                     } deriving (Generic,Show)

instance Serialize Constr


-- Bindings --------------------------------------------------------------------

data Bind a = Bind { bName    :: Name
                   , bExport  :: Export
                   , bType    :: Schema
                   , bTParams :: [TParam] -- ^ Type parameters
                   , bCParams :: [Param]  -- ^ Closure parameters
                   , bParams  :: [Param]  -- ^ Arguments
                   , bBody    :: a
                   , bResult  :: Type -- ^ Result type
                   } deriving (Generic,Show,Functor,Foldable,Traversable)

instance Serialize a => Serialize (Bind a)


data Param = Param { pName :: Name, pType :: Type
                   } deriving (Generic,Show)

instance Serialize Param

setExport :: Export -> Bind a -> Bind a
setExport e b = b { bExport = e }


-- Comprehensions --------------------------------------------------------------

data Comp a = Comp { compResult :: a
                   , compArms   :: [CompArm]
                   } deriving (Generic,Show)

instance Serialize a => Serialize (Comp a)

type CompArm = [CompStmt]

data CompStmt = CompGen Pattern Expr
                -- ^ ' n <- e '
              | CompGuard Expr
                -- ^ boolean expression
                deriving (Generic,Show)

instance Serialize CompStmt


-- Schedules -------------------------------------------------------------------

data Sched = Sched { schedStmts :: [SchedStmt]
                   } deriving (Generic,Show)

instance Serialize Sched

instance Monoid Sched where
  mempty        = Sched { schedStmts = [] }
  mappend l r   = Sched { schedStmts = merge schedStmts }
    where
    merge p = mappend (p l) (p r)

data SchedStmt = SchedUsing Expr Expr [Expr]
                 -- ^ ' tasks using topo @ tags '
                 deriving (Generic,Show)

instance Serialize SchedStmt


-- Topologies ------------------------------------------------------------------

type Topology = Bind Topo

data Topo = Topo { topoNodes    :: [Named Node]
                 , topoNodeGens :: [Comp Node]
                 , topoLinks    :: [Link]
                 , topoLinkGens :: [Comp Link]
                 } deriving (Generic,Show)

instance Serialize Topo

instance Monoid Topo where
  mempty        = Topo { topoNodes    = []
                       , topoNodeGens = []
                       , topoLinks    = []
                       , topoLinkGens = [] }

  mappend l r   = Topo { topoNodes    = merge topoNodes
                       , topoNodeGens = merge topoNodeGens
                       , topoLinks    = merge topoLinks
                       , topoLinkGens = merge topoLinkGens
                       }
    where
    merge p = mappend (p l) (p r)

data Node = Node { nSpec  :: Expr
                 , nType  :: Type
                 , nTags  :: [Expr]
                 , nTasks :: [Named Task]
                 } deriving (Generic,Show)

instance Serialize Node

data Link = Link { lLeft, lRight :: Name
                 , lTags         :: [Expr]
                 } deriving (Generic,Show)

instance Serialize Link


-- Task Sets -------------------------------------------------------------------

type TaskSet  = Bind Tasks

data Tasks = Tasks { taskTasks    :: [Named Task]
                   , taskTaskGens :: [Comp Task]
                   } deriving (Generic,Show)

instance Serialize Tasks

instance Monoid Tasks where
  mempty                = Tasks { taskTasks    = []
                                , taskTaskGens = [] }
  mappend l r           = Tasks { taskTasks    = merge taskTasks
                                , taskTaskGens = merge taskTaskGens }
    where
    merge p = mappend (p l) (p r)


data Task = Task { tBody        :: Expr
                 , tConstraints :: [TaskConstraint]
                 , tTags        :: [Expr]
                 } deriving (Generic,Show)

instance Serialize Task


data TaskConstraint = TCOn Type Expr
                      deriving (Generic,Show)

instance Serialize TaskConstraint


-- Expressions -----------------------------------------------------------------

type Decl = Bind Expr

data Action = ABind (Maybe Name) Expr Type
              -- ^ ' p : ty <- e1 : Action ty'
            | AReceive Reset [From] (Maybe Timeout) (Maybe Expr)
              -- ^ Receive messages, optionally timeout, and optionally include
              -- a default action.
              deriving (Generic,Show)

instance Serialize Action

data Reset = DoReset | NoReset
              deriving (Generic, Show, Eq)

instance Monoid Reset where
    mempty = NoReset
    NoReset `mappend` NoReset  = NoReset
    _       `mappend` _        = DoReset

instance Serialize Reset

data Expr = EApp Expr Expr
            -- ^ ' f x1 x2 '
          | ELet [Group Decl] Expr Type
            -- ^ ' let { name = expr } in e : t '
          | EPrim Prim
            -- ^ Primitives
          | EVar Name
            -- ^ ' ident '
          | ECon Name
            -- ^ ' Ident '
          | ECase Source (Match Pattern) Type
            -- ^ ' case x : ty1 of { m } :: ty2 '
          | EStmts Type Type [Action]
            -- ^ Sequenced statements, types are 'i' and 'a' in 'Stmts i a'
          | ELit Literal

          | ETApp Expr [Type]
            -- ^ type application
          | ECApp Expr [Expr]
            -- ^ closure application

          | EMkTuple [(Type,Expr)]
            -- ^ Tuple construction
          | EMkList Type [Expr]
            -- ^ List construction
          | ETopo Topo
            -- ^ 'topology <nodes> <links>'
          | ESchedule Sched
            -- ^ 'schedule ... '
          | ETaskSet Tasks
            -- ^ 'taskset ... '

            deriving (Generic,Show)

instance Serialize Expr


-- | Construct an if-then-else expression via case-expression.
ifE :: Expr -> Expr -> Expr -> Type -> Expr
ifE c t f = ECase Unknown
          $ MCase c boolCon
          $ MSplit (MPat (PCon trueName []) (MExpr t))
                   (MExpr f)

letE :: [Group Decl] -> Expr -> Type -> Expr
letE ds e ty | null ds   = e
             | otherwise = ELet ds e ty

eqE :: Expr -> Expr -> Expr
eqE a b = appE (EVar $ mkGlobal ["Prelude"] "equal") [a,b]

appE :: Expr -> [Expr] -> Expr
appE f xs
  | null xs   = f
  | otherwise = foldl EApp f xs

appC :: Expr -> [Expr] -> Expr
appC f xs
  | null xs   = f
  | otherwise = ECApp f xs

elimEApp :: Expr -> (Expr,[Expr])
elimEApp  = go []
  where
  go xs e = case e of
    EApp e' x -> go (x:xs) e'
    _         -> (e,xs)

tappE :: Expr -> [Type] -> Expr
tappE e tys | null tys  = e
            | otherwise = ETApp e tys

data Prim = PLog
            -- ^ String -> Action ()
          | PSend
            -- ^ Target t, Sendable a => t -> a -> Action ()
          | PResetTimer
            -- ^ Action ()
          | PMyTaskHandle
            -- ^ Action TaskHandle
          | PPure
            -- ^ a -> Action a
          | PGoto
            -- ^ State -> Transition
          | PDone
            -- ^ Transition
          | PSeconds
            -- ^ Time -> Int
          | PFromString
            -- ^ String -> a

          | PAsync
            -- ^ Action () -> Action ()

          | PUptime
            -- ^ Action Time

          | PAddSchedule
            -- ^ Schedule -> Action ()

            -- arithmetic functions
          | PAdd -- ^ Arith a => a -> a -> a
          | PSub -- ^ Arith a => a -> a -> a
          | PNeg -- ^ Arith a => a -> a
          | PMul -- ^ Arith a => a -> a -> a
          | PDiv -- ^ Arith a => a -> a -> a
          | PMod -- ^ Arith a => a -> a -> a
          | PPow -- ^ Arith a => a -> intCon -> a

            -- equality
          | PEq  -- ^ Eq a    => a -> a -> Bool

            -- comparisons
          | PLt  -- ^ Cmp a   => a -> a -> Bool
          | PGt  -- ^ Cmp a   => a -> a -> Bool
          | PLe  -- ^ Cmp a   => a -> a -> Bool
          | PGe  -- ^ Cmp a   => a -> a -> Bool

            -- bitwise operations
          | PLS  -- ^ Bits a  => a -> a -> a
          | PRS  -- ^ Bits a  => a -> a -> a
          | PAnd -- ^ Bits a  => a -> a -> a
          | PXor -- ^ Bits a  => a -> a -> a
          | POr  -- ^ Bits a  => a -> a -> a
          | PNot -- ^ Bits a  => a -> a

            -- string functions
          | PString   -- ^ Stringable a => a -> String
          | PStr2Atom -- ^ String -> Atom

            deriving (Show,Generic,Eq,Ord)

instance Serialize Prim


data Match pat = MCase Expr Type (Match pat)
                 -- ^ Case analysis on an expression.
               | MRename Name Expr Type (Match pat)
                 -- ^ Rename an expression in a match.
                 -- ' let n = e : ety in m '
               | MPat pat (Match pat)
                 -- ^ Attempt to apply this pattern to the expression given
                 -- ' (pat <- e : ty) -> m '
               | MSplit (Match pat) (Match pat)
                 -- ^ Try to match the lhs, falling back on the rhs if an MFail is
                 -- encountered'
                 -- ' lhs [] rhs '
               | MGuard Expr (Match pat)
               -- ^ Evaluate the guard wrt the current environment,
               -- including bindings due to any preceding pattern.
               --  ' e : ty | guardExpr -> m '
               | MFail
                 -- ^ Pattern matching failed.
                 -- ' FAIL '
               | MExpr Expr
                 -- ^ Commit to this branch, and continue executing expr.
                 -- ' e '
                 deriving (Generic,Show)

instance Serialize pat => Serialize (Match pat)

elimMSplits :: Match pat -> [Match pat]
elimMSplits m = case m of
  MSplit l r -> elimMSplits l ++ elimMSplits r
  _          -> [m]

isTuple :: Type -> Bool
isTuple ty = con == tupleCon
  where
  (con,_) = elimTApp ty

-- | True when a Match contains a pattern that will always succeed.  Note that
-- this only considers one branch, so MCase and MSplit will be False.
isDefault :: Match Pattern -> Bool
isDefault m = case m of
  MCase{}          -> True
  MExpr{}          -> True
  MRename _ _ _ m' -> isDefault m'
  MPat _ m'        -> irrefutable m && hasDefault m'
  _                -> False

-- | True when the match can never fail.
irrefutable :: Match Pattern -> Bool
irrefutable m = case m of
  -- NOTE: tuple patterns are the only irrefutable pattern we have, currently
  MPat pat _       -> isTuplePat pat
  MRename _ _ _ m' -> irrefutable m'
  MExpr{}          -> True
  _                -> False

-- | True when the pattern matches a tuple.
isTuplePat :: Pattern -> Bool
isTuplePat pat = case pat of
  PTuple _ -> True
  _        -> False

-- | True when any branch of a Match contains a pattern that will accept
-- pattern.
hasDefault :: Match Pattern -> Bool
hasDefault m = case m of
  MCase _ _ m' -> any isDefault (elimMSplits m')
  _            -> isDefault m

referenceTo :: Name -> Match Pattern -> Bool
referenceTo n m = renamingOf n m || caseOf n m

caseOf :: Name -> Match Pattern -> Bool
caseOf n m =
  case m of
    MCase (EVar x) _ m' -> x == n || caseOf n m'
    MCase _ _ m'        -> caseOf n m'
    MPat _ m'           -> caseOf n m'
    MSplit m1 m2        -> caseOf n m1 || caseOf n m2
    _                   -> False

renamingOf :: Name -> Match Pattern -> Bool
renamingOf n m =
  case m of
    MRename _ (EVar n') _ m' -> n == n' || renamingOf n m'
    MCase _ _ m'             -> renamingOf n m'
    MPat _ m'                -> renamingOf n m'
    MSplit m1 m2             -> renamingOf n m1 || renamingOf n m2
    _                        -> False

data From = From { fSource      :: Name
                 , fMessage     :: Name
                 , fMessageType :: Type
                 , fBody        :: Match Pattern
                 } deriving (Generic,Show)

instance Serialize From

data Timeout = Timeout { toTimeout :: Expr
                       , toBody    :: Expr
                       } deriving (Generic,Show)

instance Serialize Timeout

-- True if the timeout is a literal zero
timeoutZero :: Timeout -> Bool
timeoutZero (Timeout (ELit (LTime (Time hh mm ss ns))) _) = all (==0) [hh,mm,ss,ns]
timeoutZero _ = False

data Pattern = PCon Name [Maybe Param]
               -- ^ ' M:C a _ c '
             | PTuple [Maybe Param]
               -- ^ ' (a1:t1, ... , an:tn) '
             | PLit Literal
               -- ^ ' lit '
               deriving (Generic,Show)

instance Serialize Pattern

data SourcePredicate
        = SNoPred  Name       -- "src ? ..." where src :: TaskHandle
        | SSrcIs   Name       -- "src ? ... | src == x" where x is a pre-existing binding
        | STagPred [Expr]     -- "_ ? ..." or "? ..." or "#a ? ..." or "(#a,#b) ? ..."
                deriving (Show,Generic)

instance Serialize SourcePredicate


-- Types -----------------------------------------------------------------------

data Schema = Forall { sParams :: [TParam]
                     , sProps  :: [Prop]
                     , sType   :: Type
                     } deriving (Show,Eq,Generic)

instance Serialize Schema

mkSchema :: Type -> Schema
mkSchema  = Forall [] []

data Inst = Inst { iProp :: Schema
                   -- ^ Parameters are variables in an instance, the `a` in
                   -- `List a`, for example
                 } deriving (Generic,Show)

instance Serialize Inst


type Prop = Type

data Type = TApp Type Type
          | TSyn Name [Type] Type
            -- ^ Synonym name, parameters, and currently expanded form
          | TCon Name
          | TVar TVar
            deriving (Show,Eq,Ord,Generic)

instance Serialize Type

-- | Apply one type to many arguments.
tapp :: Type -> [Type] -> Type
tapp  = foldl TApp

-- | Does this type contain any (bound or unbound) variables?
isPoly :: Type -> Bool
isPoly ty = case ty of
  TApp l r     -> isPoly l || isPoly r
  TSyn _ _ ty' -> isPoly ty'
  _            -> isTVar ty

isTVar :: Type -> Bool
isTVar ty = case ty of
  TVar{} -> True
  _      -> False

elimTApp :: Type -> (Type,[Type])
elimTApp  = go []
  where
  go xs ty = case ty of
    TApp f x -> go (x:xs) f
    _        -> (ty,xs)

elimTuple :: Type -> Maybe [Type]
elimTuple ty = case elimTApp ty of
  (f,xs) | f == tupleCon -> Just xs
         | otherwise     -> Nothing

-- | Eliminate sequential applications of the tarrow constructor.
elimTArrows :: Type -> [Type]
elimTArrows ty = case elimTApp ty of
  (con,[a,b]) | con == funCon -> a : elimTArrows b
  _                           -> [ty]

data TySyn = TySyn { synName :: Name
                   , synType :: Schema
                   } deriving (Show,Eq,Generic)

instance Serialize TySyn

tsynArity :: TySyn -> Int
tsynArity syn = length (sParams (synType syn))


data TVar = TVBound TParam
          | TVFree  TParam
            deriving (Show,Eq,Ord,Generic)

instance Serialize TVar

data TParam = TParam { tpUserName :: Maybe Name
                     , tpIndex    :: Int
                     } deriving (Show,Eq,Ord,Generic)

instance Serialize TParam


-- Constant Values -------------------------------------------------------------

trueE :: Expr
trueE = EVar trueName

falseE :: Expr
falseE = EVar falseName


-- Constant Types --------------------------------------------------------------

funCon :: Type
funCon  = TCon (mkGlobal ["Prelude"] "(->)")

-- | Construt a function arrow.
tarrow :: Type -> Type -> Type
tarrow a b = TApp (TApp funCon a) b
infixr 9 `tarrow`

tupleCon :: Type
tupleCon  = TCon (mkGlobal ["Prelude"] "()")

unitCon :: Type
unitCon  = tupleCon

ttuple :: [Type] -> Type
ttuple  = foldl TApp tupleCon

listCon :: Type -- :: * -> *
listCon  = TCon (mkGlobal ["Prelude"] "[]")

isListType :: Type -> Bool
isListType (TApp c _) = c == listCon
isListType _ = False

tlist :: Type -> Type
tlist  = TApp listCon

atomCon :: Type
atomCon  = TCon (mkGlobal ["Prelude"] "Atom")

tagCon :: Type
tagCon  = TCon (mkGlobal ["Prelude"] "Tag")

intCon :: Type
intCon  = TCon (mkGlobal ["Prelude"] "Int")

stringCon :: Type
stringCon  = TCon (mkGlobal ["Prelude"] "String")

boolCon :: Type
boolCon  = TCon (mkGlobal ["Prelude"] "Bool")

tmaybe :: Type -> Type
tmaybe = TApp maybeCon

maybeCon :: Type
maybeCon = TCon (mkGlobal ["Prelude"] "Maybe")

openCon :: Type -- :: Index
openCon  = TCon (mkGlobal ["Prelude"] "Open")

closedCon :: Type -- :: Index
closedCon  = TCon (mkGlobal ["Prelude"] "Closed")

stmtsCon :: Type -- :: Index -> * -> *
stmtsCon  = TCon (mkGlobal ["Prelude"] "Stmts")

tstmts :: Type -> Type -> Type
tstmts i a = tapp stmtsCon [i,a]

actionCon :: Type
actionCon  = TCon (mkGlobal ["Prelude"] "Action")

isAction :: Type -> Bool
isAction ty = case ty of
  TSyn _ _ (TApp (TApp con ix) _) -> con == stmtsCon && ix  == openCon
  _                               -> False

taction :: Type -> Type
taction a =
  TSyn (mkGlobal ["Prelude"] "Action") [a] (tstmts openCon a)

transitionCon :: Type
transitionCon  =
  TSyn (mkGlobal ["Prelude"] "Transition") [] (tstmts closedCon stateCon)

stateCon :: Type
stateCon  = TCon (mkGlobal ["Prelude"] "State")

ipv4MaskCon :: Type
ipv4MaskCon = TCon (mkGlobal ["Prelude"] "IPv4Mask")

ipv6MaskCon :: Type
ipv6MaskCon = TCon (mkGlobal ["Prelude"] "IPv6Mask")

timeCon :: Type
timeCon  = TCon (mkGlobal ["Prelude"] "Time")

ipv4Con :: Type
ipv4Con  = TCon (mkGlobal ["Prelude"] "IPv4")

trueName :: Name
trueName  = mkGlobal ["Prelude"] "True"

falseName :: Name
falseName  = mkGlobal ["Prelude"] "False"

topoCon :: Type
topoCon  = TCon (mkGlobal ["Prelude"] "Topology")

taskCon :: Type
taskCon  = TCon (mkGlobal ["Prelude"] "Task")

taskHandleCon :: Type
taskHandleCon  = TCon (mkGlobal ["Prelude"] "TaskHandle")

taskSetCon :: Type
taskSetCon  = TCon (mkGlobal ["Prelude"] "TaskSet")

nodeCon :: Type
nodeCon  = TCon (mkGlobal ["Prelude"] "Node")

scheduleCon :: Type
scheduleCon  = TCon (mkGlobal ["Prelude"] "Schedule")


-- Constant Props --------------------------------------------------------------

sendTargetCon :: Prop -- :: * -> Prop
sendTargetCon  = TCon (mkGlobal ["Prelude"] "SendTarget")

-- | Things that can receive messages.
sendTargetProp :: Type -> Prop
sendTargetProp ty = tapp sendTargetCon [ty]

serializableCon  :: Prop
serializableCon  = TCon (mkGlobal ["Prelude"] "Serializable")

-- | Things that can be sent between tasks.
serializableProp :: Type -> Prop
serializableProp ty = tapp serializableCon [ty]

linkTargetCon :: Prop -- :: * -> Prop
linkTargetCon  = TCon (mkGlobal ["Prelude"] "LinkTarget")

-- | Things that are valid arguments to the (<->) link constructor.
linkTargetProp :: Type -> Prop
linkTargetProp ty = tapp linkTargetCon [ty]

taskTargetCon :: Prop
taskTargetCon  = TCon (mkGlobal ["Prelude"] "TaskTarget")

-- | Things that are valid values for the `on` constraint.
taskTargetProp :: Type -> Prop
taskTargetProp ty = tapp taskTargetCon [ty]

nodeSpecCon :: Prop
nodeSpecCon  = TCon (mkGlobal ["Prelude"] "NodeSpec")

nodeSpecProp :: Type -> Prop
nodeSpecProp ty = tapp nodeSpecCon [ty]

eqPropCon :: Prop
eqPropCon  = TCon (mkGlobal ["Prelude"] "(~)")

-- | Assert equality between two types.  This boils down to solving via
-- unification.
(~~) :: Type -> Type -> Prop
a ~~ b = tapp eqPropCon [a,b]

arithCon :: Prop -- :: * -> Prop
arithCon  = TCon (mkGlobal ["Prelude"] "Arith")

-- | Things that support arithmetic operations.
arithProp :: Type -> Prop
arithProp a = tapp arithCon [a]

eqCon :: Prop -- :: * -> Prop
eqCon  = TCon (mkGlobal ["Prelude"] "Eq")

-- | Things that support equality operations.
eqProp :: Type -> Prop
eqProp a = tapp eqCon [a]

cmpCon :: Prop -- :: * -> Prop
cmpCon  = TCon (mkGlobal ["Prelude"] "Cmp")

-- | Things that can be compared.
cmpProp :: Type -> Prop
cmpProp a = tapp cmpCon [a]

bitsCon :: Prop -- :: * -> Prop
bitsCon  = TCon (mkGlobal ["Prelude"] "Bits")

-- | Things that support bitwise operations.
bitsProp :: Type -> Prop
bitsProp a = tapp bitsCon [a]

sliceCon :: Prop
sliceCon = TCon (mkGlobal ["Prelude"] "Slice")

-- Things that can be sliced and joined (take, drop, concat)
sliceProp :: Type -> Prop
sliceProp a = tapp sliceCon [a]

-- Things that can be converted to a string
stringableCon :: Prop
stringableCon = TCon (mkGlobal ["Prelude"] "Stringable")

stringableProp :: Type -> Prop
stringableProp a = tapp stringableCon [a]

-- Things that can be converted from a string
fromStringCon :: Prop
fromStringCon = TCon (mkGlobal ["Prelude"] "FromString")

fromStringProp :: Type -> Prop
fromStringProp a = tapp fromStringCon [a]

-- Bound Variables -------------------------------------------------------------

instance BoundVars Program where
  boundVars prog = boundVars (progBinds prog, progDatas prog)

instance BoundVars Data where
  boundVars d = boundVars (dConstrs d)

instance BoundVars Constr where
  boundVars c = Set.singleton (cName c)

instance BoundVars (Bind a) where
  boundVars b = Set.singleton (bName b)

instance BoundVars Param where
  boundVars p = Set.singleton (pName p)

instance BoundVars Pattern where
  boundVars p = case p of
    PCon _ ns -> boundVars ns
    PTuple ps -> boundVars ps
    _         -> Set.empty

instance BoundVars SourcePredicate where
  boundVars sp = case sp of
        SNoPred  n  -> Set.singleton n
        SSrcIs   _  -> Set.empty
        STagPred _  -> Set.empty

instance BoundVars pat => BoundVars (Match pat) where
  boundVars m = case m of
    MCase _ _ m'     -> boundVars m'
    MRename n _ _ m' -> Set.insert n (boundVars m')
    MPat p m'        -> boundVars p `Set.union` boundVars m'
    MSplit l r       -> boundVars l `Set.union` boundVars r
    MFail            -> Set.empty
    MExpr _          -> Set.empty
    MGuard _ m'      -> boundVars m'

instance BoundVars Action where
  boundVars a = case a of
    ABind mb _ _ -> boundVars mb
    _            -> Set.empty

instance BoundVars a => BoundVars (Comp a) where
  boundVars (Comp r as) =
      boundVars r `Set.union` boundVars as

instance BoundVars CompStmt where
  boundVars (CompGen p _) = boundVars p
  -- boundVars (CompGuard e) = boundVars e
  -- Can't 'let' in guard yet... right?
  boundVars (CompGuard _) = Set.empty

instance BoundVars (Named a) where
  boundVars (Named n _) = Set.singleton n

-- Free Variables --------------------------------------------------------------

instance FreeVars a => FreeVars (Bind a) where
  freeVars b = freeVars (bBody b) Set.\\ boundVars (bCParams b, bParams b)

instance FreeVars Expr where
  freeVars e = case e of
    EApp l r         -> freeVars (l,r)
    ELet ds b _      -> freeVars (ds,b)
    EPrim _          -> Set.empty
    EVar n           -> Set.singleton n
    ECon n           -> Set.singleton n
    ECase _ arms _   -> freeVars arms
    EStmts _ _ as    -> fvActions as
    ELit _           -> Set.empty
    ETApp b _        -> freeVars b
    ECApp b es       -> freeVars (b,es)
    EMkTuple elems   -> freeVars (map snd elems)
    EMkList _ es     -> freeVars es
    ETopo t          -> freeVars t
    ESchedule s      -> freeVars s
    ETaskSet t       -> freeVars t

instance FreeVars Topo where
  freeVars (Topo ns ngs ls lgs) =
    Set.unions [ freeVars ns, freeVars ngs, freeVars ls, freeVars lgs ]
     Set.\\ boundVars ns

instance FreeVars Node where
  freeVars (Node s _ ts as) = freeVars (s,ts,as)

instance FreeVars Link where
  freeVars (Link l r ts) = freeVars l `Set.union` freeVars r `Set.union` freeVars ts

instance FreeVars Task where
  freeVars (Task ts tsg tags) = freeVars (ts,tsg,tags)

instance FreeVars TaskConstraint where
  freeVars (TCOn _ e) = freeVars e

instance FreeVars Sched where
  freeVars (Sched ss) = freeVars ss

instance FreeVars SchedStmt where
  freeVars (SchedUsing e1 e2 es) = freeVars (e1:e2:es)

instance FreeVars Tasks where
 freeVars (Tasks tt ttg) = freeVars (tt,ttg) Set.\\ boundVars tt

instance FreeVars SourcePredicate where
  freeVars s = case s of
    SNoPred  _  -> Set.empty
    SSrcIs   _  -> Set.empty
    STagPred t  -> freeVars t

instance (BoundVars pat, FreeVars pat) => FreeVars (Match pat) where
  freeVars m = case m of
    MCase s _ m'     -> freeVars (s,m')
    MRename n s _ m' -> freeVars s `Set.union` Set.delete n (freeVars m')
    MGuard g m'      -> freeVars (g,m')
    MPat p m'        -> freeVars (p,m') Set.\\ boundVars p
    MSplit l r       -> freeVars (l,r)
    MExpr e          -> freeVars e
    MFail            -> Set.empty

instance FreeVars From where
  freeVars f = freeVars (fBody f)

instance FreeVars Timeout where
  freeVars to = freeVars (toTimeout to, toBody to)

-- only constructors are free vars in a pattern
instance FreeVars Pattern where
  freeVars pat = case pat of
    PCon n _ -> Set.singleton n
    _        -> Set.empty

fvActions :: [Action] -> Set.Set Name
fvActions  = foldr step Set.empty
  where
  step a fvs = freeVars a `Set.union` (fvs Set.\\ boundVars a)

instance FreeVars Action where
  freeVars a = case a of
    ABind _ e _           -> freeVars e
    AReceive _ fs to wild -> freeVars (fs,to,wild)

instance FreeVars a => FreeVars (Comp a) where
  freeVars (Comp r as) =
      (freeVars r `Set.union` freeVars as)
       Set.\\
      boundVars as

instance FreeVars CompStmt where
  freeVars (CompGen _ e) = freeVars e
  freeVars (CompGuard e) = freeVars e

instance FreeVars a => FreeVars (Named a) where
  freeVars o = freeVars (nValue o)

-- Pretty Printing -------------------------------------------------------------

instance PP Program where
  ppr p = vcat $ concat
    [ map pp (progNodes p)
    , map pp (progLinks p)
    , concat [ ppGroup (return . pp) g | g <- progDatas p ]
    , concat [ ppGroup ppBind        g | g <- progBinds p ]
    ]

instance PP Module where
  ppr m = text "module" <+> pp (modName m) <+> text "where" $$ block decls
    where
    decls = concat [ concat [ ppGroup (return . pp) g | g <- modDatas m ]
                   , concat [ ppGroup ppBind        g | g <- modBinds m ] ]

ppGroup :: (a -> [PPDoc]) -> Group a -> [PPDoc]
ppGroup f g = case g of
  NonRecursive b -> addHeader "non-recursive" (f b)
  Recursive bs   -> addHeader "recursive" (concatMap f bs)
  where
  addHeader str docs = case docs of
    d:rest -> ((text "--" <+> text str) $$ d) : rest
    []     -> []

ppBind :: PP a => Bind a -> [PPDoc]
ppBind b =
  [ text "--" <+> pp (bExport b)
    $$ pp (bName b) <+> char ':' <+> pp (bType b)
  , hang (hsep decl <+> char ':' <+> pp (bResult b) <+> char '=')
       2 (pp (bBody b)) ]
  where
  decl = pp (bName b)
       : brackets (hsep (map pp (bTParams b)))
       : braces (hsep (map (ppPrec 10) (bCParams b)))
       : map (ppPrec 10) (bParams b)

ppTags :: [Expr] -> PPDoc
ppTags tags = case tags of
  []  -> empty
  [x] -> char '@' <+> pp x
  _   -> char '@' <+> parens (commas (map pp tags))

instance PP Param where
  ppr p = parens (pp (pName p) <+> char ':' <+> pp (pType p))

instance PP a => PP (Named a) where
  ppr (Named n a) = hang (pp n <+> char '=')
                       2 (pp a)

instance PP Data where
  ppr d = hang (text "data" <+> pp (dName d) <+> hsep (map pp (dParams d)))
             2 (sep (char '=' : punctuate (char '|') (map pp (dConstrs d))))

instance PP Constr where
  ppr c = pp (cName c) <+> sep (map (ppPrec 10) (cParams c))


instance PP Sched where
  ppr s = hang (text "schedule")
             2 (block (map pp (schedStmts s)))

instance PP SchedStmt where
  ppr s = case s of
    SchedUsing tasks topo tags ->
      hsep [ ppPrec 10 tasks, text "using", ppPrec 10 topo, ppTags tags ]



instance PP Topo where
  ppr t = hang (text "topology")
             2 (block decls)
    where
    decls = concat [ [ pp n | n <- topoNodes    t ]
                   , [ pp c | c <- topoNodeGens t ]
                   , [ pp l | l <- topoLinks    t ]
                   , [ pp c | c <- topoLinkGens t ] ]

instance PP Node where
  ppr n = hang (text "node" <+> ppPrec 10 (nSpec n) <+> ppTags (nTags n))
             2 tasks
    where
    tasks | null (nTasks n) = empty
          | otherwise       = hang (text "scheduled tasks")
                                 2 (vcat (map pp (nTasks n)))


instance PP Link where
  ppr l = hsep [ pp (lLeft l), text "<->", pp (lRight l), ppTags (lTags l) ]

instance PP Tasks where
  ppr ts = hang (text "taskSet")
              2 (block decls)
    where
    decls = concat [ [ pp t | t <- taskTasks    ts ]
                   , [ pp g | g <- taskTaskGens ts ] ]

instance PP Task where
  ppr t = hang (text "task" <+> ppPrec 10 (tBody t))
             2 (vcat (map pp (tConstraints t) ++ [ ppTags (tTags t) ]))


instance PP TaskConstraint where
  ppr tc = case tc of
    TCOn _ e -> text "on" <+> ppPrec 10 e


instance PP a => PP (Comp a) where
  ppr c = brackets comp
    where
    comp = hang (pp (compResult c))
              2 (vcat (map ppCompArm (compArms c)))

    ppCompArm cs = char '|' <+> commas (map pp cs)

instance PP CompStmt where
  ppr cs = case cs of
    CompGen pat gen -> pp pat <+> text "<-" <+> pp gen
    CompGuard e     -> pp e


instance PP From where
  ppr from =
      hang (hsep [ pp (fSource from), char '?'
                 , pp (fMessage from), char ':', ppPrec 10 (fMessageType from) ])
         2 (pp (fBody from))

instance PP Timeout where
  ppr to =
      hang (text "timeout" <+> pp (toTimeout to))
         2 (pp (toBody to))

instance PP Pattern where
  ppr pat = case pat of
    PCon c ps
      | null ps   -> pp c
      | otherwise -> precParens 10 (hsep (pp c : map ppMb ps))

    PTuple ps     -> parens (commas (map ppMb ps))

    PLit lit      -> pp lit

    where
    ppMb = maybe (char '_') pp


instance PP Expr where
  ppr e = case e of
    EApp f x  -> precParens 10
               $ hang (pp f)
                    2 (ppPrec 10 x)

    ELet ds b _ -> precParens 10
               $ hang (text "let") 4 (block (concatMap (ppGroup ppBind) ds))
              $$ nest 1 (text "in" <+> pp b)

    EVar n -> pp n

    ECon n -> pp n

    EPrim p -> ppr p

    ECase _ cases _ -> precParens 10 (pp cases)

    EStmts i ty a -> precParens 10
                   $ hang (text "action" <+> brackets (commas [pp i,pp ty]))
                        8 (block (map pp a))

    ELit lit -> pp lit

    ETApp e' tys -> precParens 10
                  $ hcat [ pp e', char '@'
                         , brackets (commas (map (ppPrec 10) tys)) ]

    ECApp f cs -> precParens 10
                $ pp f <+> braces (commas (map (ppPrec 10) cs))

    EMkTuple elems -> parens (commas (map (pp . snd) elems))

    EMkList _ elems -> brackets (commas (map pp elems))
    ETopo t         -> ppr t
    ESchedule s     -> ppr s
    ETaskSet  t     -> ppr t

instance PP pat => PP (Match pat) where
  ppr m = case m of

    MCase s sty m' ->
      precParens 10 $
        hang (text "case" <+> pp s <+> char ':' <+> pp sty <+> text "of")
           2 (pp m')

    MRename n e ty m' ->
      precParens 10 $
        hang (text "let" <+> pp n <+> char '=' <+> pp e <+> char ':' <+> pp ty)
           1 (text "in" <+> pp m')

    MPat pat m' ->
      hang (pp pat <+> text "->")
         2 (pp m')

    MGuard g m' ->
      pp m' <+> char '|' <+> pp g

    MSplit l r -> ppr l $$ ppr r

    MFail -> text "FAIL"

    MExpr e -> pp e

instance PP Prim where
  ppr p = case p of
    PLog               -> text "log"
    PSend              -> text "send"
    PResetTimer        -> text "resetTimer"
    PMyTaskHandle      -> text "myTaskHandle"
    PPure              -> text "pure"
    PGoto              -> text "goto"
    PDone              -> text "done"
    PSeconds           -> text "seconds"
    PFromString        -> text "read"
    PAsync             -> text "async"
    PUptime            -> text "uptime"
    PAddSchedule       -> text "addSchedule"

    PAdd -> text "add"
    PSub -> text "sub"
    PNeg -> text "neg"
    PMul -> text "mul"
    PDiv -> text "div"
    PMod -> text "mod"
    PPow -> text "pow"
    PEq  -> text "eq"
    PLt  -> text "lt"
    PGt  -> text "gt"
    PLe  -> text "le"
    PGe  -> text "ge"
    PLS  -> text "ls"
    PRS  -> text "rs"
    PAnd -> text "and"
    PXor -> text "xor"
    POr  -> text "or"
    PNot -> text "not"

    PString   -> text "string"
    PStr2Atom -> text "stringToAtom"

instance PP Action where
  ppr a = case a of

    ABind mb l ty
      | Just pat <- mb -> hsep [ pp pat, text "<-", pp l, char ':', pp ty ]
      | otherwise      -> hsep [                    pp l, char ':', pp ty ]

    AReceive DoReset froms mbTimeout mbWild ->
      vcat [text ("resetTimer"), ppr (AReceive NoReset froms mbTimeout mbWild)]
    AReceive NoReset froms mbTimeout mbWild ->
      hang (text "receive")
         2 (vcat cases)
      where
      cases = map pp froms
           ++ [ maybe empty pp mbTimeout
              , maybe empty ppWild mbWild ]

      ppWild e = hang (char '_' <+> text "->")
                    2 (pp e)

instance PP Schema where
  ppr (Forall ps props ty) = sep [ params <+> context, pp ty ]
    where
    params | null ps   = empty
           | otherwise = text "forall" <+> hsep (map pp ps) <> char '.'

    context | null props = empty
            | otherwise  = ppContext props <+> text "=>"

ppContext :: [Prop] -> PPDoc
ppContext props = parens (commas (map pp props))

instance PP TySyn where
  ppr syn = pp (synName syn) <+> hsep (map pp args) <+> char '=' <+> pp body
    where
    args = sParams (synType syn)
    body = sType (synType syn)

instance PP Inst where
  ppr i = text "instance" <+> pp (iProp i)

instance PP Type where
  ppr ty = case ty of

    -- infix type operators
    TApp (TApp op l) r
      | op == funCon -> precParens 9 (sep [ ppPrec 9 l <+> text "->", pp r ])
      | op == eqCon  -> pp l <+> char '~' <+> pp r

    TApp l a
      | l == listCon -> brackets (pp a)

    TApp {} ->
      let (f,xs) = elimTApp ty
       in if | f == tupleCon -> parens (commas (map pp xs))
             | otherwise     -> precParens 10 (hang (pp f) 2 (sep (map (ppPrec 10) xs)))

    TCon str ->
      pp str

    TVar tv ->
      ppr tv

    TSyn f xs _ ->
      ppr f <+> hsep (map ppr xs)

instance PP TVar where
  ppr tv = case tv of

    TVFree tp  -> char '?' <> pp tp
    TVBound tp -> pp tp

instance PP TParam where
  ppr tp = case tpUserName tp of
    Just n  -> pp n
    Nothing -> char 't' <> int (tpIndex tp)

class HasVars a where
  vars :: Applicative f => (Name -> f Name) -> a -> f a

instance HasVars a => HasVars (Maybe a) where
  vars f = traverse (vars f)

instance HasVars a => HasVars [a] where
  vars f = traverse (vars f)

instance HasVars Param where
  vars f p = Param <$> f (pName p) <*> pure (pType p)

instance HasVars Expr where
  vars f = go
    where
    go (EApp x y)               = EApp      <$> go x <*> go y
    go (ELet xs e t)            = ELet      <$> traverse (traverse (traverse go)) xs <*> go e <*> pure t
    go (EPrim p)                = pure (EPrim p)
    go (EVar n)                 = EVar      <$> f n
    go (ECase src cases t2)     = ECase src <$> vars f cases <*> pure t2
    go (EStmts i t xs)          = EStmts i t <$> traverse (vars f) xs
    go (ETApp e t)              = ETApp     <$> go e <*> pure t
    go (ECon n)                 = pure (ECon n)
    go (ELit l)                 = pure (ELit l)
    go (ECApp n es)             = ECApp     <$> go n <*> traverse go es
    go (EMkTuple elems)         = let (tys,es) = unzip elems
                                   in EMkTuple . zip tys <$> traverse go es
    go (EMkList ty elems)       = EMkList ty <$> traverse go elems
    go (ETopo t)                = ETopo      <$> vars f t
    go (ESchedule s)            = ESchedule  <$> vars f s
    go (ETaskSet t)             = ETaskSet   <$> vars f t

instance HasVars Tasks where
  vars f = go
   where
   go (Tasks ts tgens) = Tasks <$> traverse (vars f) ts <*> traverse (vars f) tgens

instance HasVars Topo where
  vars f = go
    where
    go (Topo nodes nodeGens links linkGens) =
        Topo <$> traverse (vars f) nodes <*> traverse (vars f) nodeGens
             <*> traverse (vars f) links <*> traverse (vars f) linkGens

instance HasVars Sched where
  vars f = go
   where
   go (Sched ss) = Sched <$> traverse (vars f) ss

instance HasVars Node where
  vars f = go
   where
   go (Node s sty t as) = Node <$> vars f s <*> pure sty
                                            <*> traverse (vars f) t
                                            <*> traverse (vars f) as

instance HasVars Link where
  vars f = go
   where
   go (Link l r t) = Link <$> f l <*> f r <*> traverse (vars f) t

instance HasVars Task where
  vars f = go
   where
   go (Task b cs ts) = Task <$> vars f b <*> traverse (vars f) cs <*> pure ts

instance HasVars TaskConstraint where
  vars f = go
   where
   go (TCOn ty e) = TCOn ty <$> vars f e

instance HasVars SchedStmt where
  vars f = go
   where
   go (SchedUsing task topo tags) =
       SchedUsing <$> vars f task <*> vars f topo <*> pure tags

instance (HasVars a) => HasVars (Named a) where
  vars f n = traverse (vars f) n

instance (HasVars a) => HasVars (Comp a) where
  vars f = go
   where
   go (Comp r as) = Comp <$> vars f r <*> traverse (traverse (vars f)) as

instance HasVars CompStmt where
  vars f (CompGen pat e) = CompGen   <$> vars f pat <*> vars f e
  vars f (CompGuard e)   = CompGuard <$> vars f e

instance HasVars pat => HasVars (Match pat) where
  vars f = go
    where
    go (MCase s sty m')    = MCase   <$> vars f s   <*> pure sty <*> vars f m'
    go (MRename n e ty m') = MRename <$> f n <*> vars f e <*> pure ty <*> vars f m'
    go (MGuard g m')       = MGuard  <$> vars f g <*> vars f m'
    go (MPat pat m')       = MPat    <$> vars f pat <*> go m'
    go (MSplit l r)        = MSplit  <$> go l       <*> go r
    go MFail               = pure MFail
    go (MExpr e)           = MExpr   <$> vars f e

instance HasVars Action where
  vars f (ABind p e ty)          = ABind p    <$> vars f e <*> pure ty
  vars f (AReceive r fs to wild) = AReceive r <$> traverse (vars f) fs <*> traverse (vars f) to
                                                                      <*> traverse (vars f) wild

instance HasVars Pattern where
  vars f (PCon c xs)            = PCon c    <$> vars f xs
  vars f (PTuple ps)            = PTuple    <$> traverse (vars f) ps
  vars _ (PLit l)               = pure (PLit l)

instance HasVars From where
  vars f (From src msg msgTy body) = From <$> f src <*> f msg <*> pure msgTy <*> vars f body

instance HasVars Timeout where
  vars f (Timeout to body) = Timeout to <$> vars f body
