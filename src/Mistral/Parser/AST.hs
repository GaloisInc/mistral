{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Mistral.Parser.AST where

import Mistral.ModuleSystem.Name
import Mistral.Utils.Names
import Mistral.Utils.PP
import Mistral.Utils.Source

import           Data.Char (intToDigit)
import           Data.Foldable ( foldMap )
import           Data.Monoid ( Monoid(..) )
import           Data.Serialize ( Serialize )
import qualified Data.Set as Set
import           GHC.Generics ( Generic )
import           Numeric (showIntAtBase)


-- Modules----------------------------------------------------------------------

data Module = Module
  { modName    :: Located Name
  , modImports :: [Import]
  , modDecls   :: [TopDecl]
  } deriving (Show)


data Import = Import { impModule :: Name
                     , impSource :: Source
                     } deriving (Show)


-- Declarations ----------------------------------------------------------------

-- | A declaration, with bindings parameterized by their body type.
data Decl a = DBind (Bind a)
            | DSig Signature
            | DSource Source (Decl a)
              deriving (Show,Functor)

data Bind a = Bind { bName   :: Located Name
                   , bSig    :: Maybe Schema
                   , bParams :: [Located Name]
                   , bBody   :: a
                   } deriving (Show,Functor)

data Signature = Signature { sigNames  :: [Located Name]
                           , sigSchema :: Schema
                           } deriving (Show)


-- Top Declarations ------------------------------------------------------------

data TopDecl = TDecl (Decl Expr)
             | TData Data
               deriving (Show)

isTDecl :: TopDecl -> Bool
isTDecl (TDecl _) = True
isTDecl _         = False

data TopDecls = TopDecls { topDecls :: [Decl Expr]
                         , topDatas :: [Data]
                         } deriving (Show)

instance Monoid TopDecls where
  mempty      = TopDecls { topDecls = []
                         , topDatas = [] }
  mappend l r = mconcat [l,r]
  mconcat ts  = TopDecls { topDecls = merge topDecls
                         , topDatas = merge topDatas }
    where
    merge p = foldMap p ts

splitTopDecls :: [TopDecl] -> TopDecls
splitTopDecls  = foldMap single
  where
  single td = case td of
    TDecl d -> mempty { topDecls = [d] }
    TData d -> mempty { topDatas = [d] }

joinTopDecls :: TopDecls -> [TopDecl]
joinTopDecls tds = concat [ map TData (topDatas tds)
                          , map TDecl (topDecls tds) ]


-- Types -----------------------------------------------------------------------

type Prop = Type

data Schema = Forall { sParams :: [TParam]
                     , sProps  :: [Prop]
                     , sType   :: Type
                     , sSource :: Source
                     } deriving (Show)

data TParam = TParam { tpName :: Name
                     } deriving (Show)

data Type = TApp Type [Type]
          | TFun Type Type
          | TVar Name
          | TCon Name
          | TTuple [Type]
          | TList Type
          | TSource Source Type
            deriving (Show)


-- Data Declarations -----------------------------------------------------------

data Data = Data { dName    :: Located Name
                 , dParams  :: [TParam]
                 , dConstrs :: [Constr]
                 , dSource  :: Source
                 } deriving (Show)

data Constr = Constr { cName   :: Located Name
                     , cParams :: [Type]
                     , cSource :: Source
                     } deriving (Show)


-- Statements ------------------------------------------------------------------

type Actions = [Stmt]

data Stmt = SBind (Located Name) Expr
            -- ^ ' p <- f e1 .. en '
          | SAssign (Located Name) Expr
            -- ^ ' p = f e1 .. en '
          | SSeq Expr
            -- ^ ' f e1 .. en '
          | SLet [Decl Expr]
            -- ^ ' let x = y '
          | StSource Source Stmt
            deriving (Show)


-- Expressions -----------------------------------------------------------------

data Expr = EApp Expr [Expr]
          | ELet [Decl Expr] Expr
          | ELam [Located Name] Expr
          | EVar Name
          | ECon Name
          | ETuple [Expr]
          | ELit Literal
          | EList [Expr]
          | EIf Expr Expr Expr
          | EActions Actions
          | ETransitions [Transition]
          | ECase Expr [CaseArm]

            -- temporary for translation into pattern syntax.
          | EWildPat

          | ESource Source Expr
          | ETopology Topology
          | ETaskSet TaskSet
          | ESchedule Schedule
            deriving (Show)

data CaseArm = CaseArm { cPat  :: Pattern
                       , cBody :: Expr
                       } deriving (Show)

data Tag = TagAtom Atom
         | TagVar Name
           deriving (Show, Eq)

newtype Atom = Atom String
               deriving (Generic, Show, Ord, Eq)

instance Serialize Atom

data Transition = Transition { trPattern :: TPattern
                             , trBody    :: Expr
                             } deriving (Show)

data TPattern = Receive SourcePred Pattern
              | Timeout Expr
              | At Expr
              | TPSource Source TPattern
                deriving (Show)

isTimeout :: TPattern -> Bool
isTimeout (Timeout {})   = True
isTimeout (TPSource _ p) = isTimeout p
isTimeout _              = False

data SourcePred = SrcBind Name
                | SrcTaskHandle Name
                | SrcTags TagOp
                | SrcAny
                  deriving (Show, Eq)

data TagOp = TOIntersect TagOp TagOp
           | TOUnion TagOp TagOp
           | TOTag (Located Tag)
                deriving (Show, Eq)

-- Generic Comprehensions ------------------------------------------------------

-- | Comprehensions producing something of type 'stmt'.  The list of arms in the
-- comprehension are for parallel comprehensions.
data Comp stmt = Comp { compResult :: stmt
                      , compArms   :: [CompArm]
                      , compTags   :: [Located Tag]
                      , compSource :: Source
                      } deriving (Show)

type CompArm = [CompStmt]

data CompStmt = CSGen Pattern Expr
                -- ' pat <- gen '
              | CSGuard Expr
                -- ' x /= y '
              | CSSource Source CompStmt
                deriving (Show)


-- Topology --------------------------------------------------------------------

-- | A topology declaration.
data Topology = Topology { topoElements :: [TopoStmt]
                         } deriving (Show)

data TopoStmt = TSNode (Maybe (Located Name)) Node
              | TSLink Link
              | TSUsing Expr [Located Tag]
              | TSComp (Comp TopoStmt)
              | TSSource Source TopoStmt
                deriving (Show)

-- List comprehension constraints
data LCConstraint
        = LCPar   (Located LCExpr)
        | LCGuard (Located LCExpr)
        deriving (Show)

data LCExpr = LCBind (Located Pattern) Expr | LCExpr Expr
        deriving (Show)

data Link = Link { linkType            :: LinkType
                 , linkLeft, linkRight :: LinkTarget
                 , linkTags            :: [Located Tag]
                 } deriving (Show)

data LinkType = In | Out | Both
                deriving (Show)

data LinkTarget = LTName (Located Name)
                | LTTag [Located Tag]
                  deriving (Show)

data Node = Node { nSpec :: Expr
                 , nTags :: [Located Tag]
                 } deriving (Show)


-- Task Sets -------------------------------------------------------------------

data TaskSet = TaskSet { tsTasks  :: [TaskStmt]
                       } deriving (Show)

data TaskStmt = TStTask (Maybe (Located Name)) Task
              | TStComp (Comp TaskStmt)
              | TStSource Source TaskStmt
                deriving (Show)

data Task = Task { tBody        :: Expr
                 , tConstraints :: [TaskConstraint]
                 , tTags        :: [Located Tag]
                 } deriving (Show)

data TaskConstraint = After TaskConstraintSeq Name
                    | On Expr
                    | TCSource Source TaskConstraint
                      deriving (Show)

data TaskConstraintSeq = Start
                       | End
                         deriving (Show)


-- Schedules -------------------------------------------------------------------

data Schedule = Schedule { sStmts :: [SchedStmt]
                         } deriving (Show)

data SchedStmt = Using Expr Expr
                 -- ^ ' taskSetA using topologyB '
               | STag SchedStmt [Tag]
               | SExpr Expr
                 -- ^ ' sstmt @ #tag1,..,#tagn '
               | SSource Source SchedStmt
                 deriving (Show)


-- Literals --------------------------------------------------------------------

data Literal = LColon String
             | LDot String
             | LNum Integer Int -- ^ The 'Int' here is the base
             | LTime Time
             | LAtom Atom
             | LString String
             | LColonSlash String
             | LDotSlash String
             | LColonDot String
               deriving (Generic,Show,Eq,Ord)

instance Serialize Literal

data Time = Time { hour, minute, second, nanosecond :: Int }
               deriving (Generic,Show,Eq,Ord)

instance Serialize Time


-- Patterns --------------------------------------------------------------------

data Pattern = PVar Name
             | PCon Name [Pattern]
             | PLit Literal
             | PTuple [Pattern]
             | PWildcard
             | PSource Source Pattern
               deriving (Show)


-- Free Variables --------------------------------------------------------------

instance FreeVars Module where
  freeVars m = freeVars (modDecls m)



instance FreeVars TopDecl where
  freeVars td = case td of
    TDecl d -> freeVars d
    TData d -> freeVars d

instance FreeVars a => FreeVars (Decl a) where
  freeVars d = case d of
    DBind b      -> freeVars b
    DSig _       -> Set.empty
    DSource _ d' -> freeVars d'

-- This instance considers the name of the binding to be free within the RHS.
instance FreeVars a => FreeVars (Bind a) where
  freeVars b =
    freeVars (bBody b) Set.\\ boundVars (bParams b)

instance FreeVars Type where
  freeVars ty = case ty of
    TApp f xs     -> freeVars (f:xs)
    TFun a b      -> freeVars [a,b]
    TVar _        -> Set.empty
    TCon n        -> Set.singleton n
    TTuple ts     -> freeVars ts
    TList a       -> freeVars a
    TSource _ ty' -> freeVars ty'


instance FreeVars Data where
  freeVars d = freeVars (dConstrs d)

instance FreeVars Constr where
  freeVars c = freeVars (cParams c)



instance FreeVars a => FreeVars (Comp a) where
  freeVars comp =
    (freeVars (compResult comp) `Set.union` freeVars (compArms comp))
      Set.\\ boundVars (compArms comp)

instance FreeVars CompStmt where
  freeVars stmt = case stmt of
    CSGen _ gen      -> freeVars gen
    CSGuard e        -> freeVars e
    CSSource _ stmt' -> freeVars stmt'



instance FreeVars Schedule where
  freeVars s = freeVars (sStmts s)

instance FreeVars SchedStmt where
  freeVars ss = case ss of
    Using l r     -> freeVars [l,r]
    STag ss' ts   -> freeVars ss' `Set.union` freeVars ts
    SExpr e       -> freeVars e
    SSource _ ss' -> freeVars ss'



instance FreeVars Topology where
  freeVars t = freeVars (topoElements t) Set.\\ boundVars (topoElements t)

instance FreeVars TopoStmt where
  freeVars ts = case ts of
    TSNode _ n     -> freeVars n
    TSLink l       -> freeVars l
    TSUsing e tags -> freeVars e `Set.union` freeVars tags
    TSComp c       -> freeVars c
    TSSource _ ts' -> freeVars ts'

instance FreeVars Node where
  freeVars n = freeVars (nSpec n) `Set.union` freeVars (nTags n)

instance FreeVars Link where
  freeVars l = freeVars [linkLeft l, linkRight l]
                   `Set.union` freeVars (linkTags l)

instance FreeVars LinkTarget where
  freeVars lt = case lt of
    LTName ln  -> freeVars ln
    LTTag tags -> freeVars tags



instance FreeVars TaskSet where
  freeVars ts = freeVars (tsTasks ts) Set.\\ boundVars (tsTasks ts)

instance FreeVars TaskStmt where
  freeVars ts = case ts of
    TStTask _ t     -> freeVars t
    TStComp c       -> freeVars c
    TStSource _ ts' -> freeVars ts'

instance FreeVars Task where
  freeVars t = Set.unions [ freeVars (tBody t)
                          , freeVars (tConstraints t)
                          , freeVars (tTags t) ]

instance FreeVars TaskConstraint where
  freeVars tc = case tc of
    After _ n      -> Set.singleton n
    On e           -> freeVars e
    TCSource _ tc' -> freeVars tc'



instance FreeVars Stmt where
  freeVars stmt = case stmt of
    SBind _ e        -> freeVars e
    SAssign _ e      -> freeVars e
    SSeq e           -> freeVars e
    SLet ds          -> freeVars ds Set.\\ boundVars ds
    StSource _ stmt' -> freeVars stmt'

instance FreeVars Expr where
  freeVars e = case e of
    EApp f xs       -> freeVars (f:xs)
    ELet ds e'      -> (freeVars ds `Set.union` freeVars e') Set.\\ boundVars ds
    ELam ps e'      -> freeVars e' Set.\\ boundVars ps
    ETuple es       -> freeVars es
    EList es        -> freeVars es
    EIf c t f       -> freeVars [c,t,f]
    EActions as     -> freeVars as Set.\\ boundVars as
    ETransitions ts -> freeVars ts
    ECase s arms    -> freeVars (s,arms)
    ESource _ e'    -> freeVars e'
    EVar n          -> Set.singleton n
    ECon n          -> Set.singleton n
    EWildPat{}      -> Set.empty
    ELit{}          -> Set.empty
    ETopology t     -> freeVars t
    ETaskSet ts     -> freeVars ts
    ESchedule s     -> freeVars s

instance FreeVars CaseArm where
  freeVars ca = freeVars (cPat ca, cBody ca)

instance FreeVars Tag where
  freeVars tag = case tag of
    TagVar n  -> Set.singleton n
    TagAtom{} -> Set.empty

instance FreeVars Transition where
  freeVars tr = freeVars (trBody tr) Set.\\ boundVars (trPattern tr)

instance FreeVars Pattern where
  freeVars pat = case pat of
    PVar _         -> Set.empty
    PCon n ps      -> Set.insert n (freeVars ps)
    PLit _         -> Set.empty
    PTuple ps      -> freeVars ps
    PWildcard      -> Set.empty
    PSource _ pat' -> freeVars pat'



-- Name Binders ----------------------------------------------------------------

instance BoundVars Module where
  boundVars m = boundVars (modDecls m)

instance BoundVars TopDecl where
  boundVars td = case td of
    TDecl d -> boundVars d
    TData d -> boundVars d


instance BoundVars Data where
  boundVars d = Set.singleton (locThing (dName d))


instance BoundVars (Decl a) where
  boundVars d = case d of
    DBind b      -> boundVars b
    DSig s       -> boundVars s
    DSource _ d' -> boundVars d'

instance BoundVars (Bind a) where
  boundVars b = Set.singleton (locThing (bName b))

instance BoundVars Signature where
  boundVars sig = Set.fromList (map locThing (sigNames sig))

instance BoundVars Tag where
  boundVars tag = case tag of
    TagAtom{} -> Set.empty
    TagVar n  -> Set.singleton n

instance BoundVars CompStmt where
  boundVars stmt = case stmt of
    CSGen pat _      -> boundVars pat
    CSGuard _        -> Set.empty
    CSSource _ stmt' -> boundVars stmt'

instance BoundVars TaskStmt where
  boundVars ts = case ts of
    TStTask mb _    -> boundVars mb
    TStComp _       -> Set.empty
    TStSource _ ts' -> boundVars ts'

instance BoundVars TopoStmt where
  boundVars ts = case ts of
    TSNode mb _    -> boundVars mb
    TSLink _       -> Set.empty
    TSUsing _ _    -> Set.empty
    TSComp _       -> Set.empty
    TSSource _ ts' -> boundVars ts'

-- a complicated way of saying the empty set.
instance BoundVars SchedStmt where
  boundVars s = case s of
    Using _ _    -> Set.empty
    STag s' _    -> boundVars s'
    SExpr _      -> Set.empty
    SSource _ s' -> boundVars s'

instance BoundVars TPattern where
  boundVars tp = case tp of
    Receive src pat -> boundVars src `Set.union` boundVars pat
    TPSource _ tp'  -> boundVars tp'
    Timeout{}       -> Set.empty
    At{}            -> Set.empty

instance BoundVars SourcePred where
  boundVars sp = case sp of
    SrcBind n       -> Set.singleton n
    SrcTaskHandle _ -> Set.empty
    SrcTags{}       -> Set.empty
    SrcAny          -> Set.empty

instance BoundVars Pattern where
  boundVars pat = case pat of
    PCon _ pats    -> boundVars pats
    PTuple pats    -> boundVars pats
    PSource _ pat' -> boundVars pat'
    PVar n         -> Set.singleton n
    PWildcard      -> Set.empty
    PLit{}         -> Set.empty

instance BoundVars Stmt where
  boundVars stmt = case stmt of
    SBind p _        -> boundVars p
    SAssign p _      -> boundVars p
    SSeq _           -> Set.empty
    SLet ds          -> boundVars ds
    StSource _ stmt' -> boundVars stmt'


-- Pretty Printing--------------------------------------------------------------

tagsDoc :: [Tag] -> PPDoc
tagsDoc tags | null tags = empty
             | otherwise = char '@' <+> ppTags tags

ppTags :: [Tag] -> PPDoc
ppTags tags = case tags of
  [t] -> pp t
  []  -> empty
  _   -> parens (commas (map pp tags))

ppActions :: Actions -> PPDoc
ppActions stmts = hang (text "actions")
                     8 (block (map pp stmts))

instance PP Module where
  ppr m = header $$ block ds
    where
    header = text "module" <+> pp (modName m) <+> text "where"
    ds     = map pp (modImports m) ++ ppTopDecls (modDecls m)

ppTopDecls :: [TopDecl] -> [PPDoc]
ppTopDecls  = concatMap $ \ td -> case td of
                                    TDecl d -> ppDecl d
                                    TData d -> [pp d]

ppDecls :: PP a => [Decl a] -> [PPDoc]
ppDecls ds = concatMap ppDecl ds

ppDecl :: PP a => Decl a -> [PPDoc]
ppDecl d = case d of
  DBind b      -> ppBind b
  DSig s       -> [pp s]
  DSource _ d' -> ppDecl d'

ppBind :: PP a => Bind a -> [PPDoc]
ppBind b
  | Just schema <- bSig b = [ sigDoc schema, body ]
  | otherwise             = [ body ]
  where

  sigDoc schema =
    hang (pp (bName b) <+> char ':')
       2 (pp schema)

  body = hang (pp (bName b) <+> params <+> char '=')
            2 (pp (bBody b))
    where
    params = hsep (map pp (bParams b))

instance PP Import where
  ppr i = text "import" <+> pp (impModule i)

instance PP Data where
  ppr d = hang (text "data" <+> pp (dName d) <+> hsep (map pp (dParams d)))
             2 (sep (char '=' : punctuate (char '|') (map pp (dConstrs d))))

instance PP Constr where
  ppr c = pp (cName c) <+> sep (map (ppPrec 10) (cParams c))

instance PP Signature where
  ppr sig = hang (commas (map pp (sigNames sig)) <+> text ":")
                    2 (pp (sigSchema sig))

instance PP Schema where
  ppr s = vars <+> props <+> pp (sType s)
    where
    params = hsep (map pp (sParams s))
    vars | null (sParams s) = empty
         | otherwise        = text "forall" <+> params <> char ','
    props | null (sProps s) = empty
          | otherwise       = parens (commas (map pp (sProps s))) <+> text "=>"

instance PP TParam where
  ppr tp = pp (tpName tp)

instance PP Type where
  ppr ty = case ty of

    TApp f xs -> precParens 10
               $ ppPrec 10 f <+> hsep (map (ppPrec 10) xs)

    TFun l r -> precParens 9
              $ sep [ppPrec 9 l, text "->" <+> ppPrec 0 r]

    TVar v -> pp v

    TCon c -> pp c

    TTuple ts -> parens (commas (map pp ts))

    TList t -> brackets (pp t)

    TSource _ ty' -> ppr ty'

instance PP Transition where
  ppr t = hang (pp (trPattern t) <+> text "->")
                  2 (pp (trBody t))

instance PP Stmt where
  ppr s = case s of
    SBind pat e -> hang (pp pat <+> text "<-")
                      2 (pp e)

    SAssign pat e -> hang (pp pat <+> char '=')
                        2 (pp e)

    SSeq e -> pp e

    SLet ds -> hang (text "let")
                  4 (block (ppDecls ds))

    StSource _ s' -> ppr s'

instance PP Expr where
  ppr e = case e of
    EApp f xs    -> precParens 10
                  $ hang (ppPrec 0 f)
                       2 (sep (map (ppPrec 10) xs))

    ELet ds e'   -> precParens 10
                  $ hang (text "let")
                       4 (block (ppDecls ds))
                 $$ nest 1 (hang (text "in") 3 (pp e'))

    ELam ps e' -> precParens 10
                $ hang (char '\\' <+> hcat (map (ppPrec 10) ps) <+> text "->")
                     2 (pp e')

    ELit lit     -> pp lit

    EVar v       -> pp v
    ECon s       -> pp s
    EIf c t f    -> hang (text "if" <+> pp c)
                       2 (vcat [ hang (text "then") 2 (pp t)
                               , hang (text "else") 2 (pp f)
                               ])
    ETuple ts    -> parens (commas (map pp ts))
    EActions as  -> ppActions as

    EList es     -> brackets (commas (map pp es))

    ETransitions ts -> hang (text "transitions")
                          2 (block (map pp ts))

    ECase s arms -> hang (text "case" <+> pp s <+> text "of")
                       2 (vcat (map pp arms))

    EWildPat -> text "_"

    ESource _ e' -> ppr e'
    ETopology t  -> pp t
    ETaskSet t   -> pp t
    ESchedule s  -> pp s

instance PP CaseArm where
  ppr ca = hang (pp (cPat ca) <+> text "->")
              2 (pp (cBody ca))

instance PP Literal where
  ppr lit = case lit of
    LColon s      -> text s
    LColonSlash s -> text s
    LDotSlash s   -> text s
    LTime (Time hh mm ss ns) -> hcat [ppr hh, char ':', ppr mm, char ':', ppr ss, char '.', ppNanoseconds ns]
    LColonDot s   -> text s
    LDot s        -> text s
    LNum n 2      -> text "0b" <+> text (showIntAtBase 2 intToDigit n "")
    LNum n 8      -> text "0o" <+> text (showIntAtBase 8 intToDigit n "")
    LNum n 16     -> text "0x" <+> text (showIntAtBase 16 intToDigit n "")
    LNum n _      -> text (show n)
    LString s     -> text (show s)
    LAtom a       -> pp a
   where
   ppNanoseconds = text . reverse . take 9 . (++ repeat '0') . reverse . show


instance PP Atom where
  ppr (Atom str) = char '#' <> text str

instance PP Tag where
  ppr t = case t of
    TagAtom a -> pp a
    TagVar n  -> pp n

instance PP Pattern where
  ppr pat = case pat of
    PVar n         -> pp n
    PCon n ps      -> precParens 10 (pp n <+> hsep (map (ppPrec 10) ps))
    PLit l         -> pp l
    PTuple ps      -> parens (commas (map pp ps))
    PWildcard      -> char '_'
    PSource _ pat' -> ppr pat'

instance PP TPattern where
  ppr tp = case tp of
    Receive src pat -> ppr src <+> char '?' <+> pp pat

    Timeout expr -> text "timeout" <+> pp expr
    At      expr -> text "at"      <+> pp expr

    TPSource _ tp' -> ppr tp'

instance PP SourcePred where
  ppr sp = case sp of
    SrcTaskHandle n -> pp n
    SrcBind n       -> text "match" <+> pp n
    SrcTags ts      -> text "tagged" <+> pp ts
    SrcAny          -> text "_"

instance PP TagOp where
    ppr to = case to of
        TOIntersect a b -> parens $ pp a <+> text "&&" <+> pp b
        TOUnion     a b -> parens $ pp a <+> text "||" <+> pp b
        TOTag         t -> pp t

instance PP a => PP (Comp a) where
  ppr c = brackets body <+> tagsDoc (map locThing (compTags c))
    where
    body = hang (pp (compResult c))
              2 (vcat (map ppArm (compArms c)))
    ppArm stmts = char '|' <+> commas (map pp stmts)

instance PP CompStmt where
  ppr cs = case cs of
    CSGen pat e    -> pp pat <+> text "<-" <+> pp e
    CSGuard e      -> pp e
    CSSource _ cs' -> ppr cs'


instance PP Topology where
  ppr t = hang (text "topology")
             2 (block (map pp (topoElements t)))

instance PP TopoStmt where
  ppr ts = case ts of
    TSNode mb n    -> maybe empty (\ln -> pp ln <+> char '=') mb <+> pp n
    TSLink l       -> pp l
    TSUsing e tags -> text "using" <+> pp e <+> tagsDoc (map locThing tags)
    TSComp c       -> pp c
    TSSource _ ts' -> ppr ts'


instance PP LCConstraint where
  ppr (LCPar   hd) = hsep [char '|' , pp hd]
  ppr (LCGuard hd) = hsep [char ',' , pp hd]

instance PP LCExpr where
  ppr (LCBind pat e) = hsep $ pp pat : text "<-" : pp e : []
  ppr (LCExpr e) = pp e

instance PP TaskSet where
  ppr ts = hang (text "taskSet")
                   2 (block (map pp (tsTasks ts)))

instance PP TaskStmt where
  ppr ts = case ts of
    TStTask mb t    -> maybe empty (\ln -> pp ln <+> char '=') mb <+> pp t
    TStComp c       -> pp c
    TStSource _ ts' -> ppr ts'

instance PP Node where
  ppr n = hang (text "node" <+> ppPrec 10 (nSpec n))
             2 (tagsDoc (map locThing (nTags n)))


instance PP Link where
  ppr l = sep [ pp (linkLeft l) <+> pp (linkType l) <+> pp (linkRight l)
              , tagsDoc (map locThing (linkTags l)) ]


instance PP LinkTarget where
  ppr (LTTag as) = parens (commas (map pp as))
  ppr (LTName l) = pp l

instance PP LinkType where
  ppr lt = text $ case lt of
    In   -> "<-"
    Out  ->  "->"
    Both -> "<->"

instance PP Task where
  ppr t =
    hang (text "task" <+> ppPrec 10 (tBody t))
       2 (vcat (map pp (tConstraints t)) <+> tagsDoc (map locThing (tTags t)))

instance PP TaskConstraint where
  ppr tc = case tc of

    After s n      -> text "after" <+> ppr s <+> pp n
    On s           -> text "on" <+> ppPrec 10 s
    TCSource _ tc' -> ppr tc'

instance PP TaskConstraintSeq where
  ppr tc = case tc of
    Start -> text "start"
    End   -> text "end"

instance PP Schedule where
  ppr s = precParens 10
        $ hang (text "schedule")
             2 (block (map pp (sStmts s)))

instance PP SchedStmt where
  ppr s = case s of

    Using tasks topo -> sep [ ppPrec 10 tasks
                            , text "using"
                            , ppPrec 10 topo ]

    SExpr e      -> pp e
    STag s' tags -> pp s' <+> tagsDoc tags

    SSource _ s' -> ppr s'


-- Utility Instances -----------------------------------------------------------

instance HasSource (Decl a) where
  getSource d = case d of
    DSource s _ -> s
    _           -> Unknown

instance HasSource SourcePred where
  getSource _ = Unknown

instance HasSource Data where
  getSource = dSource

instance HasSource Constr where
  getSource = cSource

instance HasSource Schema where
  getSource = sSource

instance HasSource Type where
  getSource t = case t of
    TSource s _ -> s
    _           -> Unknown

instance HasSource Expr where
  getSource e = case e of
    ESource s _ -> s
    _           -> Unknown

instance HasSource CaseArm where
  getSource ca = mappend (getSource (cPat ca)) (getSource (cBody ca))

instance HasSource (Comp a) where
  getSource = compSource

instance HasSource Pattern where
  getSource pat = case pat of
    PSource s _ -> s
    _           -> Unknown

instance HasSource TopoStmt where
  getSource ts = case ts of
    TSSource s _ -> s
    _            -> Unknown

instance HasSource TaskStmt where
  getSource ts = case ts of
    TStSource s _ -> s
    _             -> Unknown


instance HasSource TaskConstraint where
  getSource tc = case tc of
    TCSource s _ -> s
    _            -> Unknown

instance HasSource SchedStmt where
  getSource ss = case ss of
    SSource s _ -> s
    _           -> Unknown

instance HasSource Stmt where
  getSource st = case st of
    StSource s _ -> s
    _            -> Unknown

instance HasSource Transition where
  getSource tr = mappend (getSource (trPattern tr)) (getSource (trBody tr))

instance HasSource TPattern where
  getSource tp = case tp of
    TPSource s _ -> s
    _            -> Unknown

instance HasSource LinkTarget where
  getSource lt = case lt of
    LTName ln -> getSource ln
    LTTag ts  -> getSource ts


-- Location Stripping ----------------------------------------------------------

instance NoSource TopDecl where
  noSource td = case td of
    TDecl d -> TDecl (noSource d)
    TData d -> TData (noSource d)

instance NoSource Data where
  noSource d = Data { dName    = noSource (dName d)
                    , dParams  = dParams d
                    , dConstrs = map noSource (dConstrs d)
                    , dSource  = noSource (dSource d) }

instance NoSource Constr where
  noSource c = Constr { cName   = noSource (cName c)
                      , cParams = map noSource (cParams c)
                      , cSource = noSource (cSource c) }

instance NoSource a => NoSource (Decl a) where
  noSource (DSource _ s) = noSource s
  noSource (DSig s) = DSig (noSource s)
  noSource (DBind b) = DBind (noSource b)

instance NoSource Signature where
  noSource s = Signature (sigNames s) (noSource (sigSchema s))

instance NoSource a => NoSource (Bind a) where
  noSource b =
    Bind (bName b) (fmap noSource (bSig b))
    (map noSource (bParams b)) (noSource (bBody b))

instance NoSource Schema where
  noSource s = Forall { sParams = (map noSource (sParams s)),
                        sProps = map noSource (sProps s),
                        sType = noSource (sType s),
                        sSource = Unknown
                      }

instance NoSource TParam where
  noSource x = x

instance NoSource Type where
  noSource (TSource _ ty) = noSource ty
  noSource (TApp tc args) = TApp (noSource tc) (map noSource args)
  noSource (TFun d r) = TFun (noSource d) (noSource r)
  noSource (TTuple tys) = TTuple (map noSource tys)
  noSource (TList ty) = TList (noSource ty)
  noSource x = x

instance NoSource Pattern where
  noSource (PSource _ p) = noSource p
  noSource (PCon n ps) = PCon n (map noSource ps)
  noSource (PTuple ps) = PTuple (map noSource ps)
  noSource x = x

instance NoSource Module where
  noSource m = Module { modName = noSource (modName m),
                        modImports = noSource (modImports m),
                        modDecls = map noSource (modDecls m)
                      }
instance NoSource Import where
  noSource i = i { impSource = Unknown }


instance NoSource TaskSet where
  noSource ts = TaskSet { tsTasks = map noSource (tsTasks ts) }

instance NoSource Topology where
  noSource t = Topology { topoElements = map noSource (topoElements t) }

instance NoSource TaskStmt where
  noSource stmt =
    case stmt of
      TStTask mb t      -> TStTask (noSource mb) (noSource t)
      TStComp c         -> TStComp (noSource c)
      TStSource _ stmt' -> noSource stmt'


instance NoSource Task where
  noSource t =
    Task { tBody = noSource (tBody t)
         , tConstraints = map noSource (tConstraints t)
         , tTags = map noSource (tTags t)
         }

instance NoSource TopoStmt where
  noSource ts =
    case ts of
      TSNode mb n    -> TSNode  (noSource mb) (noSource n)
      TSLink l       -> TSLink  (noSource l)
      TSUsing e tags -> TSUsing (noSource e) (noSource tags)
      TSComp c       -> TSComp  (noSource c)
      TSSource _ ts' -> noSource ts'

instance NoSource Node where
  noSource n = Node { nSpec = noSource (nSpec n)
                    , nTags = noSource (nTags n)
                    }

instance NoSource Link where
  noSource l = Link { linkType  = linkType l
                    , linkLeft  = noSource (linkLeft  l)
                    , linkRight = noSource (linkRight l)
                    , linkTags  = noSource (linkTags  l) }

instance NoSource LinkTarget where
  noSource lt = case lt of
    LTName ln -> LTName (noSource ln)
    LTTag ts  -> LTTag  (noSource ts)


instance NoSource TaskConstraint where
  noSource (TCSource _ tc) = noSource tc
  noSource (On e) = On $ noSource e
  noSource (After s name) = After (noSource s) name

instance NoSource Expr where
  noSource (ESource _ e) = noSource e
  noSource (EApp f args) = EApp (noSource f) (map noSource args)
  noSource (ELet decls body) = ELet (map noSource decls) (noSource body)
  noSource (ELam pats body) = ELam (map noSource pats) (noSource body)
  noSource (EVar x) = EVar x
  noSource (ECon c) = ECon c
  noSource (ETuple es) = ETuple (map noSource es)
  noSource (ELit l) = ELit (noSource l)
  noSource (EList l) = EList (map noSource l)
  noSource (EIf p t e) = EIf (noSource p) (noSource t) (noSource e)
  noSource (EActions as) = EActions (map noSource as)
  noSource (ECase s arms) = ECase (noSource s) (map noSource arms)
  noSource EWildPat = EWildPat
  noSource (ETransitions ts) = ETransitions (map noSource ts)
  noSource (ETopology t)     = ETopology (noSource t)
  noSource (ETaskSet ts)     = ETaskSet (noSource ts)
  noSource (ESchedule s)     = ESchedule s

instance NoSource CaseArm where
  noSource ca = CaseArm { cPat  = noSource (cPat ca)
                        , cBody = noSource (cBody ca) }

instance NoSource a => NoSource (Comp a) where
  noSource c =
    Comp { compResult = noSource (compResult c)
         , compArms   = noSource (compArms c)
         , compTags   = noSource (compTags c)
         , compSource = noSource (compSource c)
         }

instance NoSource CompStmt where
  noSource stmt =
    case stmt of
      CSGen pat e      -> CSGen (noSource pat) (noSource e)
      CSGuard e        -> CSGuard (noSource e)
      CSSource _ stmt' -> noSource stmt'

instance NoSource Atom where
  noSource x = x

instance NoSource TaskConstraintSeq where
  noSource x = x

instance NoSource Literal where
  noSource x = x

instance NoSource Stmt where
  noSource (StSource _ s) = noSource s
  noSource (SBind p e)    = SBind (noSource p) (noSource e)
  noSource (SAssign p e)  = SBind (noSource p) (noSource e)
  noSource (SSeq e)       = SSeq (noSource e)
  noSource (SLet decls)   = SLet (map noSource decls)

instance NoSource Transition where
  noSource t = Transition { trPattern = noSource (trPattern t)
                          , trBody = noSource (trBody t)
                          }

instance NoSource TPattern where
  noSource (TPSource _ p) = noSource p
  noSource (Receive e p) = Receive e (noSource p)
  noSource x = x
