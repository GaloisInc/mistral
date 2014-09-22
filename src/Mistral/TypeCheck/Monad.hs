{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mistral.TypeCheck.Monad (
    -- * Type Checking Monad
    TC()
  , runTC

    -- ** Unification Interface
  , applySubst, getSubst
  , unify

    -- ** Variables
  , freshTVar
  , freshTVarTemplate
  , freshName
  , withSkolems
  , getSkolems

    -- ** Schemas
  , instSchema, instVarType
  , freshType, freshVarType, freshVarTypeRewrite
  , generalize

    -- ** Constraints
  , Goal(..), GoalSource(..)
  , goalToAssump
  , userGoal, userGoals
  , subGoalOf
  , getGoals
  , addGoal, addGoals
  , collectGoals

    -- ** Instances
  , schemaAssumps
  , localAssumps
  , assume
  , getAssumps
  , getInsts

    -- ** Environment
  , withEnv
  , getEnv
  , lookupSchema, tryLookupSchema
  , lookupTSyn
  , srcLoc
  , withSource, withLoc

    -- ** Errors
  , tcErr
  , tcWarn
  ) where

import           Mistral.Driver
import           Mistral.ModuleSystem.Interface ( IfaceTrie )
import qualified Mistral.ModuleSystem.Name as M
import           Mistral.TypeCheck.AST
import qualified Mistral.TypeCheck.Env as E
import           Mistral.TypeCheck.Unify
import           Mistral.Utils.PP
import           Mistral.Utils.Panic
import           Mistral.Utils.Source

import Control.Applicative ( Applicative(..), Alternative )
import Control.Monad ( MonadPlus(..), unless )
import Control.Monad.Fix ( MonadFix )
import Data.List ( partition )
import Data.Monoid ( Monoid(..) )
import MonadLib ( runM, ReaderT, ask, local, StateT, get, set, BaseM(..) )
import qualified Data.Set as Set


tcPanic :: [String] -> a
tcPanic  = panic "Mistral.TypeCheck.Monad"


data RO = RO { roSource  :: Source
             , roEnv     :: E.Env
             , roSkolems :: Skolems
             }

data RW = RW { rwSubst   :: Subst
             , rwFreshTy :: Int
             , rwFreshNm :: Int
             , rwGoals   :: [Goal]
             , rwAssumps :: [Inst]
               -- ^ Local assumptions, learned from the solver.
             }

newtype TC a = TC { getTC :: ReaderT RO (StateT RW Driver) a
                  } deriving (Functor,Applicative,Monad,MonadPlus,MonadFix,Alternative)

instance BaseM TC Driver where
  {-# INLINE inBase #-}
  inBase m = TC (inBase m)

-- | Run a TC action in the Driver monad.
runTC :: IfaceTrie -> TC a -> Driver a
runTC ifaces m =
  do (a,_) <- runM (getTC m) ro rw
     return a
  where
  ro = RO { roSource  = Unknown
          , roEnv     = mempty { E.envIfaces = ifaces }
          , roSkolems = Set.empty
          }
  rw = RW { rwSubst   = mempty
          , rwFreshTy = 0
          , rwFreshNm = 0
          , rwGoals   = []
          , rwAssumps = []
          }


-- Fresh Variables -------------------------------------------------------------

-- | Generate a fresh type parameter with no specific name.
freshTVar :: TC Type
freshTVar  =
  do p <- freshTVarTemplate TParam { tpUserName = Nothing, tpIndex = 0 }
     return (TVar (TVFree p))

-- | Generate a fresh type parameter, given a template to use.
freshTVarTemplate :: TParam -> TC TParam
freshTVarTemplate tp = TC $
  do rw  <- get
     set rw { rwFreshTy = rwFreshTy rw + 1 }
     return tp { tpIndex = rwFreshTy rw }

-- | Generate a fresh name for the typechecking pass.
freshName :: TC M.Name
freshName  = TC $
  do rw <- get
     set $! rw { rwFreshNm = rwFreshNm rw + 1 }
     return (M.mkFresh M.TC (rwFreshNm rw))


-- Schemas ---------------------------------------------------------------------

-- | Instantiate a schema, given a set of types for the parameters, adding any
-- generated goals to the set to be proven.
instSchema :: [Type] -> Schema -> TC Type
instSchema tys s@(Forall ps props ty) =
  do unless (length tys == length ps) $
       do tcErr (invalidSchemaInst tys s)
          mzero

     let iSubst       = boundBinds (zip ps tys)
         (iprops,ity) = apply iSubst (props,ty)

     addGoals =<< applySubst =<< userGoals iprops
     applySubst ity

invalidSchemaInst :: [Type] -> Schema -> PPDoc
invalidSchemaInst ts s =
  hang (text "Not enough type parameters given to schema")
     2 (vcat [ text "parameters:" <+> nest 11 (commas (map pp ts))
             , text "schema:"     <+> nest 11 (pp s) ])

-- | Instantiate a schema from the environment, provided that the right number
-- of parameters are given.  In the event that the schema is instantiated
-- incorrectly, log an error and fail with mzero.
instVarType :: [Type] -> Expr -> E.VarType -> TC (Type,Expr)
instVarType tys e vt =
  do ty <- instSchema tys (E.vtSchema vt)

     let expr = case vt of
           E.Var _          -> tappE e  tys
           E.Checking _ e'  -> tappE e' tys
           E.Primitive _ e' -> tappE (EPrim e') tys

     return (ty,expr)

-- | Generate a fresh instantiation of a Schema, adding any generated goals to
-- the set to be proven.
freshType :: Schema -> TC ([TParam],Type)
freshType s =
  do ps <- mapM freshTVarTemplate (sParams s)
     ty <- instSchema [ TVar (TVFree p) | p <- ps ] s
     return (ps,ty)

-- | Instantiate a VarType, automatically adding its goals to the set of things
-- to prove.
freshVarType :: E.VarType -> TC ([TParam],Type)
freshVarType vt = freshType (E.vtSchema vt)

-- | Generate a fresh instantiation of a VarType, with an expression to use if
-- there is no body to rewrite with.
freshVarTypeRewrite :: Expr -> E.VarType -> TC ([TParam], Type, Expr)
freshVarTypeRewrite e vt =
  do ps      <- mapM freshTVarTemplate (sParams (E.vtSchema vt))
     let tys = map (TVar . TVFree) ps
     (ty,e') <- instVarType tys e vt
     return (ps,ty,e')

-- | Generalize a type WRT a set of type variables.  The result is the new
-- schema, and any goals that couldn't be generalized.
generalize :: Types body => [TParam] -> [Goal] -> Type -> body
                         -> TC (Schema,[Goal],body)
generalize ps gs ty body =
  do env <- getEnv
     let -- don't generalize anything that's in the environment
         fvs = typeVars env
         ps' = filter (`Set.notMember` fvs) ps

         -- partition out goals that mention the variables that will be
         -- generalized
     gs' <- applySubst gs
     let tvars         = Set.fromList ps'
         mentions_ps g = not (Set.null (tvars `Set.intersection` typeVars g))
         (cxt,other)   = partition mentions_ps gs'

         -- produce a set of new bound variables...
         bs       = zipWith step [0 ..] ps'
         step i p = p { tpIndex = i }

         -- ...and the types they correspond to
         tys = [ TVar (TVBound p) | p <- bs ]

         -- generate a substitution to the bound variables
         u = freeBinds (zip ps' tys)

     props <- applySubst (map gProp cxt)
     body' <- applySubst body
     return (Forall bs (apply u props) (apply u ty), other, apply u body')


-- | Extend the skolem environment locally.
withSkolems :: Skolems -> TC a -> TC a
withSkolems skolems body = TC $
  do ro <- ask
     local ro { roSkolems = skolems `Set.union` roSkolems ro } (getTC body)

getSkolems :: TC Skolems
getSkolems  = TC (roSkolems `fmap` ask)


-- Constraints -----------------------------------------------------------------

data Goal = Goal { gSource :: GoalSource
                 , gProp   :: Prop
                 } deriving (Show,Eq,Ord)

instance PP Goal where
  ppr g = hang (backquotes (pp (gProp g)))
             2 (pp (gSource g))

instance Types Goal where
  typeVars  g      = typeVars (gProp g, gSource g)
  applyBndrs b u g = g { gSource = applyBndrs b u (gSource g)
                       , gProp   = applyBndrs b u (gProp   g) }

instance HasSource Goal where
  getSource g = getSource (gSource g)


data GoalSource = GSUser Source
                  -- ^ Goals generated directly from user programs
                | GSGoal Prop GoalSource
                  -- ^ Goals generated by other goals.
                  deriving (Show,Eq,Ord)

instance PP GoalSource where
  ppr gs = vcat (map (text "from" <+>) (ppGoalSource gs))

ppGoalSource :: GoalSource -> [PPDoc]
ppGoalSource gs = case gs of
  GSUser Unknown -> []
  GSUser src     -> [pp src]
  GSGoal g gs'   -> backquotes (pp g) : ppGoalSource gs'

instance Types GoalSource where
  typeVars gs = case gs of
    GSUser _     -> Set.empty
    GSGoal g gs' -> typeVars (g,gs')

  applyBndrs b u gs = case gs of
    GSUser _     -> gs
    GSGoal g gs' -> GSGoal (applyBndrs b u g) (applyBndrs b u gs')

instance HasSource GoalSource where
  getSource gs = case gs of
    GSUser src   -> src
    GSGoal _ gs' -> getSource gs'

goalToAssump :: Goal -> Inst
goalToAssump g = Inst { iProp = mkSchema (gProp g) }

-- | Construct a goal whose origin is the current source location.
userGoal :: Prop -> TC Goal
userGoal p =
  do src <- srcLoc
     return Goal { gSource = GSUser src, gProp = p }

userGoals :: [Prop] -> TC [Goal]
userGoals  = mapM userGoal

subGoalSource :: Goal -> GoalSource
subGoalSource g = GSGoal (gProp g) (gSource g)

-- | Make p a subgoal of q.
subGoalOf :: Goal -> Goal -> Goal
subGoalOf q p = p { gSource = subGoalSource q }

addGoal :: Goal -> TC ()
addGoal g = addGoals [g]

addGoals :: [Goal] -> TC ()
addGoals gs = TC $
  do rw <- get
     set rw { rwGoals = gs ++ rwGoals rw }

getGoals :: TC [Goal]
getGoals  = applySubst =<< TC body
  where
  body =
    do rw <- get
       set rw { rwGoals = [] }
       return (rwGoals rw)

collectGoals :: TC a -> TC (a,[Goal])
collectGoals m =
  do orig <- getGoals
     a    <- m
     ags  <- getGoals
     addGoals orig
     return (a,ags)


-- Instance DB Interaction -----------------------------------------------------

-- | Extract constraints from a schema.
--
-- XXX this isn't quite right, as it adds all variables to every generated
-- assumption.  as a result, this type:
--
--   forall a b. (Eq a, Eq b) => ...
--
-- generates these assumptions
--
--   forall a b. Eq a
--   forall a b. Eq b
schemaAssumps :: Schema -> [Inst]
schemaAssumps schema = map toInst (sProps schema)
  where
  toInst prop = Inst { iProp = Forall (sParams schema) [] prop }

-- | Clean out any assumptions introduced during this computation.
localAssumps :: TC a -> TC a
localAssumps m =
  do assumps <- getAssumps
     a       <- m
     TC $ do rw' <- get
             set rw' { rwAssumps = assumps }
             return a

-- | Add an assumption to the instance db.
assume :: Inst -> TC ()
assume i = TC $
  do traceMsg (text "assuming:" <+> pp i)
     rw <- get
     set rw { rwAssumps = i : rwAssumps rw }

getAssumps :: TC [Inst]
getAssumps  = TC (rwAssumps `fmap` get)

getInsts :: TC [Inst]
getInsts  = TC $ do ro <- ask
                    return (E.getInsts (roEnv ro))


-- Unification -----------------------------------------------------------------

-- | Apply the current substitution to a type.
applySubst :: Types a => a -> TC a
applySubst a =
  do u <- getSubst
     return (apply u a)

getSubst :: TC Subst
getSubst  = TC (rwSubst `fmap` get)

-- | Unify two types, logging errors and using mzero on a failure.  The
-- left-hand side is the user-expected type, the right-hand-side is the inferred
-- type.  NOTE: the arguments are somewhat backwards from the way they work in
-- `mgu`, this is just an accident.
unify :: Type -> Type -> TC ()
unify l r =
  do skolems <- getSkolems
     u       <- getSubst
     case mgu skolems (apply u r) (apply u l) of

       Right u' -> TC $ do rw <- get
                           set $! rw { rwSubst = u' `mappend` u }

       Left err -> do tcErr (pp err)
                      mzero

-- Environment Interaction -----------------------------------------------------

-- | Run an action with an extended environment.
withEnv :: E.Env -> TC a -> TC a
withEnv env m =
  do env' <- applySubst env
     TC $ do ro <- ask
             local ro { roEnv = env' `mappend` roEnv ro } (getTC m)

getEnv :: TC E.Env
getEnv  = applySubst =<< TC (roEnv `fmap` ask)

-- | Lookup a schema in the environment.  If the schema doesn't exist in the
-- environment, this will report an error at the current location, and fail with
-- mzero.
lookupSchema :: Name -> TC E.VarType
lookupSchema qn =
  do ro <- TC ask
     case E.lookupEnv qn (roEnv ro) of
       Just res -> return res
       Nothing  -> tcPanic [ "no type for: " ++ show qn
                           , "bug in the renamer? "
                           , E.showEnv (roEnv ro) ]

tryLookupSchema :: Name -> TC (Maybe E.VarType)
tryLookupSchema qn = TC $ do ro <- ask
                             return (E.lookupEnv qn (roEnv ro))

-- | Lookup a type synonym
lookupTSyn :: Name -> TC (Maybe TySyn)
lookupTSyn qn = TC $
  do ro <- ask
     return (E.lookupTSyn qn (roEnv ro))

-- | The current source location.
srcLoc :: TC Source
srcLoc  = TC (roSource `fmap` ask)

-- | Run a TC action with source location information.
withSource :: HasSource src => src -> TC a -> TC a
withSource src m
  | real == Unknown = m
  | otherwise       = TC $
    do ro <- ask
       local ro { roSource = real } (getTC m)
  where
  real = getSource src

-- | Run a TC action with the location and value of a located thing.
withLoc :: (a -> TC b) -> Located a -> TC b
withLoc f loc = withSource (getSource loc) (f (locThing loc))


-- Error Reporting -------------------------------------------------------------

-- | Report an error at the current location.
tcErr :: PPDoc -> TC ()
tcErr msg = TC $
  do ro <- ask
     inBase (addErrAt msg (roSource ro))

-- | Report a warning at the current location.
tcWarn :: PPDoc -> TC ()
tcWarn msg = TC $
  do ro <- ask
     inBase (addWarnAt msg (roSource ro))
