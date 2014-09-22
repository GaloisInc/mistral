{-# LANGUAGE MultiWayIf #-}

module Mistral.TypeCheck.Solver (
    simplifyConstraints
  ) where

import Mistral.Driver
import Mistral.TypeCheck.AST
import Mistral.TypeCheck.Monad
import Mistral.TypeCheck.Unify
import Mistral.Utils.PP
import Mistral.Utils.Panic

import           Control.Monad ( msum, mzero, guard )
import           Data.List ( nub )
import qualified Data.Set as Set

tcPanic :: [String] -> a
tcPanic  = panic "Mistral.TypeCheck.Solver"


-- | Simplify internal constraints.
simplifyConstraints :: TC ()
simplifyConstraints  = localAssumps $
  do gs <- applySubst =<< getGoals
     solve (nub gs)
  where

  solve goals =
    do goals' <- concat `fmap` mapM simplify goals
       if null goals'
          then return ()
          else solve goals'

  -- attempt to solve a single goal
  simplify g =
    do traceMsg $ hang (text "trying to solve:")
                     2 (pp g)
       (r,assumps) <- collectGoals (trySolve g)
       case r of

         -- Solved, given these assumptions.
         HasInstance ->
           do traceMsg $ if | null assumps -> empty
                            | otherwise    -> hang (text "subgoals generated:")
                                                 2 (vcat (map pp assumps))
              return (map (subGoalOf g) assumps)

         -- Unable to solve right now.  emit, and continue solving.
         Skipped ->
           do traceMsg (text "skipping goal")
              addGoal g
              return []


data Result = HasInstance
            | Skipped
              deriving (Show,Eq)

-- | Currently, we only have internal constraints, so solve them by brute-force.
trySolve :: Goal -> TC Result
trySolve g = withSource (gSource g) $
  msum [ byAssump g
       , byBuiltin g
       , return Skipped ]


-- Solving by assumption -------------------------------------------------------

-- | Try to solve by assumption
byAssump :: Goal -> TC Result
byAssump g =
  do gs <- hasInst (gProp g)
     addGoals (map (subGoalOf g) gs)
     traceMsg (text "solved by assumption")
     return HasInstance

-- | Either returns a list of new goals to add to the environment in the event
-- that the assumption gets used, or fails with mzero.
hasInst :: Prop -> TC [Goal]
hasInst prop =
  do assumps <- getAssumps
     insts   <- getInsts
     skolems <- getSkolems
     prop'   <- applySubst prop
     is      <- tryMapM (checkInst skolems prop') (assumps ++ insts)
     case is of
       [(_,gs)] -> return gs
       []       -> mzero
       _        -> tcPanic $ "overlapping instances in the instance db"
                           : pretty (text "skolems:" <+> commas (map pp (Set.toList skolems)))
                           : map showInst is
  where
  showInst (b,h) = pretty (parens (commas (map pp h)) <+> text "=>" <+> pp b)

  (pcon,_) = elimTApp prop

  -- check that the constraint is the same constraint of prop
  samePred i = pcon == p'
    where
    (p',_) = elimTApp (sType (iProp i))

  checkInst skolems prop' i =
    do guard (samePred i)
       ((_,ip),gs) <- collectGoals (freshType (iProp i))
       case match skolems ip prop' of
         Right u -> return (ip, apply u gs)
         Left  _ -> mzero


-- Solving equality constraints ------------------------------------------------

-- | Try to solve known instances and equality constraints.  Equality
-- constraints are solved by unification.
byBuiltin :: Goal -> TC Result
byBuiltin g = case gProp g of

  -- equality constraints
  TApp (TApp prop@TCon{} a) b
    | prop == eqPropCon -> solveEqProp g a b

  _ -> return Skipped


-- | Solve equality constraints by attempting a unification.
solveEqProp :: Goal -> Type -> Type -> TC Result
solveEqProp g l r =
  do skolems <- getSkolems
     l'      <- applySubst l
     r'      <- applySubst r
     case mgu skolems l' r' of
       Right _ -> return HasInstance
       Left  _ -> do tcErr $ text "Unable to solve equality constraint:"
                          $$ pp g
                     return Skipped
