{-# LANGUAGE FlexibleInstances #-}
-- | Eliminate dead code from a Mistral program.
module Mistral.CodeGen.DeadCode (
    elimDeadCode
  ) where

import Mistral.Driver
import Mistral.TypeCheck.AST
import Mistral.Utils.Names
import Mistral.Utils.PP
import Mistral.Utils.SCC

import qualified Data.Foldable as F
import           Data.List ( partition )
import           Data.Maybe ( catMaybes )
import qualified Data.Set as Set


-- Dead Code Removal -----------------------------------------------------------

-- | Remove declarations that aren't in the transitive closure of any task
-- declaration.
elimDeadCode :: Program -> Driver Program
elimDeadCode prog = phase "dc" $
  do let tasks       = concatMap (nTasks . nValue) (progNodes prog)
         (keep,dead) = prune (freeVars tasks) [ (freeVars l,l)
                                              | l <- concatMap F.toList (progBinds prog) ]

     traceMsg $ hang (text "initial task sets:")
                   2 (commas (map (pp . nName) tasks))
             $$ hang (text "removing:")
                   2 (commas (map (pp . bName) dead))
             $$ hang (text "keeping:")
                   2 (commas (map (pp . bName) keep))

     let prog' = prog { progBinds = scc keep }
     progFinal <- elimDeadLocalVars prog'

     traceMsg (pp progFinal)
     return progFinal

-- Eliminate dead local variables.
elimDeadLocalVars :: Program -> Driver Program
elimDeadLocalVars prog = return (elimUnused prog)

elimBinds :: [Action] -> [Action]
elimBinds [] = []
elimBinds (ABind Nothing   e t : rest ) = ABind Nothing (elimUnused e) t : elimBinds rest
elimBinds (ABind (Just nm) e t : rest ) =
    if nm `Set.member` freeVars rest
        then ABind (Just nm) (elimUnused e) t : elimBinds rest
        else ABind Nothing (elimUnused e) t   : elimBinds rest
elimBinds (AReceive rst fs tm df : rest) =
    AReceive rst (elimUnused fs) (elimUnused tm) (elimUnused df) : elimBinds rest

-- Let statements
class ElimUnused a where
    elimUnused :: a -> a

instance ElimUnused Program where
  elimUnused p = p { progBinds = elimUnused (progBinds p) }

instance ElimUnused Module where
  elimUnused m = m { modBinds = elimUnused (modBinds m) }

instance ElimUnused (Group Decl) where
  elimUnused = groupMap elimUnused

instance ElimUnused (Bind Expr) where
  elimUnused b = b { bBody = elimUnused (bBody b) }

instance ElimUnused a => ElimUnused [a] where
  elimUnused xs = map elimUnused xs

instance ElimUnused Action where
  elimUnused a =
    case a of
        ABind n e t       -> ABind n (elimUnused e) t
        AReceive r fs t d -> AReceive r (elimUnused fs) (fmap elimUnused t) (fmap elimUnused d)

instance ElimUnused a => ElimUnused (Maybe a) where
  elimUnused = fmap elimUnused

instance ElimUnused Expr where
  elimUnused (ELet b e t)      = elimELet b e t
  elimUnused (EApp e1 e2)      = EApp (elimUnused e1) (elimUnused e2)
  elimUnused (ECase s m t)     = ECase s (elimUnused m) t
  elimUnused (EStmts t1 t2 as) = EStmts t1 t2 (elimBinds as)
  elimUnused (ETApp e1 ts)     = ETApp (elimUnused e1) ts
  elimUnused (ECApp e es)      = ECApp (elimUnused e) (elimUnused es)
  elimUnused (EMkTuple te)     = EMkTuple (map (\(t,e) -> (t, elimUnused e)) te)
  elimUnused (EMkList t es)    = EMkList t (elimUnused es)
  elimUnused e                 = e

instance ElimUnused (Match Pattern) where
  elimUnused m =
   case m of
    MCase e t m'     -> MCase (elimUnused e) t (elimUnused m')
    MRename n e t m' -> MRename n (elimUnused e) t (elimUnused m')
    MGuard g m'      -> MGuard g (elimUnused m')
    MPat p m'        -> MPat p (elimUnused m')
    MSplit l r       -> MSplit (elimUnused l) (elimUnused r)
    MExpr e          -> MExpr (elimUnused e)
    MFail            -> MFail

instance ElimUnused Timeout where
  elimUnused t = t { toBody = elimUnused (toBody t) }

instance ElimUnused From where
  elimUnused f = f { fBody = elimUnused (fBody f) }

elimELet :: [Group Decl] -> Expr -> Type -> Expr
elimELet gd e t =
  let e'            = elimUnused e
      fv            = freeVars e'
      pruneGroup :: Group Decl -> Maybe (Group Decl)
      pruneGroup g =
          case g of
              NonRecursive r -> case fst (prune fv [(freeVars r, r)]) of
                                    [x] -> Just (NonRecursive x)
                                    _   -> Nothing
              Recursive es   -> case fst (prune fv [(freeVars b, b) | b <- es]) of
                                    [] -> Nothing
                                    xs -> Just (Recursive xs)
      keepGroups    = catMaybes [pruneGroup g | g <- gd]
  in if null keepGroups
         then e'
         else ELet keepGroups e' t

-- | Partition declarations by whether or not they are reachable from the
-- initial set given.
prune :: Set.Set Name -> [(Set.Set Name, Decl)] -> ([Decl],[Decl])
prune refs ds
  | null reachable = ([], map snd next)
  | otherwise      = (map snd reachable ++ rs, dead)
  where
  isReachable (_,d) = bName d `Set.member` refs
  (reachable,next)  = partition isReachable ds
  (rs,dead)         = prune (Set.unions (map fst reachable)) next


