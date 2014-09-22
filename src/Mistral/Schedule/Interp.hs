module Mistral.Schedule.Interp (
    modulesEnv
  , interpSchedule
  ) where

import Mistral.ModuleSystem.Name ( mkGlobal )
import Mistral.Schedule.Value
import Mistral.TypeCheck.AST
import Mistral.Utils.Panic ( panic )

import           Control.Monad ( mzero, mplus )
import qualified Data.Foldable as Fold
import           Data.List ( partition )
import           Data.Monoid ( Monoid(..) )
import qualified Data.Sequence as Seq


sPanic :: [String] -> a
sPanic  = panic "Mistral.Schedule.Interp"


-- Environment Management ------------------------------------------------------

type ROEnv = Env

-- | Generate an environment from a collection of modules.
modulesEnv :: [Module] -> Env
modulesEnv ms = finalEnv
  where
  finalEnv        = foldl addModule mempty ms
  addModule env m = moduleEnv finalEnv m `mappend` env

-- | Generate the environment for a single module, in the context of others.
moduleEnv :: ROEnv -> Module -> Env
moduleEnv ro m = mconcat [ Fold.foldMap (groupEnv (declEnv ro)) (modBinds m) ]

-- | Introduce all variables as tfun/fun values, and eval the body with the
-- read-only environment.
declEnv :: ROEnv -> Decl -> Env
declEnv ro b = bindValue (bName b) (tfun ro) mempty
  where
  tfun           = foldr mkTFun vfun (bTParams b)
  mkTFun p f env = VTFun (\ ty -> f (bindType p ty env))

  -- treat closure parameters as extra parameters
  vfun          = foldr mkFun body (bCParams b ++ bParams b)
  mkFun p f env = VFun (\ val -> f (bindParam p val env) )

  -- interpret the body with the extended environment
  body env = interpExpr env (bBody b)


-- Schedule Interpreter --------------------------------------------------------

interpSchedule :: ROEnv -> Expr -> SNetwork
interpSchedule env e = case interpExpr env e of
  VSched nets -> mconcat nets
  _           -> sPanic [ "unexpected non-schedule value" ]


-- General Interpreter ---------------------------------------------------------

-- | Interpret an expression, producing one in a normal form as a result.
interpExpr :: ROEnv -> Expr -> Value
interpExpr env e = case e of
  EVar n        -> lookupEnv n env
  EApp f x      -> interpEApp (interpExpr env f) (interpExpr env x)

  -- XXX these should be finished
  ELet{}        -> sPanic [ "ELet"  ]
  EPrim{}       -> sPanic [ "EPrim" ]

  ECon n        -> lookupEnv n env

  -- XXX these should be finished
  ECase{}       -> sPanic [ "ECase"  ]
  EStmts{}      -> sPanic [ "EStmts" ]

  ELit l        -> VLit l
  ETApp b ts    -> interpTApp (interpExpr env b) ts
  ECApp b es    -> interpCApp (interpExpr env b) (map (interpExpr env) es)
  EMkTuple fs   -> VCon tupleName [ interpExpr env f | (_,f) <- fs ]
  EMkList _ es  -> toList env es
  ETopo t       -> VTopo  (interpTopo  env t)
  ESchedule s   -> VSched (interpSched env s)
  ETaskSet t    -> VTasks (interpTasks env t)

interpEApp :: Value -> Value -> Value
interpEApp val x = case val of
  VFun f -> f x
  _      -> sPanic [ "interpEApp: expected a VFun" ]

interpTApp :: Value -> [Type] -> Value
interpTApp val ts = case ts of

  ty:rest | VTFun f <- val -> interpTApp (f ty) rest
          | otherwise      -> sPanic [ "interpTApp: expected a VTFun" ]
  []                       -> val

interpCApp :: Value -> [Value] -> Value
interpCApp val es = case es of
  e:rest | VFun f <- val -> interpCApp (f e) rest
         | otherwise     -> sPanic [ "interpCApp: expected a VFun" ]
  []                     -> val

tupleName :: Name
tupleName  = mkGlobal ["$Interp"] "()"

consName :: Name
consName  = mkGlobal ["$Interp"] ":"

nilName :: Name
nilName  = mkGlobal ["$Interp"] "[]"

toList :: Env -> [Expr] -> Value
toList env es = foldr cons nil es
  where
  cons h tl = VCon consName [ interpExpr env h, tl ]
  nil       = VCon nilName  []

-- | Interpret a schedule expression.
interpSched :: ROEnv -> Sched -> [SNetwork]
interpSched env (Sched stmts) = map (interpSchedStmt env) stmts

-- | Interpret a single schedule statement.
--
-- XXX once we come up with a way to share topology between statements, these
-- will need to be processed in bulk.
interpSchedStmt :: ROEnv -> SchedStmt -> SNetwork
interpSchedStmt env (SchedUsing tasks topo _) = schedule (ts tags) net
  where
  VTopo net = interpExpr env topo
  tags      = networkNodeTags net
  VTasks ts = interpExpr env tasks

networkNodeTags :: SNetwork -> NodeTags
networkNodeTags sn = Fold.foldMap bindNode (snNodes sn)

-- XXX handle comprehensions
-- XXX handle links
interpTopo :: ROEnv -> Topo -> SNetwork
interpTopo env topo = foldl addNode initialNetwork (topoNodes topo)
  where
  initialNetwork   = mempty { snLinks = topoLinks topo }
  addNode net node = net { snNodes = interpNode env node : snNodes net }

interpNode :: ROEnv -> Named Node -> SNode
interpNode env (Named name node) =
  SNode { snName  = name
        , snSpec  = nSpec node
        , snType  = nType node
        , snTags  = interpTags env (nTags node)
        , snTasks = [] }

-- XXX handle task comprehensions
interpTasks :: ROEnv -> Tasks -> NodeTags -> [STask]
interpTasks env tasks tags = stasks
  where
  ts       = taskTasks tasks
  ns       = map nName ts
  recEnv   = foldl (\e n -> bindValue n (getVal n) e) env ns
  assocs   = zip ns stasks
  getVal n = case lookup n assocs of
               Just v -> VTask v
               _      -> sPanic [ "invalid task environment" ]

  stasks = map (interpTask recEnv tags) (taskTasks tasks)

interpTask :: ROEnv -> NodeTags -> Named Task -> STask
interpTask env tags (Named name t) =
  STask { stName        = name
        , stTask        = t
        , stTags        = interpTags env (tTags t)
        , stConstraints = concatMap (interpTaskConstraint env tags)
                                    (tConstraints t)
        }

interpTags :: Env -> [Expr] -> [Atom]
interpTags env es = map (toAtom . interpExpr env) es
  where
  toAtom val = case val of
    VLit (LAtom a) -> a
    _              -> sPanic [ "interpTags: expected a literal atom" ]

interpTaskConstraint :: Env -> NodeTags -> TaskConstraint -> [SConstraint]
interpTaskConstraint env ntags tc = case tc of
  TCOn _ e | Just tags <- asTags env e -> map SCOn (lookupTags tags ntags)
  _                                    -> []

asTags :: Env -> Expr -> Maybe [Atom]
asTags env e = fromAtom val `mplus` fromTuple
  where
  val = interpExpr env e

  -- NOTE: this will throw away variable references, so those had better be
  -- expanded out by the time that this is used.
  fromAtom lit =
    case lit of
      VLit (LAtom a) -> return [a]
      _              -> mzero

  fromTuple =
    case interpExpr env e of
      VCon con es | con == tupleName -> do tagss <- mapM fromAtom es
                                           return (concat tagss)
      _ -> mzero


-- Scheduling ------------------------------------------------------------------

-- | Schedule a set of tasks on a network.
schedule :: [STask] -> SNetwork -> SNetwork
schedule tasks net = roundRobbin pickyNet easy
  where
  (picky,easy) = partition hasConstraints tasks
  pickyNet     = foldl addPicky net picky

addPicky :: SNetwork -> STask -> SNetwork
addPicky net task = modifyNode pref (addTask task) net
  where
  pref = target (head (stConstraints task))

-- | Take a list of tasks with no scheduling constraints, and round-robbin
-- assign them to nodes in the network.
roundRobbin :: SNetwork -> [STask] -> SNetwork
roundRobbin net tasks
  | null tasks = net
  | otherwise  = net { snNodes = Fold.toList nodes' }
  where
  nodes'            = Fold.foldl' update (Seq.fromList (snNodes net)) tasks
  update nodes task = case Seq.viewl nodes of
    node Seq.:< rest -> rest Seq.|> addTask task node
    Seq.EmptyL       -> sPanic [ "roundRobbin:empty node sequence" ]
