{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
      RecursiveDo, FlexibleContexts, ScopedTypeVariables, ViewPatterns,
      GeneralizedNewtypeDeriving, DeriveFunctor  #-}
{-# OPTIONS_GHC -w #-}

module Mistral.Semantics.SimpleInterp(interpret) where

import Prelude hiding (log)

import Mistral.TypeCheck.AST
import Mistral.Utils.SCC(Group(..), groupElems)
import Mistral.ModuleSystem.Name(Namespace,mangleName,mkLocal,mkGlobal,mkFresh,Pass(..))
import Mistral.Utils.Panic ( panic )
import Mistral.Utils.PP
import Mistral.Utils.Misc
import Mistral.Utils.Source ( Source )
import Mistral.Driver

import MonadLib

import Control.Applicative (Applicative(..))
import Data.List(intercalate)
import Data.Maybe
import Data.Bits
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Monoid ( Monoid(..) )
import System.Random(randomRIO)


----------------------------------------------------------------------
-- * Utilitiy helper functions
----------------------------------------------------------------------

-- | Panic in the interpreter
iPanic :: [String] -> a
iPanic  = panic "Mistral.Semantics.SimpleInterp"

-- | Generate some interpreter output
interpOutput :: String -> Driver ()
interpOutput str = io $ putStrLn str

-- | Generate a formatted interpreter debugging message
interpDebug :: PPDoc -> Driver ()
interpDebug = traceMsg

-- | Generate a string debugging message
interpDebugStr :: String -> Driver ()
interpDebugStr = interpDebug . text


----------------------------------------------------------------------
-- * The interpreter entrypoint and helpers
----------------------------------------------------------------------

-- | The top-level interpreter entrypoint
interpret :: Program -> Driver [(Name, Value)]
interpret prog = phase "interp" $ do
  interpDebugStr $ "Running the interpreter on " ++ pretty (progName prog)
  interpDebugStr $ "Program = " ++ show prog
  let bindings = interpProgram prog
  mapM_ (\(n,v) -> interpDebugStr (pretty n ++ ":=" ++ show v)) bindings
  mainSched <- runInterpreter bindings (progSchedule prog)
  let ss = addSchedule emptySimulationState mainSched
  interpDebugStr $ "***** Starting Simulator *****"
  _v <- simulateTaskSets bindings ss
  return bindings


----------------------------------------------------------------------
-- * Interpreter environments
----------------------------------------------------------------------

-- | Environment type
type Env = [(Name, Value)]

-- | The empty environment
emptyEnv :: Env
emptyEnv = []

-- | Append two environments
appEnv :: Env -> Env -> Env
appEnv = (++)

-- | Look up a name in the environment
lookupEnv :: Name -> IMonad Value
lookupEnv nm = do
  env <- ask
  case lookup nm env of
    Nothing -> do
      let err = "Can't get name " ++ show nm ++
                "\nEnvironment contains:\n " ++ showEnv env
      fail err
    Just v -> return v

-- | Print an 'Env' to a string
showEnv :: Env -> String
showEnv env = unlines $ map (show . fst) env

-- | Extend the environment with a ('Name','Value') pair
withEnv :: Name -> Value -> IMonad a -> IMonad a
withEnv nm v m = do
  env <- ask
  local ((nm,v):env) m

-- | Extend the environment with a list of ('Name','Value') pairs
extendEnv :: [(Name,Value)] -> IMonad a -> IMonad a
extendEnv bs m = foldr (uncurry withEnv) m bs


----------------------------------------------------------------------
-- * A monad transformer for fresh integers
----------------------------------------------------------------------

newtype FreshT m a = FreshT { unFreshT :: StateT Int m a }

instance (Monad m) => Functor (FreshT m) where
    fmap f (FreshT m) = FreshT $ fmap f m

instance (Monad m) => Applicative (FreshT m) where
    pure a = FreshT (pure a)
    FreshT s1 <*> FreshT s2  = FreshT (s1 <*> s2)

instance Monad m => Monad (FreshT m) where
    return x = FreshT $ return x
    (FreshT m) >>= f = FreshT $ m >>= unFreshT . f

instance MonadT FreshT where
  lift = FreshT . lift

class Monad m => MonadFresh m where
  fresh :: m Int
  freshName :: m Name
  freshName = do
    i <- fresh
    return $ mkFresh INT i

instance Monad m => MonadFresh (FreshT m) where
  fresh = FreshT $ do
            i <- get
            set (i+1)
            return i

instance MonadFresh m => MonadFresh (ReaderT r m) where
  fresh = lift fresh

instance MonadFresh m => MonadFresh (MistralReactT m) where
  fresh = MRStep $ fresh >>= return . return

runFreshT :: Monad m => FreshT m a -> m a
runFreshT = liftM fst . runStateT 0 . unFreshT


----------------------------------------------------------------------
-- * A Mistral-specific reactive monad transformer
----------------------------------------------------------------------

-- | A receive continuation
type RecvCont m a = Value -> Value -> Type -> m a

-- | Lifting a 'RecvCont'
liftRecvCont :: (m a -> t m a) -> RecvCont m a -> RecvCont (t m) a
liftRecvCont liftF k =
  (\ v1 v2 t -> liftF $ k v1 v2 t)

-- | A specialized form of the reactive monad for Mistral
data MistralReactT m a
  = MRDone a
  | MRStep (m (MistralReactT m a))
  | MRRcv (RecvCont (MistralReactT m) a)
  | MRSend Value Value Type (MistralReactT m a)
  | MRPrimAction Prim [Value] (Value -> MistralReactT m a) 

-- newtype IMonad a = IMonad a
instance Monad m => Monad (MistralReactT m) where
  return = MRDone
  (MRDone x) >>= f = f x
  (MRStep m) >>= f = MRStep (m >>= \k -> return (k >>= f))
  (MRRcv k) >>= f = MRRcv (\sender msg msgTy ->  k sender msg msgTy >>= f)
  (MRPrimAction p arg k) >>= f = MRPrimAction p arg (\res -> k res >>= f)
  (MRSend dest msg msgTy k) >>= f = MRSend dest msg msgTy (k >>= f)
  fail = lift . fail

instance (Monad m) => Functor (MistralReactT m) where
    fmap f m = m >>= return . f

instance (Monad m) => Applicative (MistralReactT m) where
    pure a = MRDone a
    (<*>)  = ap

instance MonadT MistralReactT where
  lift m = MRStep (m >>= return . return)

instance Show (MistralReactT m a) where
  show (MRDone _) = "MRDone"
  show (MRStep _) = "MRStep"
  show (MRRcv _) = "MRRcv"
  show (MRSend _ _ _ _) = "MRSend"
  show (MRPrimAction _ _ _) = "MRPrimAction"

-- | Running a MistralReactT computation with no non-functional
-- behavior, i.e., built entirely from 'MRStep' and 'MRDone'
runMRReactT :: Monad m => MistralReactT m a -> m a
runMRReactT (MRDone a) = return a
runMRReactT (MRStep m) = do
  mReact <- m
  runMRReactT mReact
runMRReactT m = 
  fail $ "Error: Cannot runMRReactT on something not a reader computation: " ++ show m

-- FIXME XXX (emw4): implement receive, send, and primitive as methods
-- in a MonadMReact class

-- | Receive a message
receive :: IMonad (Value,Value,Type)
receive = lift $ MRRcv $ \sender msg msgTy -> return (sender,msg,msgTy)

-- | Send a message
send :: Value -> Value -> Type -> IMonad Value
send dest msg msgTy = lift $ MRSend dest msg msgTy (return unitValue)

-- | Ask the runtime system to perform a 'Prim'
primitive :: Prim -> [Value] -> IMonad Value
primitive prim args =
  lift $ MRPrimAction prim args return


----------------------------------------------------------------------
-- * Interpreter monad
----------------------------------------------------------------------

-- | The actual monad type used by the interpreter
type IMonad = ReaderT Env (MistralReactT (FreshT Driver))

-- | Running an interpreter computation that is purely functional (no
-- receives, sends, or primitive actions) in the 'Driver' monad
runInterpreter :: Env -> IMonad a -> Driver a
runInterpreter env m =
  runFreshT $ runMRReactT $ runReaderT env m


----------------------------------------------------------------------
-- * Tag Sets and Tag Maps
----------------------------------------------------------------------

-- | A mapping from tags to sets of @a@
newtype TagMap a = TagMap { unTagMap :: M.Map Atom (S.Set a) } deriving Show

instance Ord a => Monoid (TagMap a) where
  mempty = TagMap M.empty
  mappend (TagMap m1) (TagMap m2) = TagMap $ M.unionWith (S.union) m1 m2

-- | Build up a 'M.Map' from tags to some objects, given a list of
-- | those objects and an accessor getting the tags of each
mkTagMap :: Ord a => (a -> [Atom]) -> [a] -> TagMap a
mkTagMap objTags objs =
  TagMap $
  foldr (\obj m ->
           foldr (\tag -> M.adjust (S.insert obj) tag) m (objTags obj))
  M.empty objs


----------------------------------------------------------------------
-- * Topologies
----------------------------------------------------------------------

-- | A fully-evaluated 'Topo', which is an undirected graph where each
-- vertex is named and is associated with a list of 'Atom's; edges are
-- given as pairs of names, and are also labeled with 'Atom's
data TopoValue =
    TopoValue { topoValNodeSet :: NodeSet
              , topoValLinkSet :: LinkSet
              } deriving (Show)

instance Monoid TopoValue where
  mempty = TopoValue mempty mempty
  mappend (TopoValue ns1 ls1) (TopoValue ns2 ls2) =
    (TopoValue (ns1 `mappend` ns2) (ls1 `mappend` ls2))

-- | A set of nodes, indexed by 'Name' and by tag
type NodeSet = (M.Map Name NodeValue, TagMap NodeValue)

-- | Build a 'NodeSet' from a list of 'NodeValue's
mkNodeSet :: [NodeValue] -> NodeSet
mkNodeSet nodes =
  (M.fromList (map (\node -> (nvName node, node)) nodes),
   mkTagMap nvTags nodes)

-- | Extract the 'NodeValue's from a 'NodeSet'
nodeSetNodes :: NodeSet -> [NodeValue]
nodeSetNodes (nodeMap, _) = M.elems nodeMap

-- | A set of links, indexed by the endpoints
type LinkSet = M.Map (Name,Name) LinkValue

-- | Build a 'LinkSet' from a list of 'LinkValue's
mkLinkSet :: [LinkValue] -> LinkSet
mkLinkSet links =
  M.fromList (map (\link -> ((lvLeft link, lvRight link), link)) links)

-- | Test whether two nodes are linked in a topology
linkedP :: TopoValue -> Name -> Name -> Bool
linkedP _    n1 n2 | n1 == n2 = True
linkedP topo n1 n2
  | Just _ <- M.lookup (n1, n2) (topoValLinkSet topo) = True
linkedP topo n1 n2
  | Just _ <- M.lookup (n2, n1) (topoValLinkSet topo) = True
linkedP _ _ _ = False


-- | The data stored at each vertex in a 'TopoValue'
data NodeValue = NodeValue { nvName :: Name
                           , nvTags :: [Atom]
                           } deriving (Show,Eq,Ord)

-- | The edges in a 'TopoValue'
data LinkValue = LinkValue { lvLeft, lvRight :: Name
                           , lvTags :: [Atom]
                           } deriving Show

-- | Set the name of a 'NodeValue'
setNodeValueName :: Name -> NodeValue -> NodeValue
setNodeValueName n nv = nv { nvName = n }


----------------------------------------------------------------------
-- * Tasks, TaskHandles, and TaskMaps
----------------------------------------------------------------------

-- | A fully-evaluated 'Task', which has a 'Name', a body, a list of
-- tags, and a 'TagSet' specifying which nodes it could run on
data TaskValue =
  TaskValue { tvName        :: Name
            , tvBody        :: IMonad Value
            , tvNode        :: Name
            , tvTags        :: [Atom]
            }

instance Show TaskValue where
    show task = "<task, name = " ++ show (tvName task) ++ ">"

-- XXX (emw4): is this ok?
instance Eq TaskValue where
  t1 == t2 = tvName t1 == tvName t2

instance Ord TaskValue where
  t1 <= t2 = tvName t1 <= tvName t2

-- | Task handles: a thin wrapper around 'String'.  Strings are used as the task
-- handles as we currently have no other way to refer to the name of a task in
-- the core AST.  If this changes, make sure to update ResolveTags (if it still
-- exists) to not emit 'ELit (LString (mangleName n))'.
newtype TaskHandle = TaskHandle String deriving (Eq,Ord)

instance PP TaskHandle where
    ppr (TaskHandle nm) = text nm

instance Show TaskHandle where
  show (TaskHandle nm) = "task " ++ nm

-- | Build a 'TaskHandle' from a 'Name'
mkTaskHandle :: Name -> TaskHandle
mkTaskHandle n = TaskHandle (mangleName n)

-- | A representation of a set of tasks as: a mapping from task names
-- | to the names of the nodes they run on; and a mapping from tags to
-- | task names
type TaskMap = (M.Map TaskHandle Name, TagMap TaskHandle)

-- | Build a 'TaskMap' from a list of tasks
mkTaskMap :: [TaskValue] -> TaskMap
mkTaskMap tasks =
  (M.fromList (map (\task -> (mkTaskHandle (tvName task), tvNode task)) tasks),
   TagMap $ M.map (S.map (mkTaskHandle . tvName)) $ unTagMap $ mkTagMap tvTags tasks)

-- | Look up the node name associated with a 'TaskHandle'
taskNodeLookup :: TaskMap -> TaskHandle -> Maybe Name
taskNodeLookup (tm, _) n = M.lookup n tm


----------------------------------------------------------------------
-- * Schedules
----------------------------------------------------------------------

-- | A fully-evaluated 'Sched'
type SchedValue = [(TopoValue, [TaskValue])]


----------------------------------------------------------------------
-- * Interpreter values
----------------------------------------------------------------------

-- | Interpreter Value Space
data Value = VLit Literal -- ^ A primitive value
           | VFun (Value -> IMonad Value) -- ^ A function
           | VCon Name [Value] -- ^ A value of an algebraic data type
           | VThunk (IMonad Value) -- ^ A suspension
           | VTaskHandle TaskHandle
           | VTopo TopoValue
           | VTasks (IMonad [TaskValue]) -- ^ tasksets are not
                                         -- interpreted until schedule
                                         -- statements
           | VNode NodeValue
           | VSched SchedValue

-- | Create a tuple
mkTuple :: [Value] -> Value
mkTuple  = VCon tupleName

-- | Return the name of the tuple data constructor
tupleName :: Name
tupleName  = mkLocal "$tuple"

nilCon, consCon :: Name
nilCon  = mkLocal "[]"
consCon = mkLocal ":"


-- | Represents types that can possibly be projected out of 'Value'
class ValueProj a where
    valueProjName :: a -> String
    valueProjMaybe :: Value -> Maybe a

instance ValueProj Value where
    valueProjName _ = "Value"
    valueProjMaybe v = Just v

instance ValueProj [Char] where
    valueProjName _ = "string"
    valueProjMaybe (VLit (LString str)) = Just str
    valueProjMaybe _ = Nothing

instance ValueProj Atom where
    valueProjName _ = "atom"
    valueProjMaybe (VLit (LAtom atom)) = Just atom
    valueProjMaybe _ = Nothing

instance ValueProj (Value -> IMonad Value) where
    valueProjName _ = "function"
    valueProjMaybe (VFun fun) = Just fun
    valueProjMaybe _ = Nothing

instance ValueProj SchedValue where
    valueProjName _ = "schedule"
    valueProjMaybe (VSched sched) = Just sched
    valueProjMaybe _ = Nothing

instance ValueProj NodeValue where
    valueProjName _ = "node"
    valueProjMaybe (VNode node) = Just node
    valueProjMaybe _ = Nothing

instance ValueProj TopoValue where
    valueProjName _ = "topology"
    valueProjMaybe (VTopo topo) = Just topo
    valueProjMaybe _ = Nothing

instance ValueProj (IMonad [TaskValue]) where
    valueProjName _ = "taskset"
    valueProjMaybe (VTasks tasks) = Just tasks
    valueProjMaybe _ = Nothing

instance ValueProj Bool where
    valueProjName _ = "Boolean"
    valueProjMaybe (VCon con args)
        | con == trueName && null args = Just True
    valueProjMaybe (VCon con args)
        | con == falseName && null args = Just False
    valueProjMaybe _ = Nothing

instance ValueProj Integer where
    valueProjName _ = "Integer"
    valueProjMaybe (VLit (LNum i _)) = Just i
    valueProjMaybe _ = Nothing

instance ValueProj Int where
    valueProjName _ = "Int"
    valueProjMaybe (VLit (LNum i _)) = Just $ fromInteger i
    valueProjMaybe _ = Nothing

instance ValueProj [Value] where
    valueProjName _ = "list"
    valueProjMaybe (VCon con []) | con == nilCon = Just []
    valueProjMaybe (VCon con [x, xs])
        | con == consCon
        , Just l <- valueProjMaybe xs
        = Just (x : l)
    valueProjMaybe _ = Nothing


-- | Interpret an expression and then project the value to a given
-- type, 'fail'ing if this is not possible
interpProj :: ValueProj a => Expr -> IMonad a
interpProj e = helper undefined e where
    helper :: ValueProj a => a -> Expr -> IMonad a
    helper undef expr =
        interp expr >>=
        projValue
          ("Expected a(n) " ++ valueProjName undef
           ++ " in expression " ++ pretty expr)

-- | Project a value to a specific type, 'fail'ing with the given
-- 'String' if this is not possible
projValue :: ValueProj a => String -> Value -> IMonad a
projValue str v = do
  v_forced <- forceThunks v
  case valueProjMaybe v_forced of
    Just a -> return a
    _ -> fail $ str ++ " (actual value = " ++ show v_forced ++ ")"

instance Show Value where
  show (VLit l) = pretty l
  show (VFun _f) = "<fun>"
  show (VCon n vals) = "(" ++ pretty n ++  "[" ++ intercalate "," (map show vals) ++ "])"
  show (VThunk _) = "<thunk>"
  show (VTaskHandle nm) = "<task: " ++ pretty nm ++ ">"
  show (VTopo _) = "<topology>"
  show (VTasks _) = "<taskSet>"
  show (VNode _) = "<node>"
  show (VSched _) = "<schedule>"

-- XXX (emw4) : fix up remaining show cases to actually display something

-- | Build a list 'Value' from a list of 'Value's
mkList :: [Value] -> Value
mkList vals =
  foldr cons nil vals
  -- FIXME: Constructor names should not be 'Local' (same with Tup
  -- above, and unit somewhere else). What 'Global' module should
  -- they be in, though?
  where cons x y = VCon consCon [x,y]
        nil      = VCon nilCon []

-- | Apply a function value to an argument
applyValue :: Value -> Value -> IMonad Value
applyValue (VFun f) v = f v -- Apply the given function
applyValue (VCon n args) v = return (VCon n (args ++ [v])) -- Accumulate
                                                           -- constructor
                                                           -- args.
applyValue _ _ = fail "applyValue of something not a function or a construction."

-- | Build a thunk value
mkThunk :: IMonad Value -> IMonad Value
mkThunk m = do
  env <- ask
  return $ VThunk $ local env m

-- | Force a thunk value
forceThunks :: Value -> IMonad Value
forceThunks (VThunk m) = m >>= forceThunks
forceThunks v = return v
-- forceThunks v = fail $  "forceThunks of something not a thunk."  ++ show v


----------------------------------------------------------------------
-- * Interpreting expressions, actions, and primitives
----------------------------------------------------------------------

-- | Interpret an expression
interp :: Expr -> IMonad Value
interp (EApp e1 e2) = do
  v1 <- interp e1
  v2 <- interp e2
  applyValue v1 v2

interp (ETApp (EPrim p) ts) = interpPrim p ts
interp (EPrim p)            = interpPrim p []
interp (EVar nm) = lookupEnv nm
interp (ECon n) = return $ VCon n []

interp (ECase src m _rty) = runMatch src m

--interp (ELit l) | trace ("Interpreting literal: " ++ show l) False = undefined
interp (ELit l) = return $ VLit l
interp (ELet decls body _) = do
  i <- ask
  let i' = interpSCCs interpDecl i decls
  local (appEnv i' i) $ interp body

interp (ETApp e _) = interp e
interp (ECApp _ _) = fail "interp ECApp not handled."
interp (EMkTuple vals) = do
  vs <- mapM (interp . snd) vals
  return (mkTuple vs)
interp (EMkList _ vals) = do
    vs <- mapM interp vals
    return $ mkList vs
interp (EStmts _ _ stmts) = mkThunk $ interpActions stmts
interp (ETopo topo) = liftM VTopo $ interpTopo topo
interp (ETaskSet tasks) = return $ VTasks $ interpTasks tasks
interp (ESchedule sched) = liftM VSched $ interpSched sched


-- | Interpreting Primitives
interpPrim :: Prim -> [Type] -> IMonad Value
interpPrim PLog [] = function f
  where f msg = primitive PLog [msg]

interpPrim PSend [_who,what] =
  do env <- ask
     function (f env)
  where f env dest msg = local env $ send dest msg what

interpPrim PResetTimer [] = fail "Interpreter does not support the timer."

interpPrim PPure [_] = function (return  :: Value -> IMonad Value)

interpPrim PDone [] = return unitValue

interpPrim PGoto [] = return (VFun forceThunks)

-- XXX these don't just work on ints now
interpPrim PAdd [a] | a == intCon = binIntOp (+)
interpPrim PSub [a] | a == intCon = binIntOp (-)
interpPrim PNeg [a] | a == intCon = unIntOp negate
interpPrim PMul [a] | a == intCon = binIntOp (*)
interpPrim PDiv [a] | a == intCon = binIntOp div
interpPrim PMod [a] | a == intCon = binIntOp mod
interpPrim PEq  [a] | a == intCon = binIntBool (==)
interpPrim PLt  [a] | a == intCon = binIntBool (<)
interpPrim PGt  [a] | a == intCon = binIntBool (>)
interpPrim PLe  [a] | a == intCon = binIntBool (<=)
interpPrim PGe  [a] | a == intCon = binIntBool (>=)
interpPrim PLS  [a] | a == intCon = binIntOp shiftInteger
  where shiftInteger v dist = shiftL v (fromInteger dist)

interpPrim PRS  [a] | a == intCon = binIntOp shiftInteger
  where shiftInteger v dist = shiftR v (fromInteger dist)
interpPrim PAnd [a] | a == intCon = binIntOp (.&.)
interpPrim PXor [a] | a == intCon = binIntOp xor
interpPrim POr  [a] | a == intCon = binIntOp (.|.)
interpPrim PNot [a] | a == intCon = unIntOp complement

interpPrim PString [] =
  function $ \orig@(VLit x) -> 
                case x of
                    (LAtom (Atom str)) -> VLit (LString str)
                    (LString _)        -> orig
                    (LDot    str)      -> VLit (LString str)
                    _                  -> error ("PString applied to non-stringable? " ++ show orig)
interpPrim PStr2Atom [] =
  function $ \(VLit (LString str)) -> VLit (LAtom (Atom str))

interpPrim p ts = iPanic [ "Invalid primop", show p ++ show ts ]

-- | Interpreting Actions
interpActions :: [Action] -> IMonad Value
interpActions [] = fail "interpreting empty set of actions"
interpActions ((ABind var e _ty):as) = do
    thk <- interp e
    val <- forceThunks thk
    k var val
  where k _var val | null as = return val
        k Nothing _val = interpActions as
        k (Just nm) val = extendEnv [(nm, val)] (interpActions as)

-- immediate timeouts override all other receive branches
interpActions [ AReceive _ _ (Just to) _ ]
  | timeoutZero to =
    do thk <- interp (toBody to)
       forceThunks thk

interpActions as@[ AReceive _ froms _mbTimeout mbDef ] =
  do (sender,msg,msgTy) <- receive
     let def = case mbDef of
                 Just e  -> interp e
                 Nothing -> interpActions as
     interpFroms def sender msg msgTy froms

interpActions _ = iPanic [ "statements following a receive" ]

-- | Interpret the pattern-matching of a message receive block
interpFroms :: IMonad Value -> Value -> Value -> Type -> [From] -> IMonad Value
interpFroms end sender msg msgTy = foldr step end
  where
  step from def = interpFrom def sender msg msgTy from

-- | Interpret the pattern-matching of a single message receive form
interpFrom :: IMonad Value -> Value -> Value -> Type -> From -> IMonad Value
interpFrom def sender msg msgTy from
  | msgTy /= fMessageType from = def
  | otherwise                  =
    do env <- ask
       local (appEnv [ (fSource from, sender), (fMessage from, msg) ] env)
             (interpCase def (fBody from))

-- | Interpret an expression pattern-match
runMatch :: Source -> Match Pattern -> IMonad Value
runMatch src = interpCase patMatchFailure
  where
  patMatchFailure = fail (pretty (pp src <> text "Pattern matching failed"))

-- | Top-level case, or acceptable substitutions.
interpCase :: IMonad Value -> Match Pattern -> IMonad Value
interpCase def m = case m of

  MCase e _ety m' ->
    do s <- forceThunks =<< interp e
       case break isDefault (elimMSplits m') of

         -- default case was present, interpret that as the new default arm
         (arms,d:_) -> interpArms (interpCase def d) s arms

         -- no default case present, fall back on the existing one
         (arms,_)   -> interpArms def s arms

  MRename n e _ety m' ->
    do env <- ask
       v   <- forceThunks =<< interp e
       local ((n,v):env) (interpCase def m')

  MExpr e -> forceThunks =<< interp e

  MFail -> def

  _ -> iPanic [ "interpCase: Invalid match", show m ]

-- | Interpret a list of clauses of an expression pattern-match
interpArms :: IMonad Value -> Value -> [Match Pattern] -> IMonad Value
interpArms def arg arms = foldr step def arms
  where
  step arm end = interpMatch end arg arm

-- | Interpret a single pattern-matching clause
interpMatch :: IMonad Value -> Value -> Match Pattern -> IMonad Value
interpMatch def val m = case m of

  MPat pat m' -> case (pat,val) of

    -- constructors match the constructor name, then augment the environment
    (PCon n ns, VCon n' vs) | n == n' ->
      bindVals ns vs (interpCase def m')

    -- unpack all of the tuple elements into the environment
    (PTuple ns, VCon con vs) | con == tupleName ->
      bindVals ns vs (interpCase def m')


    -- literals just match exactly
    (PLit l, VLit l') | l == l' ->
      interpCase def m'

    _ -> def

  -- all other cases are handled in interpCase
  _ -> interpCase def m

-- | Bind a list of optional parameters to a list of values in
-- computation
bindVals :: [Maybe Param] -> [Value] -> IMonad a -> IMonad a
bindVals ns vs body =
  do env <- ask
     local (appEnv valsEnv env) body
  where
  bindVal mb v = do p <- mb
                    return (pName p, v)
  valsEnv      = catMaybes (zipWith bindVal ns vs)

-- XXX FIXME HERE (emw4) : use patternMatch in the above

-- | Match a 'Pattern' against a 'Value': if the match succeeds,
-- return an 'Env' mapping of pattern variables in the pattern to
-- 'Value's; otherwise, if the match fails, return 'Nothing'
patternMatch :: Pattern -> Value -> Maybe Env
patternMatch (PCon con ns) (VCon con' args) | con == con' =
    Just $ concat $
    zipWith (\param_m arg -> case param_m of
                               Just param -> [(pName param, arg)]
                               _ -> []) ns args
patternMatch (PTuple ns) (VCon con args) | con == tupleName =
    Just $ concat $
    zipWith (\param_m arg -> case param_m of
                               Just param -> [(pName param, arg)]
                               _ -> []) ns args
patternMatch (PLit lit) (VLit lit') | lit == lit' =
    Just []
patternMatch _ _ = Nothing


----------------------------------------------------------------------
-- * "Interpretations" of declarations...
----------------------------------------------------------------------

-- | Given a (core) module, return an environment mapping each top
-- level definition to its denotation.
interpProgram :: Program -> Env
interpProgram prog = finalEnv
  where
  dataEnv  = interpSCCs interpData emptyEnv (progDatas prog)
  finalEnv = interpSCCs interpDecl dataEnv  (progBinds prog)

progSchedule :: Program -> IMonad SchedValue
progSchedule prog =
  do topo  <- progTopo prog
     tasks <- progTasks prog
     return [(topo,tasks)]

progTopo :: Program -> IMonad TopoValue
progTopo prog =
  interpTopo mempty { topoNodes = progNodes prog
                    , topoLinks = progLinks prog }

progTasks :: Program -> IMonad [TaskValue]
progTasks prog =
  sequence [ progTask n task | Named n node <- progNodes prog
                             , task         <- nTasks node ]

progTask :: Name -> Named Task -> IMonad TaskValue
progTask node (Named name t) =
  do tags <- mapM interpProj (tTags t)
     env  <- ask
     return TaskValue { tvName = name
                      , tvBody = local env (interp (tBody t))
                      , tvNode = node
                      , tvTags = tags }


interpSCCs :: (Env -> a -> Env) -> Env -> [Group a] -> Env
interpSCCs interpElem init_env sccs = env
    where
      env = concatMap (interpElem env) (concatMap groupElems sccs) ++ init_env


-- | Add functions that produce each constructor to the environment.
interpData :: Env -> Data -> Env
interpData env d = map (interpConstr env) (dConstrs d)

-- | Interpret a single constructor
interpConstr :: Env -> Constr -> (Name,Value)
interpConstr _env c = (cName c, go [] (cParams c))
  where
  go args ps = case ps of
    _:rest -> VFun (\ arg -> return (go (arg:args) rest))
    []     -> VCon (cName c) (reverse args)

-- | Using the passed in environment as read-only, produce a new environment
-- that contains the denotation declaration.
interpDecl :: Env -> Bind Expr -> Env
interpDecl env d =
    [(bName d, mkFunInEnv env (bParams d) (interp (bBody d)))]

-- | Build a functional value in a given 'Env' with a list of
-- | parameters from an interpreter compuation
mkFunInEnv :: Env -> [Param] -> IMonad Value -> Value
mkFunInEnv env [] body = VThunk $ local env body
mkFunInEnv env [p] body = VFun $ \x -> local ((pName p,x):env) body
mkFunInEnv env (p : params) body =
    VFun $ \x -> return $ mkFunInEnv ((pName p,x):env) params body



----------------------------------------------------------------------
-- * Interpreting list comprehensions
----------------------------------------------------------------------

-- | A generalization of the 'zipWith' functions, where @f@ is
-- successively applied to a list of the first arguments of all the
-- lists, then the second, etc.; i.e.,
--
-- > zipWithList f [[e11, e12, .. ], [e21, e22, ..], .. ]
-- > = [f [e11, e21, ..], f [e12, e22, ..], .. ]
zipWithList :: ([a] -> b) -> [[a]] -> [b]
zipWithList _ lists | any null lists = []
zipWithList f lists =
  (f $ map head lists) : zipWithList f (map tail lists)

-- | Interpret a parallel list comprehension, where the results of
-- each parallel branch are zipped together as lists. The number of
-- lists returned is equal to the fewest number of elements returned
-- by any parallel branch, and each list has exactly the same number
-- of elements as the number of branches
interpListComp :: (a -> IMonad v) -> Comp a -> IMonad [v]
interpListComp interpA comp = do
  envss <- mapM interpArm $ compArms comp
  let envs = zipWithList (foldr appEnv emptyEnv) envss
  mapM (\env -> extendEnv env $ interpA $ compResult comp) envs
    where
      interpArm :: CompArm -> IMonad [Env]
      interpArm [] = return [emptyEnv]
      interpArm (CompGen pat e : stmts) = do
        values <- interpProj e
        let cur_envs = catMaybes $ map (patternMatch pat) values
        concatMapM (\cur_env -> do
                      rest_envs <-
                        extendEnv cur_env $ interpArm stmts
                      return $ map (cur_env `appEnv`) rest_envs)
                     cur_envs
      interpArm (CompGuard e : stmts) =
        do res <- interpProj e
           if res then interpArm stmts else return []


----------------------------------------------------------------------
-- * Interpreting schedules, topologies, and taskSets
----------------------------------------------------------------------

-- | Interpret an 'Named' given a way to interpret the value.
interpOptName :: (a -> IMonad v) -> (Name -> v -> v) -> Named a -> IMonad v
interpOptName interpA setName (Named n a) = do
  v <- interpA a
  return (setName n v)

interpNode :: Node -> IMonad NodeValue
interpNode n =
  do name <- freshName
     interpNamedNode (Named name n)

-- | Interpret a 'Node' to a 'NodeValue'
interpNamedNode :: Named Node -> IMonad NodeValue
interpNamedNode (Named name node) = do
  tags <- mapM interpProj $ nTags node
  -- XXX (emw4) : we currently ignore the nodeSpec...
  return NodeValue { nvName = name, nvTags = tags }

-- | Interpret a 'Link' to a 'LinkValue', given a 'NodeSet' of nodes
-- it could refer to
interpLink :: NodeSet -> Link -> IMonad LinkValue
interpLink _node_map link = do
  -- XXX (emw4): add tag lookups for nodes...?
  tags <- mapM interpProj $ lTags link
  return LinkValue { lvLeft = lLeft link,
                     lvRight = lRight link,
                     lvTags = tags }

-- | Bind a single named nodes into the environment
withNodeName :: NodeValue -> IMonad a -> IMonad a
withNodeName node = withEnv (nvName node) (VNode node)

-- | Bind a list of named nodes into the environment
withNodeNames :: [NodeValue] -> IMonad a -> IMonad a
withNodeNames nodes m0 =
  foldl (\m node -> withNodeName node m) m0 nodes

-- | Bind all the named nodes in a 'Topo' into the environment
withTopoNames :: TopoValue -> IMonad a -> IMonad a
withTopoNames topo = withNodeNames (nodeSetNodes (topoValNodeSet topo))

-- | Interpret a list of 'NodeValue's, binding the names of any
-- previous ones when interpreting later ones
interpNodesWithBindings :: (a -> IMonad NodeValue) -> [a] ->
                           IMonad [NodeValue]
interpNodesWithBindings _         []     = return []
interpNodesWithBindings interpA (a : as) = do
  node <- interpA a
  nodes <- withNodeName node $ interpNodesWithBindings interpA as
  return (node : nodes)

-- | Interpret a 'Topo' into a 'TopoValue'
interpTopo :: Topo -> IMonad TopoValue
interpTopo topo = do
  nodes1 <- interpNodesWithBindings
            (interpOptName interpNode setNodeValueName) $
            topoNodes topo
  nodes2 <- withNodeNames nodes1 $
            concatMapM (interpListComp interpNode) $ topoNodeGens topo
  let nodeSet = mkNodeSet (nodes1 ++ nodes2)
  links1 <- withNodeNames nodes1 $
            mapM (interpLink nodeSet) $ topoLinks topo
  links2 <- withNodeNames nodes1 $
            concatMapM (interpListComp (interpLink nodeSet)) $
            topoLinkGens topo
  return TopoValue { topoValNodeSet = nodeSet,
                     topoValLinkSet = mkLinkSet $ links1 ++ links2 }

-- | Interpret a 'Task' into a 'TaskValue'
interpTask :: Maybe Name -> Task -> IMonad TaskValue
interpTask n_maybe task = do
  env <- ask
  constraints <- mapM (\(TCOn _ e) -> interp e) $
                 tConstraints task
  -- XXX (emw4): for now, the only "constraints" allowed are a single
  -- one that gives the NodeValue; this needs to be extended for
  -- dynamic topologies
  destNode <- case constraints of
                [VNode node] -> return $ nvName node
                _ -> fail $ "Unexpected task constraints: " ++ show constraints
  tags <- mapM interpProj $ tTags task
  name <- case n_maybe of
            Just n -> return n
            _ ->  freshName
  return TaskValue { tvName = name
                   , tvBody = local env $ interp $ tBody task
                   , tvNode = destNode
                   , tvTags = tags }

-- | Interpret a list of tasks, binding the names of any
-- previous ones when interpreting later ones
interpTaskList :: [Named Task] -> IMonad [TaskValue]
interpTaskList [] = return []
interpTaskList (Named name t : ts) = do
  task <- interpTask (Just name) t
  let env' = [(name, VTaskHandle $ mkTaskHandle $ tvName task)]
  tasks <- extendEnv env' $ interpTaskList ts
  return (task : tasks)

-- | Interpret 'Tasks' into a list of 'TaskValue's
interpTasks :: Tasks -> IMonad [TaskValue]
interpTasks tasks = do
  tasks1 <- interpTaskList $ taskTasks tasks
  tasks2 <- extendEnv (map (\t -> (tvName t, VTaskHandle $ mkTaskHandle $ tvName t)) tasks1) $
            concatMapM (interpListComp $ interpTask Nothing) $ taskTaskGens tasks
  return $ tasks1 ++ tasks2


-- | Interpret a 'Sched' to a 'SchedValue'
interpSched :: Sched -> IMonad SchedValue
interpSched sched =
    mapM interpSchedStmt $ schedStmts sched

-- | Interpret an individual 'SchedStmt'
interpSchedStmt :: SchedStmt -> IMonad (TopoValue, [TaskValue])
interpSchedStmt (SchedUsing eTasks eTopo _) = do
    topo <- interpProj eTopo
    tasks <- withTopoNames topo $ join $ interpProj eTasks
    return (topo, tasks)

----------------------------------------------------------------------
-- * The simulator
----------------------------------------------------------------------

data SimulationState =
  SimState { rtr :: [(TaskHandle,IMonad Value)],
             -- ^ Ready to run queue
             msgBuffers :: M.Map TaskHandle [(TaskHandle,Value,Type)],
             msgBlocked :: M.Map TaskHandle (RecvCont IMonad Value),
             topology :: TopoValue,
             taskMap :: TaskMap
           }

emptySimulationState :: SimulationState
emptySimulationState =
  SimState { rtr = [],
             msgBuffers = M.empty,
             msgBlocked = M.empty,
             topology = mempty,
             taskMap = mempty }

addSchedule :: SimulationState -> SchedValue -> SimulationState
addSchedule simState sched =
  let (foldr mappend mempty -> newTopo, concat -> newTasks) = unzip sched in
  simState { rtr = map (\task ->
                          (mkTaskHandle (tvName task), tvBody task)) newTasks
                   ++ rtr simState
           , topology = topology simState `mappend` newTopo
           , taskMap = taskMap simState `mappend` mkTaskMap newTasks }

simulateTaskSets :: Env -> SimulationState -> Driver Value

-- XXX FIXME: This should use the record pattern binding extension,
-- whatever the language extension is to enable that...
-- XXX FIXME: This should randomly select elements of the rtr queue
-- XXX FIXME: This should use a State transformer for SimulationState
-- and a Reader transformer for Env
simulateTaskSets env ss = case rtr ss of
  [] -> do
    unless (M.null (msgBlocked ss)) $ do
       interpOutput "** Deadlocked **"
       interpOutput "Tasks blocked on receive:"
       interpOutput $ show $ M.keys (msgBlocked ss)
       interpOutput "Undelivered Messages:"
       interpOutput $ show (msgBuffers ss)
    return unitValue

  ((th,t):ts) -> do
    let log debugP str = (if debugP then interpDebugStr else interpOutput) $
                         "task " ++ pretty th ++ ": " ++ str
        logDebug = log True
        logOutput = log False
    case runReaderT env t of
      MRDone (VThunk m) ->
        simulateTaskSets env (ss { rtr = ts ++ [(th,m)] })
      MRDone _a -> do
        -- The task has completed.
        logOutput "Task done."
        simulateTaskSets env (ss { rtr = ts })
      MRStep m -> do
        -- The task is executing some 'pure' computation. We execute
        -- it, then enqueue into the RTR queue.
        t' <- runFreshT m
        simulateTaskSets env (ss { rtr = ts ++ [(th,lift t')]})

      MRRcv k -> do
        logDebug $ "Issued receive."
        case dequeue th (msgBuffers ss) of
          -- In this case, a message had previously been sent to this
          -- task, so we dequeue it and pass the sender id and the
          -- message to the continuation of the receive.
          Just ((sender,msg,msgTy), msgBuffers') -> do
            logOutput ("Message received: " ++ show msg)
            simulateTaskSets env (ss {rtr = ts ++ 
                                        [(th, lift $ k (VTaskHandle sender) msg msgTy)],
                                  msgBuffers = msgBuffers'
                                 })
          _ -> do
            logDebug $ "Receive is blocked."
            simulateTaskSets env (ss { rtr = ts,
                                       msgBlocked = M.insert th (liftRecvCont lift k) (msgBlocked ss) })

      MRSend dest msg msgTy k
        | Just srcNode <- taskNodeLookup (taskMap ss) th,
          Just destNode <- taskNodeLookup (taskMap ss) destTask,
          linkedP (topology ss) srcNode destNode -> do
        logOutput $ "Sending " ++ show msg ++ " to " ++ show destTask
        case M.lookup destTask (msgBlocked ss) of
          Nothing -> do
            logOutput "Message is being queued for later"
            let buffers' = enqueue destTask (th,msg,msgTy) (msgBuffers ss)
            let ss' = ss { msgBuffers = buffers', rtr = ts ++ [(th,lift k)] }
            simulateTaskSets env ss'
          Just rcvk -> do
            logOutput $ "Message is received by " ++ show destTask
            let blocked' = M.delete destTask (msgBlocked ss)
            let ss' = ss { msgBlocked = blocked',
                           rtr = ts ++
                                 [(destTask,rcvk (VTaskHandle th) msg msgTy),
                                  (th,lift k)]
                         }
            simulateTaskSets env ss'
        | otherwise -> do -- not linked
            logOutput $ "Send of " ++ show msg ++ " to " ++ show destTask
                        ++ " failed: nodes not linked"
            logOutput $ "Sender is on node " ++ pretty (fromJust (taskNodeLookup (taskMap ss) th))
            logOutput $ "Receiver is on node " ++  pretty (fromJust (taskNodeLookup (taskMap ss) destTask))
            logDebug  "Topology"
            logDebug $ show (topology ss)
            logDebug  "TaskMap"
            logDebug $ show (taskMap ss)
            let ss' = ss { rtr = ts ++ [(th, lift k)]  -- no error raised
                         }
            simulateTaskSets env ss'
        where destTask = destToTaskHandle dest

      MRPrimAction act args k ->  do
        case (act,args) of
          (PLog, [arg]) -> do
            logOutput $ "Logging message: " ++ show arg
            simulateTaskSets env (ss { rtr = ts ++ [(th,lift $ k unitValue)] })

          _ -> do
            iPanic [ "unhandled primitive action", pretty act ]

  where
        destToTaskHandle (VLit (LString nm)) = TaskHandle nm
        destToTaskHandle (VTaskHandle th)    = th
        destToTaskHandle h                   = iPanic [ "invalid handle:"
                                                      , show h ]


-- Utility functions

prelude :: Namespace
prelude = ["Prelude"]

unitValue :: Value
unitValue = VCon (mkGlobal prelude "()") []

trueValue :: Value
trueValue = VCon trueName []

falseValue :: Value
falseValue = VCon falseName []

-- | Embed Haskell functions to be Mistral functions
class InterpFun a where
  function :: a -> IMonad Value

instance InterpFun (IMonad Value) where
  function m = m

instance InterpFun Value where
  function m = return m

instance InterpFun [Value] where
  function m = return $ mkList m

instance InterpFun [Char] where
  function str = return $ VLit $ LString str

instance (ValueProj a, InterpFun r) => InterpFun (a -> r) where
  function f = do
      env <- ask
      return $ VFun $ \v -> do
          (a :: a) <- projValue (valueProjName (undefined :: a) ++ " expected") v
          local env $ function (f a)

-- | Build a binary integer operation as a 'Value'
binIntOp :: (Integer -> Integer -> Integer) ->
            IMonad Value
binIntOp bop = function f
  where f :: Value -> Value -> IMonad Value
        f (VLit (LNum bx base1)) (VLit (LNum by _)) = return $
          VLit $ LNum (bop bx by) base1
        f _ _ = fail "binIntOp"

-- | Build a unary integer operation as a 'Value'
unIntOp :: (Integer -> Integer) ->
           IMonad Value
unIntOp bop = function f
  where f :: Value -> IMonad Value
        f (VLit (LNum bx base)) = return $
          VLit $ LNum (bop bx) base
        f _ = fail "binIntOp"


-- | Build a binary integer comparison as a 'Value'
binIntBool :: (Integer -> Integer -> Bool) ->
              IMonad Value
binIntBool bop = function f
  where f :: Value -> Value -> IMonad Value
        f (VLit (LNum x _)) (VLit (LNum y _)) =
            return $ if bop x y then trueValue else falseValue
        f _ _ = fail "binIntBool"

-- | Dequeue a receiver from a receiver queue
dequeue :: TaskHandle
        -> M.Map TaskHandle [(TaskHandle,Value,Type)]
        -> Maybe ((TaskHandle,Value,Type), M.Map TaskHandle [(TaskHandle,Value,Type)])
dequeue nm m = 
  case M.lookup nm m of
    Just []     -> Nothing
    Just [v]    -> Just (v,M.delete nm m)
    Just (v:vs) -> Just (v,M.insert nm vs m)
    Nothing     -> Nothing

-- | Enqueue a receiver to a receiver queue
enqueue :: TaskHandle -> (TaskHandle,Value,Type)
        -> M.Map TaskHandle [(TaskHandle,Value,Type)]
        -> M.Map TaskHandle [(TaskHandle,Value,Type)]
enqueue tn val = M.insertWith (flip (++)) tn [val]


-- XXX (emw4): the below should all be unused now...

-- Network Topology Stuff
newtype Network = Network { nGraph :: M.Map Name (S.Set Name)
                          } deriving (Show)

instance Monoid Network where
  mempty      = Network { nGraph = M.empty }
  mappend l r = Network { nGraph = merge }
    where
    merge = M.unionWith S.union (nGraph l) (nGraph r)

  mconcat ns  = Network { nGraph = merge }
    where
    merge = M.unionsWith S.union (map nGraph ns)
