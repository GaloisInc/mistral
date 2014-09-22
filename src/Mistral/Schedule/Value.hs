module Mistral.Schedule.Value (
    Env
  , groupEnv
  , lookupEnv
  , bindType
  , bindValue
  , bindParam

  , NodeTags
  , bindNode
  , lookupTag, lookupTags

  , Value(..)
  , SNetwork(..), mapWhen, modifyNode
  , SNode(..), addTask
  , STask(..), hasConstraints
  , SConstraint(..), target
  ) where

import Mistral.TypeCheck.AST
import Mistral.Utils.PP
import Mistral.Utils.Panic ( panic )
import Mistral.Utils.SCC ( Group )

import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import           Data.Monoid ( Monoid(..) )
import qualified Data.Set as Set


sPanic :: [String] -> a
sPanic  = panic "Mistral.Schedule.Value"


-- Environments ----------------------------------------------------------------

data Env = Env { envValues :: Map.Map Name Value
               , envTypes  :: Map.Map TParam Type
               }

instance Monoid Env where
  mempty       = Env { envValues = mempty, envTypes = mempty }
  mappend l r  = mconcat [l,r]
  -- merge the two environments, preferring things from the left
  mconcat envs = Env { envValues = Map.unions (map envValues envs)
                     , envTypes  = Map.unions (map envTypes  envs) }

lookupEnv :: Name -> Env -> Value
lookupEnv n env =
  case Map.lookup n (envValues env) of
    Just v  -> v
    Nothing -> sPanic [ "no value for: " ++ pretty n ]

bindType :: TParam -> Type -> Env -> Env
bindType p ty env = env { envTypes = Map.insert p ty (envTypes env) }

bindValue :: Name -> Value -> Env -> Env
bindValue n v env = env { envValues = Map.insert n v (envValues env) }

bindParam :: Param -> Value -> Env -> Env
bindParam p v env = bindValue (pName p) v env

groupEnv :: (a -> Env) -> (Group a -> Env)
groupEnv  = Fold.foldMap


-- Node Tags -------------------------------------------------------------------

newtype NodeTags = NodeTags { getNodeTags :: Map.Map Atom (Set.Set Name) }

instance Monoid NodeTags where
  mempty      = NodeTags mempty
  mappend l r = mconcat [l,r]
  mconcat nts = NodeTags (Map.unionsWith Set.union (map getNodeTags nts))

bindNode :: SNode -> NodeTags
bindNode sn = mempty { getNodeTags = foldl add mempty allTags }
  where
  name               = snName sn
  allTags            = [ (tag, Set.singleton name) | tag <- snTags sn ]
  add nodes (tag, s) = Map.insertWith Set.union tag s nodes

-- | Try to resolve a single tag to set of Node names.
lookupTag :: Atom -> NodeTags -> Set.Set Name
lookupTag tag env = case Map.lookup tag (getNodeTags env) of
  Just nodes -> nodes
  Nothing    -> Set.empty

-- | Lookup the nodes that have all of the tags given.
lookupTags :: [Atom] -> NodeTags -> [Name]
lookupTags tags env
  | null tags = [] -- XXX maybe all nodes?
  | otherwise = Set.toList (foldl1 Set.intersection (map (`lookupTag` env) tags))


-- Values ----------------------------------------------------------------------

data Value = VTFun (Type -> Value)
             -- ^ Type abstractions
           | VFun (Value -> Value)
             -- ^ Value abstractions
           | VCon Name [Value]
             -- ^ Constructor use
           | VLit Literal
             -- ^ Literals

           | VSched [SNetwork]
             -- ^ Evaluted schedules
           | VTopo SNetwork
             -- ^ Nodes and links
           | VNode SNode
             -- ^ Nodes
           | VLink Link
             -- ^ Links

           | VTasks (NodeTags -> [STask])
           | VTask STask

instance Show Value where
  show val = case val of
    VTFun _     -> "<function>"
    VFun _      -> "<type-function>"
    VCon n vs   -> "(" ++ unwords (pretty n : map show vs) ++ ")"
    VLit lit    -> "(VLit " ++ show lit ++ ")"
    VSched nets -> show nets
    VTopo net   -> show net
    VNode n     -> show n
    VLink l     -> show l
    VTasks _    -> "<tasks>"
    VTask t     -> show t

-- | Scheduling network.
data SNetwork = SNetwork { snNodes :: [SNode]
                         , snLinks :: [Link]
                         } deriving (Show)

instance Monoid SNetwork where
  mempty      = SNetwork { snNodes = []
                         , snLinks = [] }
  mappend l r = SNetwork { snNodes = Fold.foldMap snNodes [l,r]
                         , snLinks = Fold.foldMap snLinks [l,r] }

mapWhen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapWhen p f = go
  where
  go as = case as of
    a:rest | p a       -> f a :    rest
           | otherwise ->   a : go rest
    []                 -> []

-- | Modify the first occurrence of node n.
--
-- INVARIANT: This relies on the assumption that the renamer has given fresh
-- names to all nodes.
modifyNode :: Name -> (SNode -> SNode) -> (SNetwork -> SNetwork)
modifyNode n f net = net { snNodes = mapWhen nameMatches f (snNodes net) }
  where
  nameMatches sn = snName sn == n

data SNode = SNode { snName  :: Name
                   , snSpec  :: Expr
                   , snType  :: Type
                   , snTags  :: [Atom]
                   , snTasks :: [STask]
                   } deriving (Show)

addTask :: STask -> (SNode -> SNode)
addTask task sn = sn { snTasks = task : snTasks sn }

data STask = STask { stName        :: Name
                   , stTask        :: Task
                   , stTags        :: [Atom]
                   , stConstraints :: [SConstraint]
                   } deriving (Show)

hasConstraints :: STask -> Bool
hasConstraints t = not (null (stConstraints t))

data SConstraint = SCOn Name -- ^ On this node
                   deriving (Show)

-- XXX This won't work for constraints that specify relative information like:
-- "I need to be able to communicate with X"
target :: SConstraint -> Name
target (SCOn n) = n
