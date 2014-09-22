module Mistral.CodeGen.ResolveTags where

import Mistral.Driver
import Mistral.ModuleSystem.Name ( mangleName )
import Mistral.TypeCheck.AST
import Mistral.Utils.SCC ( Group )
import Mistral.Utils.PP

import           Control.Applicative ( (<$>), (<*>), pure )
import           Data.Foldable ( foldMap, foldl' )
import qualified Data.Map as Map
import           Data.Monoid ( Monoid(..) )
import qualified Data.Set as Set
import qualified Data.Traversable as T


resolveTags :: Program -> Driver Program
resolveTags prog =
  do let tags = progTagMap prog
     traceMsg (text "tag map:" $$ text (show tags))
     nodes' <- mapM (resolveNamed resolveNode tags) (progNodes prog)
     binds' <- mapM (resolveGroup resolveDecl tags) (progBinds prog)
     return prog { progNodes = nodes'
                 , progBinds = binds' }



-- | Sets of tags, and the thing that they map to.
data TagMap = TagMap { getTopoTagMap :: AtomMap
                     , getTaskTagMap :: AtomMap
                     } deriving (Show)

type AtomMap = Map.Map Atom (Set.Set Name)

topoTags, taskTags :: AtomMap -> TagMap
topoTags tags = mempty { getTopoTagMap = tags }
taskTags tags = mempty { getTaskTagMap = tags }

hasTags :: Name -> Set.Set Atom -> AtomMap
hasTags n tags =
    let s = Set.singleton n
    in foldl' (\m t -> Map.insert t s m) Map.empty tags

instance Monoid TagMap where
  mempty        = TagMap Map.empty Map.empty
  mappend (TagMap a b) (TagMap c d) =
      let su = Map.unionWith Set.union
      in TagMap (su a c) (su b d)
  mconcat ms    =
      let su = Map.unionsWith Set.union
      in TagMap (su (map getTopoTagMap ms)) (su (map getTaskTagMap ms))

type TagSet = [(Name,Set.Set Atom)]

resolveName :: Set.Set Atom -> AtomMap -> Maybe Name
resolveName tags names =
    case validList of
        [x]   -> Just x
        (_:_) -> Nothing -- error "Explicit fail on ambiguous tags pending further discussion"
        _     -> Nothing
 where
  validList = Set.toList validSet
  validSet  = intersections (map lk (Set.toList tags))

  intersections []     = mempty
  intersections (x:xs) = foldl' Set.intersection x xs

  lk t = maybe mempty id (Map.lookup t names)


-- | Construct the static task-tag map for a program.
progTagMap :: Program -> TagMap
progTagMap prog = foldMap nodeTagMap (progNodes prog)

nodeTagMap :: Named Node -> TagMap
nodeTagMap (Named name node) =
  mconcat $ topoTags (name `hasTags` foldMap exprTags (nTags node))
          : map taskTagMap (nTasks node)

taskTagMap :: Named Task -> TagMap
taskTagMap (Named name task) =
  taskTags (name `hasTags` foldMap exprTags (tTags task))

exprTags :: Expr -> Set.Set Atom
exprTags e = case getTags e of
  Just tags -> tags
  Nothing   -> mempty

resolveTaskSet :: TagMap -> Tasks -> Driver Tasks
resolveTaskSet tags tasks =
  do ts' <- mapM (resolveTask tags) (taskTasks tasks)
     return tasks { taskTasks = ts' }

resolveTask :: TagMap -> Named Task -> Driver (Named Task)
resolveTask tags o =
  do let task = nValue o
     cs' <- mapM (resolveTaskConstraint tags) (tConstraints task)
     return o { nValue = task { tConstraints = cs' } }

resolveTaskConstraint :: TagMap -> TaskConstraint -> Driver TaskConstraint
resolveTaskConstraint tags tc = case tc of
  TCOn ty e -> do n <- tagsToName (getTopoTagMap tags) e
                  return (TCOn ty (EVar n))



resolveNamed :: (TagMap -> a -> Driver a)
             -> (TagMap -> Named a -> Driver (Named a))
resolveNamed resolve tags nm = T.traverse (resolve tags) nm

resolveNode :: TagMap -> Node -> Driver Node
resolveNode tags node =
  do tasks <- mapM (resolveTask tags) (nTasks node)
     return node { nTasks = tasks }


-- NOTE: resolving tags for bindings won't change the structure of a recursive
-- group, as tags aren't used to identify functions, beyond task declarations.
resolveGroup :: (TagMap -> a -> Driver a)
             -> (TagMap -> Group a -> Driver (Group a))
resolveGroup resolve tags group = T.mapM (resolve tags) group

resolveDecl :: TagMap -> Decl -> Driver Decl
resolveDecl tags b =
  do e' <- resolveExpr tags (bBody b)
     return b { bBody = e' }

resolveExpr :: TagMap -> Expr -> Driver Expr
resolveExpr tags = go
  where
  go e = case elimEApp e of

    -- XXX we really need the evidence from the constraint solver at this point,
    -- but for now we just assume the send target is a tag expression defaulting
    -- back to what was on error.
    (send@(ETApp (EPrim PSend) _),[who,what]) ->
      do (_,_,mb) <- tryMessages (tagsToName (getTaskTagMap tags) who)
         case mb of
           Just who' -> return $ appE send [ ELit (LString (mangleName who'))
                                           , what]
           Nothing   -> return e

    -- resolve node names in a task set
    (ETaskSet tasks,args) ->
      do tasks' <- resolveTaskSet tags tasks
         args'  <- mapM go args
         return (appE (ETaskSet tasks') args')

    (f,xs)
        -- reached a single expression
      | null xs -> case f of
          ELet gs e' ty -> ELet <$> mapM (resolveGroup resolveDecl tags) gs
                                <*> go e'
                                <*> return ty

          ECase s m rty -> ECase s <$> match m <*> return rty

          EStmts ity ty as -> EStmts ity ty <$> mapM action as

          -- XXX there are likely important cases being skipped
          _ -> return e

        -- traverse the application
      | otherwise ->
        appE <$> go f <*> mapM go xs

  match :: Match pat -> Driver (Match pat)
  match m = case m of
    MCase s sty m'    -> MCase    <$> go s    <*> pure sty <*> match m'
    MRename n e ty m' -> MRename n<$> go e    <*> pure ty  <*> match m'
    MPat pat m'       -> MPat pat <$> match m'
    MSplit l r        -> MSplit   <$> match l <*> match r
    MFail             -> return m
    MExpr e           -> MExpr    <$> go e
    MGuard e m'       -> MGuard   <$> go e <*> match m'

  action a = case a of
    ABind p m ty          -> ABind p    <$> go m <*> pure ty
    AReceive r fs to wild -> AReceive r <$> T.traverse from fs <*> T.traverse timeout to
                                        <*> T.traverse go wild

  from (From src msg msgTy body) = From src msg msgTy <$> match body
  timeout (Timeout lit body)     = Timeout lit        <$> go body


-- | Resolve a set of tags to a task identifier.  Fail when one isn't uniquely
-- identified.
tagsToName :: AtomMap -> Expr -> Driver Name
tagsToName tags e =
  do spec <- case getTags e of
               Just spec -> return spec
               Nothing   -> fail "Invalid tag expression"

     case resolveName spec tags of
       Just n -> return n
       _      -> fail $ "Unable to resolve tags: " ++ show (spec,tags)



getTags :: Expr -> Maybe (Set.Set Atom)
getTags e = case e of

  EMkTuple elems | (tys,es) <- unzip elems, all (== atomCon) tys ->
    mconcat `fmap` mapM getTags es

  ELit (LAtom a) ->
    Just (Set.singleton a)

  _ -> Nothing
