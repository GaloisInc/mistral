{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns  #-}

module Mistral.TypeCheck.Patterns (
    withPatEnv
  , genMatch
  , tcLit
  ) where

import           Mistral.ModuleSystem.Name
import qualified Mistral.Parser.AST as P
import           Mistral.TypeCheck.AST
import           Mistral.TypeCheck.Env
import           Mistral.TypeCheck.Monad
import           Mistral.Utils.Names
import           Mistral.Utils.Panic ( panic )
import           Mistral.Utils.Source

import           Control.Monad ( replicateM, guard )
import           Data.Foldable ( foldlM, foldrM )
import           Data.List ( partition, groupBy )
import           Data.Monoid ( mempty )
import qualified Data.Set as Set


tcPanic :: [String] -> a
tcPanic  = panic "Mistral.TypeCheck.Patterns"


-- Patterns --------------------------------------------------------------------

-- | Run a computation with the environment generated by a parsed pattern.
withPatEnv :: Type -> P.Pattern -> TC a -> TC a
withPatEnv sty pat body =
  do env <- applySubst =<< genEnv mempty (pat,sty)
     withEnv env body
  where
  genEnv env (p,pty) = case p of

    P.PVar v ->
      do return (addSchema v (mkSchema pty) env)

    P.PCon n ps ->
      do (_,nty) <- freshVarType =<< lookupSchema n

         let len = length ps
         ptys <- replicateM len freshTVar
         unify nty (foldr tarrow pty ptys)

         foldlM genEnv env (zip ps ptys)

    P.PTuple ps ->
      do ptys <- replicateM (length ps) freshTVar
         unify pty (ttuple ptys)

         foldlM genEnv env (zip ps ptys)

    P.PWildcard ->
      return env

    P.PLit lit ->
      do tcLit lit pty
         return env

    P.PSource src pat' ->
      withSource src (genEnv env (pat',pty))

-- | Checking for literals.
--
-- XXX Literals are not all monomorphic.
-- Numbers are obvious. Also, colon can be Time or IPv6.
tcLit :: Literal -> Type -> TC ()
tcLit lit ety = case lit of
  LTime _       -> unify ety timeCon
  LColonDot _   -> unify ety timeCon
  LColonSlash _ -> unify ety ipv6MaskCon
  LDotSlash _   -> unify ety ipv4MaskCon
  LColon _      -> unify ety timeCon
  LDot _        -> unify ety ipv4Con
  LNum _ _      -> unify ety intCon
  LAtom _       -> unify ety atomCon
  LString _     -> unify ety stringCon


-- Pattern Matching Compilation ------------------------------------------------

genMatch :: [(Expr,Type)] -> [(P.Pattern,Match Pattern)] -> TC (Match Pattern)
genMatch args arms =
  do m <- match args [ ([noSource pat], m) | (pat,m) <- arms ] MFail
     return (cleanupMatch m)

-- | Unroll nested patterns.
match :: [(Expr,Type)] -> [([P.Pattern],Match Pattern)] -> Match Pattern
      -> TC (Match Pattern)
match args arms err = case args of

  (s,sty):args'
    -- when matching a tuple literal against a tuple pattern, go ahead and
    -- unroll the pattern and the literal
    | EMkTuple es <- s
    , Just arms' <- mapM (unpackTuple (length es)) arms ->
      do let tupleArgs = [ (e,ty) | (ty,e) <- es ]
         match (tupleArgs ++ args') arms' err

    -- the variable rule: all leading patterns are variables, or wildcards,
    -- allowing the pattern match to be translated into a renaming.
    | Just arms' <- mapM varRule arms ->
      match args' (map (rename s sty) arms') err

    -- the constructor rule: all leading patterns are constructor patterns.
    -- group similar constructors together, preserving matching order
    | Just arms' <- mapM constructorRule arms ->
      do cons <- mapM (matchCon sty args' err) (groupCons arms')
         return (MCase s sty (foldr MSplit err cons))

    -- all leading patterns are literals.  group similar literals together,
    -- preserving matching order, generating sub-matches
    | Just arms' <- mapM litRule arms ->
      do ms <- mapM (matchLit args' err) (groupLits arms')
         return (MCase s sty (foldr MSplit err ms))

    -- gather all tuple patterns together.  as tuple patterns are irrefutable,
    -- this boils down to one case.
    | Just arms' <- mapM tupleRule arms ->
      do body <- matchTuple sty args' err arms'
         return (MCase s sty body)

    -- the mixture rule: the set of leading patterns are a combination of
    -- variable introductions, wildcards, and constructor patterns.
    | otherwise ->
      mixtureRule args arms err

  -- no arguments left, and all patterns are empty
  [] | (pats,ms) <- unzip arms
     , all null pats ->
      return (foldr1 MSplit ms)

  -- something went wrong
  _ -> tcPanic [ "pattern matching bug", show args, show arms ]

  where

  -- rename the expression being cased over in the body of the current arm.
  rename s sty (Just n, ps, m) = (ps, MRename n s sty m)
  rename _ _   (_, ps, m)      = (ps, m)

  -- attempt to unpack a pattern as a tuple match, unrolling the match into
  -- left-to-right matches.
  unpackTuple arity (ps,m) = case ps of
    P.PTuple qs:rest -> return (qs ++ rest, m)
    P.PWildcard:rest -> return (replicate arity P.PWildcard ++ rest,m)
    _                -> Nothing

  -- check to see if the variable rule will apply to this branch
  varRule (ps,m) = case ps of
    P.PVar n    : rest -> Just (Just n, rest, m)
    P.PWildcard : rest -> Just (Nothing,rest, m)
    _                  -> Nothing

  -- the pattern begins with a constructor
  constructorRule (ps,m) = case ps of
    P.PCon n qs : rest -> Just ((n,qs), (rest, m))
    _                  -> Nothing

  -- the pattern is a literal
  litRule (ps,m) = case ps of
    P.PLit lit : rest -> Just (lit, (rest,m))
    _                 -> Nothing

  tupleRule (ps,m) = case ps of
    P.PTuple qs : rest -> Just (qs, (rest,m))
    _                  -> Nothing

  -- group similar constructors together, preserving the matching order.
  groupCons as = case as of

    -- group like-patterns together
    arm@((n,qs), _) : rest ->
       let (likes,others) = partition (isCon n) rest
        in (n, length qs, map extendPats (arm:likes)) : groupCons others

    [] -> []

    where

    isCon n ((n',_),_) = n == n'

    extendPats ((_,qs),(ps,m)) = (qs ++ ps, m)

  -- group similar literals together, preserving the matching order
  groupLits as = case as of

    arm@(lit, _) : rest ->
      let (likes,others) = partition (sameLit lit) rest
       in (lit, map dropLit (arm:likes)) : groupLits others

    [] -> []

    where

    sameLit l (l',_) = l == l'

    dropLit (_,arm) = arm


-- | Turn a group of construtor patterns into a single match.
matchCon :: Type -> [(Expr,Type)] -> Match Pattern
         -> (Name,Int,[([P.Pattern], Match Pattern)]) -> TC (Match Pattern)
matchCon sty args err (n,arity,arms) =
  do -- if goals were emitted here, we would somehow have something like a GADT,
     -- or existentially quantified constraint
     ((_,nty),_) <- collectGoals (freshVarType =<< lookupSchema n)

     -- introduce fresh variables for each field of the constructor
     ns   <- replicateM arity freshName
     ntys <- replicateM arity freshTVar
     unify nty (foldr tarrow sty ntys)

     -- expand the patterns for this constructor, then add a final 
     body <- match (zip (map EVar ns) ntys ++ args) arms err

     -- create the pattern
     let ps = zipWith Param ns ntys
     return (MPat (PCon n (map Just ps)) body)

-- | Given a group of matches to the same literal, generate the branch that
-- represents that match.  For example:
--
--  case ... of
--    ...
--    (0,X) -> ...
--    (0,Y) -> ...
--    (1,Z) -> ...
--    ...
--
-- will translate (loosely) to
--
-- case ... of
--   ...
--   t -> case t of
--     (a,b) -> case a of
--       0 -> case b of
--         X -> ...
--         Y -> ...
--         _ -> FAIL
--       1 -> case b of
--         Z -> ...
--         _ -> FAIL
--
-- It's worth noting that you can replace the numeric literals with any other
-- kind of literal, or constructor match, the principle remains the same.
matchLit :: [(Expr,Type)] -> Match Pattern
         -> (Literal, [([P.Pattern], Match Pattern)]) -> TC (Match Pattern)
matchLit args err (lit,arms) =
  do body <- match args arms err
     return (MPat (PLit lit) body)

-- | Unpack the tuple using a PTuple, then continue matching all other branches.
-- The reason that this produces a case with a single branch is that the tuple
-- matches are irrefutable -- they can only ever match successfully.
matchTuple :: Type -> [(Expr,Type)] -> Match Pattern
           -> [([P.Pattern], ([P.Pattern],Match Pattern))] -> TC (Match Pattern)
matchTuple sty args err tarms = case tarms of
  -- shouldn't be possible
  []       -> tcPanic [ "matchTuple: no match arms" ]
  (qs,_):_ ->
    do let arity = length qs

       ns   <- replicateM arity freshName
       ntys <- replicateM arity freshTVar
       unify sty (ttuple ntys)

       let unpack (ps,(ps',m)) = (ps ++ ps', m)
       body <- match (zip (map EVar ns) ntys ++ args) (map unpack tarms) err

       let ps = zipWith Param ns ntys
       return (MPat (PTuple (map Just ps)) body)


-- The Mixture Rule ------------------------------------------------------------

mixtureRule :: [(Expr,Type)] -> [([P.Pattern], Match Pattern)]
            -> Match Pattern -> TC (Match Pattern)
mixtureRule args arms err = foldrM (match args) err (groupBy bothWild arms)
  where

  -- groups wild things together, and non-wild things together
  bothWild a b = (wa && wb) || (not wa && not wb)
    where
    wa = isWild a
    wb = isWild b

  isWild (ps,_) = case ps of
    P.PVar _    : _ -> True
    P.PWildcard : _ -> True
    _               -> False


-- Cleanup ---------------------------------------------------------------------

-- | Remove unused variable bindings from a Match.
--
-- XXX it would be nice if this emitted warnings about the variables being
-- unused.
cleanupMatch :: Match Pattern -> Match Pattern
cleanupMatch m0 = fst (go m0)
  where
  go m = case m of

    MCase s sty body ->
      let (body',used) = go body
       in (MCase s sty body', freeVars s `Set.union` used)

    MRename n e ety body ->
      let (body',used) = go body
       in if n `Set.member` used
             then (MRename n e ety body', freeVars e `Set.union` used)
             else (body', used)

    MPat (PCon n ns) body ->
      let (body',used) = go body
          checkUsed mb = do param <- mb
                            guard (pName param `Set.member` used)
                            return param
       in (MPat (PCon n (map checkUsed ns)) body', used)

    MPat (PTuple ps) body ->
      let (body',used) = go body
          checkUsed mb = do param <- mb
                            guard (pName param `Set.member` used)
                            return param
       in (MPat (PTuple (map checkUsed ps)) body', used)

    MPat p body ->
      let (body',used) = go body
       in (MPat p body', used)

    -- prune off the right branch when the left is irrefutable
    MSplit l r ->
      let (l',lu) = go l
          (r',ru) = go r
       in if irrefutable l'
             then (l', lu)
             else (MSplit l' r', lu `Set.union` ru)

    MExpr e -> (m, freeVars e)

    MGuard g body ->
      let (body',used) = go body
      in (MGuard g body', freeVars g `Set.union` used)

    MFail -> (m,Set.empty)
