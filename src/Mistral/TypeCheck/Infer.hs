{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}

module Mistral.TypeCheck.Infer ( tcModule ) where

import           Mistral.Driver
import           Mistral.ModuleSystem.Export ( Export(..) )
import           Mistral.ModuleSystem.Interface ( Iface(ifaceModName) )
import qualified Mistral.Parser.AST as P
import           Mistral.TypeCheck.AST
import           Mistral.TypeCheck.Env
                     ( Env, addSchema, vtSchema, checkingSchema, depModules )
import           Mistral.TypeCheck.Monad
import           Mistral.TypeCheck.Patterns ( withPatEnv, genMatch, tcLit )
import           Mistral.TypeCheck.Solver
import           Mistral.TypeCheck.TcData
import           Mistral.TypeCheck.Translate
import           Mistral.TypeCheck.Unify
                     ( Types(..), freeBind, freeBinds, apply )
import           Mistral.Utils.PP
import           Mistral.Utils.Panic ( panic )
import           Mistral.Utils.SCC ( Group(..), groupElems, scc )
import           Mistral.Utils.Source
                     ( Source, Located(..), getSource, noSource )

import           Control.Monad
                     ( unless, mzero, mplus, zipWithM, replicateM, foldM, when )
import           Data.Foldable ( foldMap )
import           Data.Function ( on )
import           Data.List ( partition, groupBy, sortBy )
import           Data.Maybe ( isJust )
import           Data.Monoid ( mempty, mappend, mconcat )
import qualified Data.Set as Set
import           Data.Traversable ( traverse )

tcPanic :: [String] -> a
tcPanic  = panic "Mistral.TypeChecker.Infer"

-- | Typecheck a module.
tcModule :: P.Module -> TC Module
tcModule m = failErrs $
  do let tds = P.splitTopDecls (P.modDecls m)

     (dEnv,datas) <- tcDataDecls (P.topDatas tds)
     (ds,goals)   <- withEnv dEnv (collectGoals (tcTopDecls (P.topDecls tds)))

     unless (null goals) (tcErr (unsolvedGoals goals))

     env     <- getEnv
     assumps <- getAssumps
     return Module { modName  = locThing (P.modName m)
                   , modDeps  = map ifaceModName (depModules env)
                   , modDatas = datas
                   , modBinds = ds
                   , modInsts = assumps }

-- | A message about unsolved top-level goals.
unsolvedGoals :: [Goal] -> PPDoc
unsolvedGoals goals =
  hang (text "unresolved top-level goals")
     2 (vcat (map pp goals))


-- Top-level Declarations ------------------------------------------------------

bindEnv :: Bind a -> Env
bindEnv b = addSchema (bName b) (bType b) mempty

groupEnv :: Group (Bind a) -> Env
groupEnv g = foldMap bindEnv (groupElems g)

tcTopDecls :: [P.Decl P.Expr] -> TC [Group Decl]
tcTopDecls decls = go (scc decls)
  where
  go gs = case gs of

    g:rest ->
      do g' <- tcTopDeclGroup g
         let env = groupEnv g'

         ds' <- withEnv env (go rest)
         return (g' : ds')

    [] ->
      return []


tcTopDeclGroup :: Group (P.Decl P.Expr) -> TC (Group Decl)
tcTopDeclGroup g = case g of

  NonRecursive d ->
    do (_,[d']) <- tcDecls [d]
       return (NonRecursive d')

  Recursive decls ->
    do (_,decls') <- tcDecls decls
       return (Recursive decls')


-- Binding Environments --------------------------------------------------------

-- | Construct the typing environt for a group of top-level bindings.
recGroupEnv :: [P.Bind a] -> TC Env
recGroupEnv  = foldM getSig mempty
  where
  getSig env b =
    do schema <- getBindSig b
       return (addSchema (locThing (P.bName b)) schema env)

-- | Find the actual binding in a declaration.
getBind :: P.Decl a -> P.Bind a
getBind d = case d of
  P.DBind b      -> b
  P.DSource _ d' -> getBind d'
  P.DSig _       -> tcPanic [ "unexpected signature" ]

-- | Translate the schema for things with a signature, and add a variable for
-- things that don't.
getBindSig :: P.Bind a -> TC Schema
getBindSig b = case P.bSig b of
  Just pschema -> translateSchema pschema
  Nothing      -> mkSchema `fmap` freshTVar


-- Topologies ------------------------------------------------------------------

tcTopo :: P.Topology -> Type -> TC Topo
tcTopo topo ety =
  do unify ety topoCon
     go (scc (P.topoElements topo))
  where
  go stmts = case stmts of

    NonRecursive s : rest ->
      do (env,t) <- tcTopoStmt s
         t'      <- withEnv env (go rest)
         return (t `mappend` t')

    Recursive _ : _ ->
      do tcErr (text "invalid recursive topology declaration")
         mzero

    [] -> return mempty

tcTopoStmt :: P.TopoStmt -> TC (Env,Topo)
tcTopoStmt stmt = case stmt of

  P.TSNode (Just ln) spec -> withSource (getSource ln) $
    do spec' <- tcNode spec
       return ( addSchema (locThing ln) (mkSchema nodeCon) mempty
              , mempty { topoNodes = [Named (locThing ln) spec'] })

  P.TSNode Nothing _ ->
    tcPanic [ "renamer bug: unnamed node" ]

  P.TSLink l ->
    do l' <- tcLink l
       return ( mempty, mempty { topoLinks = [l'] })

  P.TSUsing _ _ ->
    do tcErr (text "topology using expression not supported")
       return (mempty, mempty)

  P.TSComp _ ->
    do tcErr (text "topology comprehensions not supported")
       return (mempty, mempty)

  P.TSSource src stmt' ->
    withSource src (tcTopoStmt stmt')


tcNode :: P.Node -> TC Node
tcNode n =
  do ty <- freshTVar

     spec' <- tcExpr (P.nSpec n) ty
     tags' <- mapM (withLoc tcTag) (P.nTags n)

     ((),goals) <- collectGoals $
       do addGoal =<< userGoal (nodeSpecProp ty)
          simplifyConstraints

     unless (null goals) $ tcErr $
       hang (text "unexpected constraint(s) generated from node:")
          2 (vcat (map pp goals))

     return Node { nSpec  = spec'
                 , nType  = ty
                 , nTags  = tags'
                 , nTasks = [] }

tcLink :: P.Link -> TC Link
tcLink link =
  do lt <- tcLinkTarget (P.linkLeft  link)
     rt <- tcLinkTarget (P.linkRight link)

     tags' <- mapM (withLoc tcTag) (P.linkTags link)

     return Link { lLeft  = lt
                 , lRight = rt
                 , lTags  = tags' }

-- | Check a link target.
tcLinkTarget :: P.LinkTarget -> TC Name
tcLinkTarget lt = case lt of

  P.LTName ln -> withSource (getSource ln) $
    do let name = locThing ln
       (_,ty) <- freshVarType =<< lookupSchema name

       -- assert that this thing is a valid link target
       ((),goals) <- collectGoals $
         do addGoal =<< userGoal (linkTargetProp ty)
            simplifyConstraints

       unless (null goals) $
           (tcErr (text "unexpected constraints generated from link"))

       return name

  P.LTTag _ ->
    do tcErr (text "tags aren't supported as link targets")
       mzero


-- TaskSet ---------------------------------------------------------------------

tcTaskSet :: P.TaskSet -> Type -> TC Tasks
tcTaskSet b ety =
  do unify ety taskSetCon
     let stmts = P.tsTasks b
     tasks' <- withEnv (foldMap taskStmtEnv stmts) (mapM tcTaskStmt stmts)
     return (mconcat tasks')

taskStmtEnv :: P.TaskStmt -> Env
taskStmtEnv stmt = case stmt of
  P.TStTask (Just ln) _ -> addSchema (locThing ln) (mkSchema taskHandleCon) mempty
  P.TStTask _ _         -> mempty

  -- XXX can this create an environment?
  P.TStComp _           -> mempty
  P.TStSource _ stmt'   -> taskStmtEnv stmt'


tcTaskStmt :: P.TaskStmt -> TC Tasks
tcTaskStmt stmt = case stmt of

  P.TStTask (Just ln) t -> withSource (getSource ln) $
    do t' <- tcTask t
       return mempty { taskTasks = [Named (locThing ln) t'] }

  P.TStTask Nothing _ ->
    tcPanic [ "bug in the renamer: unnamed task" ]

  P.TStComp _ ->
    do tcErr (text "task comprehensions aren't supported")
       return mempty

  P.TStSource src stmt' ->
    withSource src (tcTaskStmt stmt')


tcTask :: P.Task -> TC Task
tcTask t =
  do e'    <- tcExpr (P.tBody t) stateCon
     cs'   <- tryMapM tcTaskConstraint (P.tConstraints t)
     tags' <- mapM (withLoc tcTag) (P.tTags t)
     return Task { tBody        = e'
                 , tConstraints = cs'
                 , tTags        = tags' }

tcTaskConstraint :: P.TaskConstraint -> TC TaskConstraint
tcTaskConstraint tc = case tc of

  P.After {} ->
    do tcErr (text "after constraints aren't supported")
       mzero

  P.On e ->
    do (e',goals) <- collectGoals $
         do ty <- freshTVar
            addGoal =<< userGoal (taskTargetProp ty)
            e' <- tcExpr e ty
            simplifyConstraints
            return (TCOn ty e')

       unless (null goals) $
         do tcErr $ hang (text "Unable to validate `on` constraint")
                       2 (vcat (map pp goals))
            mzero

       return e'


  P.TCSource src tc' ->
    withSource src (tcTaskConstraint tc')


-- Schedules -------------------------------------------------------------------

tcSchedule :: P.Schedule -> Type -> TC Sched
tcSchedule b ety =
  do unify ety scheduleCon
     go (scc (P.sStmts b))
  where
  go gs = case gs of

    NonRecursive s : rest ->
      do sched  <- tcSchedStmt s
         sched' <- go rest
         return (sched `mappend` sched')

    Recursive _ : _ ->
      do tcErr (text "invalid recursive schedule declaration")
         mzero

    [] -> return mempty

tcSchedStmt :: P.SchedStmt -> TC Sched
tcSchedStmt stmt = case stmt of

  P.Using tasks topo ->
    do tasks' <- tcExpr tasks taskSetCon
       topo'  <- tcExpr topo  topoCon
       return (mempty { schedStmts = [SchedUsing tasks' topo' []] })

  P.STag _ _ ->
    do tcErr (text "unsupported tagged schedule statement")
       return mempty

  P.SExpr _ ->
    do tcErr (text "unsupported expression statement in schedule")
       return mempty

  P.SSource src stmt' ->
    withSource src (tcSchedStmt stmt')


-- Tags ------------------------------------------------------------------------

tcTag :: P.Tag -> TC Expr
tcTag t = case t of
  P.TagAtom a -> tcExpr (P.ELit (LAtom a)) atomCon
  P.TagVar n  ->
    do (_,tn,e') <- freshVarTypeRewrite (EVar n) =<< lookupSchema n
       traceMsg (pp n <+> char ':' <+> pp tn)
       unify tn atomCon

       return e'


-- Bindings --------------------------------------------------------------------

-- | Generate an environment, set of types and list of params for the arguments
-- of a binding.
bindParams :: [Located Name] -> TC (Env,[Type],[Param])
bindParams lns =
  do tys <- replicateM (length lns) freshTVar
     let params = zipWith mkParam lns tys
         env    = foldl paramEnv mempty params
     return (env,tys,params)
  where
  mkParam ln ty  = Param { pName = locThing ln, pType = ty }
  paramEnv env p = addSchema (pName p) (mkSchema (pType p)) env

-- | Typecheck a group of recursive bindings, producing a list of declarations.
tcBinds :: (PP b, Types b) => (a -> Type -> TC b) -> [P.Bind a] -> TC [Bind b]
tcBinds checkBody binds =
  do env <- recGroupEnv binds
     withEnv env $
       do let (sigs,noSigs) = partition (isJust . P.bSig) binds
          sigs'   <- mapM (tcSigBind checkBody) sigs
          noSigs' <- tcNoSigBinds checkBody noSigs
          let decls' = sigs' ++ noSigs'
          return decls'

-- | Check a single declaration that had a signature.
tcSigBind :: (PP b, Types b) => (a -> Type -> TC b) -> P.Bind a -> TC (Bind b)
tcSigBind checkBody b = withSource (P.bName b) $
  do let name = locThing (P.bName b)
     vt <- lookupSchema name
     let schema = vtSchema vt

     traceMsg $ hang (text "checking")
                   2 (pp name <+> char ':' <+> pp schema)

     ((ps,params,body,res),goals) <- collectGoals $ localAssumps $
       do -- instantiate the schema, and assume all the constraints
          ((ps,ty),assumps) <- collectGoals (freshType schema)
          mapM_ (assume . goalToAssump) assumps

          -- the type variables in the assumptions had better all be mentioned
          -- in the rhs of the type (no type functions here).
          let isAmbiguous g = not (typeVars g `Set.isSubsetOf` typeVars ty)
          mapM_ (tcErr . ambiguousConstraint schema) (filter isAmbiguous assumps)

          -- the parameters used during the instantiation get skolemized
          withSkolems (Set.fromList ps) $
            do -- extract the types of all the arguments
               (argEnv,tps,params) <- bindParams (P.bParams b)
               res                 <- freshTVar
               unify ty (foldr tarrow res tps)

               -- check the body with the arguments in scope
               b' <- withEnv argEnv (checkBody (P.bBody b) res)

               -- try to solve the generated constraints, given the assumptions
               -- introduced by the signature
               simplifyConstraints

               return (ps,params,b',res)

     -- generate a binding substitution for the skolem variables, and merge it
     -- with the current substitution
     u0 <- getSubst
     let u = freeBinds [ (p,TVar (TVBound s))
                       | p <- ps
                       | s <- sParams schema ] `mappend` u0

         -- apply the substitution to the new body of the binding
         (params',body',res') = apply u (params,body,res)

     -- only keep goals that don't mention the skolem variables.  ones that
     -- weren't solved, but do mention the skolems represent an under-specified
     -- context from the user signature.
     let skolems            = Set.fromList ps
         skolemsFreeIn g    = Set.null (typeVars g `Set.intersection` skolems)
         (unsolved,invalid) = partition skolemsFreeIn goals

     addGoals unsolved

     -- add some errors about the leftover invalid constraints
     mapM_ (tcErr . noInst schema) (apply u invalid)

     -- check that the skolems haven't leaked into the environment
     env <- getEnv
     unless (skolemsFreeIn env) $
       tcErr (sep [ text "Rigid variable(s) leaked into the environment;"
                  , text "signature not polymorphic enough" ])

     defBody <- defaultBody body'

     return Bind { bName    = name
                 , bExport  = Public
                 , bType    = vtSchema vt
                 , bTParams = sParams schema
                 , bCParams = []
                 , bParams  = params'
                 , bBody    = defBody
                 , bResult  = res' }

-- | The error message about goals left unsatisfied by a user signature.
noInst :: Schema -> Goal -> PPDoc
noInst schema g =
  hang (fsep [ text "Unable to prove", backquotes (pp (gProp g))
             , text "from user signature" ])
     2 (pp schema)
  $$
  fsep [ text "Possible fix: add the constraint", backquotes (pp (gProp g))
       , text "to the context of the signature" ]

ambiguousConstraint :: Schema -> Goal -> PPDoc
ambiguousConstraint _schema g =
  hang (text "Ambiguous constraint" <+> backquotes (pp (gProp g)))
     2 (sep [ text "forall'd type variables present in constraints must be"
            , text "mentioned after the `=>`" ])


-- | Default any leftover variables in the body that aren't mentioned in the
-- environment.  This can be produced by weird situations like this:
--
--   let { k = j; j = k} in actions { k ; pure () }
--
-- The use of k in the actions block will end up with a unification
-- variable as its type, but that variable won't be considered for
-- generalization.
defaultBody :: Types a => a -> TC a
defaultBody e =
  do env <- getEnv
     e'  <- applySubst e
     let defaultable  = typeVars e' Set.\\ typeVars env
         defaultVar v = freeBind v unitCon
         u            = foldMap defaultVar (Set.toList defaultable)

     return (apply u e')


-- | Check a group of recursive declarations that don't have signatures.
tcNoSigBinds :: (PP b, Types b)
             => (a -> Type -> TC b) -> [P.Bind a] -> TC [Bind b]
tcNoSigBinds checkBody binds
  | null binds = return []
  | otherwise  =
  do traceMsg $ hang (text "inferring types for group:")
                   2 (commas (map (pp . P.bName) binds))

     rec let ptys      = map (TVar . TVFree) gvars
             step re b = do ty <- freshTVar
                            let name   = locThing (P.bName b)
                                schema = mkSchema ty
                                body | null gvars = EVar name
                                     | otherwise  = ETApp (EVar name) ptys
                            return (checkingSchema name schema body re)

         recEnv    <- foldM step mempty binds
         (pbs,cxt) <- withEnv recEnv $ collectGoals $
            do bs' <- mapM (tcPartial checkBody) binds
               simplifyConstraints
               return bs'

         env <- getEnv

         -- IMPORTANT: only gather type variables from the type, as examining the
         -- body will cause a loop.
         tys <- applySubst (map pbType pbs)
         let genVars = typeVars tys Set.\\ typeVars env
         let gvars   = Set.toList genVars

     -- should be OK to print them at this point
     traceMsg $ fsep [ text "generalizing variables:", commas (map pp gvars) ]

     -- filter for goals that don't mention any of the variables being
     -- generalized
     let keepGoal g      = Set.null (Set.intersection (typeVars g) genVars)
         (keep,genGoals) = partition keepGoal cxt

     addGoals keep

     unless (null keep) $ traceMsg $
       hang (text "unsolved goals")
          2 (vcat (map pp keep))

     mapM (generalizePartialBind gvars genGoals) =<< applySubst pbs


-- | A binding, lacking type information.
data PartialBind a = PartialBind
  { pbSource :: Source
  , pbExport :: Export
  , pbName   :: Name
  , pbType   :: Type
  , pbParams :: [Param]
  , pbBody   :: a
  , pbResult :: Type
  } deriving (Show)

instance PP a => PP (PartialBind a) where
  ppr pb =
       pp (pbName pb) <+> char ':' <+> pp (pbType pb)
    $$ hang (hsep [ pp (pbName pb)
                  , vcat (map (ppPrec 10) (pbParams pb))
                  , char '=' ])
          2 (pp (pbBody pb))

instance Types a => Types (PartialBind a) where
  typeVars pb       = typeVars (pbType pb, pbParams pb, pbBody pb, pbResult pb)
  applyBndrs b u pb = pb { pbType   = applyBndrs b u (pbType pb)
                         , pbParams = applyBndrs b u (pbParams pb)
                         , pbBody   = applyBndrs b u (pbBody pb)
                         , pbResult = applyBndrs b u (pbResult pb) }

-- | Produce a partially-checked binding, emitting any goals to the environment.
tcPartial :: (a -> Type -> TC b) -> P.Bind a -> TC (PartialBind b)
tcPartial checkBody b = withSource (P.bName b) $
  do let name = locThing (P.bName b)
     -- ty will be the variable given to this binding in tcNoSigBinds
     (_,ty) <- freshVarType =<< lookupSchema name

     (argEnv,tys,params) <- bindParams (P.bParams b)
     bty                 <- freshTVar

     unify ty (foldr tarrow bty tys)

     -- check the body in the environment of the params
     body' <- withEnv argEnv (checkBody (P.bBody b) bty)

     return PartialBind { pbSource = getSource (P.bName b)
                        , pbExport = Public
                        , pbName   = name
                        , pbType   = ty
                        , pbParams = params
                        , pbBody   = body'
                        , pbResult = bty }

-- | Generalize a single partial binding.
generalizePartialBind :: Types a
                      => [TParam] -> [Goal] -> PartialBind a -> TC (Bind a)
generalizePartialBind ps cxt pb = withSource (pbSource pb) $
  do (schema,gs,pb') <- generalize ps cxt (pbType pb) pb

     unless (null gs)
         (tcPanic [ "unexpected goals leftover after generalization" ])

     traceMsg (pp (pbName pb') <+> char ':' <+> pp schema)

     defBody <- defaultBody (pbBody pb')

     return Bind { bName    = pbName pb'
                 , bExport  = Public
                 , bType    = schema
                 , bTParams = sParams schema
                 , bCParams = []
                 , bParams  = pbParams pb'
                 , bBody    = defBody
                 , bResult  = pbResult pb' }


-- Expressions -----------------------------------------------------------------

-- | Typecheck a group of recursive declarations, producing a list of
-- declarations.
tcDecls :: [P.Decl P.Expr] -> TC (Env,[Decl])
tcDecls decls =
  do decls' <- tcBinds tcExpr (map getBind decls)
     return (foldMap bindEnv decls', decls')


-- | Typecheck a group of declarations from a let-expression.
tcLetDecls :: [P.Decl P.Expr] -> TC (Env,[Group Decl])
tcLetDecls decls = go [] [] (scc decls)
  where

  go envs acc gs = case gs of

    NonRecursive d : rest -> 
      do (env',[d']) <- tcDecls [d]
         let d'' = d' { bExport = Private }
         withEnv env' (go (env' : envs) (NonRecursive d'' : acc) rest)

    Recursive ds : rest ->
      do (env',ds') <- tcDecls ds
         let ds'' = [ d { bExport = Private } | d <- ds' ]
         validateRec ds' ds
         withEnv env' (go (env' : envs) (Recursive ds'' : acc) rest)

    [] -> return (mconcat envs, reverse acc)

-- | Detect and throw errors when recursive values are found.  The source
-- declarations are used when reporting the error.
validateRec :: [Decl] -> [P.Decl P.Expr] -> TC ()
validateRec ds sds
  | any notFun ds = invalidValueRecursion sds
  | otherwise     = return ()
  where
  -- when the inferred type isn't a function.
  notFun d = length argTys <= 1
    where
    argTys = elimTArrows (sType (bType d))

invalidValueRecursion :: [P.Decl a] -> TC ()
invalidValueRecursion ds =
  withSource (getSource ds) $
      tcErr $ text "values may not be recursive:"
           $$ nest 2 (vcat (map ppName ds))
  where
  ppName d = fsep [ backquotes(pp (locThing (P.bName bind))), text "at"
                  , pp (getSource d)]
    where
    bind = getBind d


-- | Check an expression against a type.
tcExpr :: P.Expr -> Type -> TC Expr
tcExpr e ety = case e of

  P.EApp f xs ->
    do let len = length xs
       txs <- replicateM len freshTVar
       f'  <- tcExpr f (foldr tarrow ety txs)
       xs' <- zipWithM tcExpr xs txs
       return (appE f' xs')

  P.ELet ds b ->
    do (env',ds') <- tcLetDecls ds
       withEnv env' $
         do b'   <- tcExpr b ety
            return (ELet ds' b' ety)

  P.EVar n ->
    do (_,tn,e') <- freshVarTypeRewrite (EVar n) =<< lookupSchema n
       traceMsg (pp n <+> char ':' <+> pp tn)
       unify tn ety

       return e'

  P.ECon n ->
    do (_,tn,e') <- freshVarTypeRewrite (ECon n) =<< lookupSchema n
       unify tn ety
       return e'

  P.ETuple es ->
    do ts  <- replicateM (length es) freshTVar
       unify ety (ttuple ts)
       es' <- zipWithM tcExpr es ts
       return (EMkTuple (zip ts es'))

  P.ELit lit ->
    do tcLit lit ety
       return (ELit lit)

  P.EList es ->
    do aty      <- freshTVar
       unify ety (tlist aty)
       es'      <- mapM (`tcExpr` aty) es
       return (EMkList aty es')

  P.EIf c t f ->
    do c'  <- tcExpr c boolCon
       t'  <- tcExpr t ety
       f'  <- tcExpr f ety
       return (ifE c' t' f' ety)

  P.EActions as ->
    do as' <- tcActions as ety
       ity <- freshTVar
       aty <- freshTVar
       unify ety (tstmts ity aty)
       return (EStmts ity aty as')

  P.ETransitions ts ->
    do (cases,mbTimeout,mbWild,rst) <- tcTransitions ts
       unify ety stateCon
       return (EStmts openCon stateCon [AReceive rst cases mbTimeout mbWild])

  P.ECase s arms ->
    do sty <- freshTVar
       s'  <- tcExpr s sty
       m   <- tcCase s' sty arms ety
       src <- srcLoc
       return (ECase src m ety)

  -- Sources just add context for further checking
  P.ESource src e' -> withSource src (tcExpr e' ety)

  P.ETopology t ->
    do t' <- tcTopo t ety
       return (ETopo t')

  P.ETaskSet t ->
    do t' <- tcTaskSet t ety
       return (ETaskSet t')

  P.ESchedule s ->
      do s' <- tcSchedule s ety
         return (ESchedule s')

  -- INVARIANT: the renamer should be naming lambdas, turning them into
  -- let-bound functions.
  P.ELam _ _ -> tcPanic
       [ "Unexpected ELam, should have been removed during renaming" ]

  -- INVARIANT: the parser should remove 'EWildPat's, as they're only used
  -- temporarily when parsing patterns as expressions.
  P.EWildPat -> tcPanic
       [ "Unexpected EWildPat" ]

-- | Check a group of sequential actions.
tcActions :: [P.Stmt] -> Type -> TC [Action]
tcActions stmts0 ety = go stmts0
  where
  go stmts = case stmts of

    [s@P.SSeq{}] ->
      do (_,s') <- tcStmt s ety
         return [s']

     -- special case blocks of actions that end in a variable binding, so that
     -- we can add a warning
    [P.SBind bind e] ->
      do tcWarn (unusedBinding bind)
         (_,s') <- tcStmt (P.SSeq e) ety
         return [s']

    [P.SAssign bind e] ->
      do tcWarn (unusedBinding bind)
         (_,p,rhs) <- tcSAssign bind e ety
         return [ABind Nothing rhs (pType p)]

     -- action blocks that end in a `let` aren't valid, as we don't know what
     -- the result of the whole expression should be.
    [P.SLet{}] ->
      do tcErr $ fsep [ text "the last line of an action block cannot end in"
                      , text "a let binding" ]
         mzero

    [P.StSource src s'] ->
      withSource src (go [s'])

    s:rest
        -- introduce some bindings over the tail of a list of actions
      | P.SLet ds <- noSource s -> withSource s $
        do (env,ds') <- tcLetDecls ds
           traceMsg (text "SLet")
           withEnv env $
             do rest' <- tcActions rest ety
                ity   <- freshTVar
                aty   <- freshTVar
                unify ety (tstmts ity aty)
                return [ABind Nothing (ELet ds' (EStmts ity aty rest') ety) aty]

      -- actions to the left of a bind should always be of type Action.
      | otherwise ->
        do aTy      <- freshTVar
           (env,s') <- tcStmt s (taction aTy)
           withEnv env $
             do rest' <- go rest
                return (s':rest')

    -- attempt to unify the result with `Action ()`, when the list of statements
    -- is empty, logging a helpful message in the case that it's not
    [] -> do unify ety (taction unitCon)
                 `mplus` do tcErr (emptyActionsBlock ety)
                            mzero
             return []


unusedBinding :: Located Name -> PPDoc
unusedBinding ln = fsep
  [ text "the last line of an action block introduced a binding,"
  , backquotes (pp ln)
  , text "which can never be used." ]

emptyActionsBlock :: Type -> PPDoc
emptyActionsBlock ety =
  fsep [ text "the empty statement is implicity of type"
       , text "`Action ()`, but is used as type "
       , ppr ety ]


-- | Check a single statement in isolation, yielding out the environment for any
-- bindings that it produces.
tcStmt :: P.Stmt -> Type -> TC (Env,Action)
tcStmt stmt0 ty = go stmt0
  where
  go stmt = case stmt of
    P.SBind ln rhs ->
      do rhs'                  <- tcExpr rhs ty
         (env,[lhsTy],[param]) <- bindParams [ln]
         unify ty (taction lhsTy)
         return (env,ABind (Just (pName param)) rhs' lhsTy)

    P.SAssign ln rhs ->
      do (env,p,rhs') <- tcSAssign ln rhs ty
         return (env, ABind (Just (pName p)) rhs' (pType p))

    P.SSeq e ->
      do e'    <- tcExpr e ty
         iTy   <- freshTVar
         resTy <- freshTVar
         unify ty (tstmts iTy resTy)
         return (mempty,ABind Nothing e' resTy)

    P.SLet{} ->
      tcPanic [ "tcStmt: P.SLet should have been handled in tcActions" ]

    P.StSource src stmt' ->
      withSource src (go stmt')

-- | Assignment statements will produce either a binding to an effectful thing,
-- or a binding to a pure expression that has `pure` wrapped around it.
tcSAssign :: Located Name -> P.Expr -> Type -> TC (Env,Param,Expr)
tcSAssign ln rhs ety =
  do rhsTy <- freshTVar
     rhs'  <- tcExpr rhs rhsTy

     (env,[lhsTy],[param]) <- bindParams [ln]

     -- examine the type inferred for the rhs.  if it's not an action type, then
     -- assume that it's a pure value.
     rhsTy' <- applySubst rhsTy
     traceMsg (pp rhsTy')
     tm     <- case elimTApp rhsTy' of

       (con,_) | con == stmtsCon ->
         do unify rhsTy (taction lhsTy)
            unify ety (taction lhsTy)
            return rhs'

       _ ->
         do unify rhsTy lhsTy
            unify ety (taction lhsTy)
            return (EApp (ETApp (EPrim PPure) [lhsTy]) rhs')

     return (env, param, tm)


tcTransitions :: [P.Transition] -> TC ([From], Maybe Timeout, Maybe Expr, Reset)
tcTransitions ts =
  do when (null ts) $
       do tcErr (text "invalid empty transition block")
          mzero

     let (fs, to, rst) = partitionTransitions ts

     (froms,mbWild) <- tcFroms fs

     -- only allow one timeout transition per transitions block
     mbTimeout <- case to of
       []  -> return Nothing
       [t] -> Just `fmap` tcTimeout t
       _   -> do tcErr (text "multiple timeout branches in transitions")
                 return Nothing

     return (froms, mbTimeout, mbWild, rst)


data TFrom = TFrom Source P.SourcePred P.Pattern P.Expr
             deriving (Show)

data TTimeout = TTimeout Source P.Expr P.Expr
                deriving (Show)

-- | Partition out transition arms into from patterns, and timeout patterns.
partitionTransitions :: [P.Transition] -> ([TFrom], [TTimeout], Reset)
partitionTransitions  = foldr step ([],[],NoReset)
  where
  step tr (fs,ts,r) = go mempty (P.trPattern tr)
    where
    body        = P.trBody tr
    go src tpat = case tpat of
      P.Receive p pat       -> (TFrom src p pat body:fs,  ts, r `mappend` NoReset)
      P.Timeout e           -> (fs, TTimeout src e body : ts, r `mappend` DoReset)
      P.At      e           -> (fs, TTimeout src e body : ts, r `mappend` NoReset)
      P.TPSource src' tpat' -> go src' tpat'

-- |
--  * Partition out the wild case, if there is one
--  * Check each from pattern in isolation
--  * Group from patterns by type
tcFroms :: [TFrom] -> TC ([From], Maybe Expr)
tcFroms fs =
  do (froms,mbWild) <- partitionTWild fs
     froms'  <- groupFroms =<< traverse tcFrom froms
     mbWild' <- traverse (`tcExpr` transitionCon) mbWild
     return (froms',mbWild')

data PartialFrom = PartialFrom { pfSource  :: P.SourcePred
                               , pfPattern :: P.Pattern
                               , pfType    :: Type -- ^ The type of the message
                               , pfBody    :: Expr }

-- | Check a single receive pattern.  As the type of the pattern isn't inferable
-- from the outer context, we require that the pattern be monomorphic.  The
-- effect of this is that after inference is done on the body of the branch,
-- if the type of the pattern is still polymorphic, it's considered an error.
tcFrom :: TFrom -> TC PartialFrom
tcFrom (TFrom src ts pat body) = withSource src $
  do -- check the body, assuming nothing about the pattern.
     newEnv <- tcSPat ts

     patTy <- freshTVar
     body' <- withEnv newEnv (withPatEnv patTy pat (tcExpr body transitionCon))

     -- apply anything learned from the pattern, or body to the type of the
     -- pattern.  this helps with the typed grouping.
     patTy' <- applySubst patTy
     traceMsg (text "receive pattern type:" <+> pp patTy')

     -- guard that the pattern is monomorphic (what would it mean to receive a
     -- list containing any element?)
     unless (Set.null (typeVars patTy'))
         (tcErr (text "receive patterns can't accept polymorpic types"))


     return (PartialFrom ts pat patTy' body')

tcSPat :: P.SourcePred -> TC Env
tcSPat P.SrcAny            = return mempty
tcSPat (P.SrcBind n)       = do
    pty <- freshTVar
    unify pty taskHandleCon
    return (addSchema n (mkSchema pty) mempty)
tcSPat (P.SrcTaskHandle h) = tcExpr (P.EVar h) taskHandleCon >>
                             return mempty
tcSPat (P.SrcTags to)      = tcTagOp to >> return mempty

tcTagOp :: P.TagOp -> TC ()
tcTagOp (P.TOIntersect a b) = tcTagOp a >> tcTagOp b
tcTagOp (P.TOUnion a b)     = tcTagOp a >> tcTagOp b
tcTagOp (P.TOTag t)         = withLoc tcTag t >> return ()

-- | Group receive patterns by the type of message they receive, turning them
-- into single case expressions.
groupFroms :: [PartialFrom] -> TC [From]
groupFroms froms =
  do let common = groupBy ((==) `on` pfType)
                $ sortBy (compare `on` pfType) froms

     mapM mkFrom common

-- | Turn a collection of partial receive patterns of the same type into one
-- final receive pattern.
mkFrom :: [PartialFrom] -> TC From
mkFrom partials = case partials of
  []  -> tcPanic [ "mkFrom: invalid group" ]
  p:_ -> do sender <- freshName
            msg    <- freshName

            let newMatch r  = (pfPattern r,) `fmap` newMatch' r
                newMatch' r = case pfSource r of
                    P.SrcAny          -> return $ MExpr (pfBody r)
                    P.SrcTags _       -> tcPanic ["Tags not supported by TC ATT."]
                    P.SrcBind newNm   ->
                     -- FIXME rename is misused here
                     return $ MRename newNm (EVar sender) taskHandleCon (MExpr (pfBody r))
                    P.SrcTaskHandle _ ->
                      -- XXX return $ eqE h (EVar sender)
                      tcPanic ["source-match requires equality (unsupported ATT)"]
            -- merge all matches together
            m  <- genMatch [(EVar msg, pfType p)] =<< mapM newMatch partials

            return (From sender msg (pfType p) m)

-- | Partition out the wildcard pattern into its own case.
partitionTWild :: [TFrom] -> TC ([TFrom], Maybe P.Expr)
partitionTWild fs = case partition (not . isWildTFrom) fs of
  (_,[])                 -> return (fs,Nothing)
  (fs',[TFrom _ P.SrcAny  _ e]) -> return (fs',Just e)
  _                      -> do tcErr (text "overlapping wild transitions")
                               return ([], Nothing)

-- | True when a from pattern can accept any message type from any source.
isWildTFrom :: TFrom -> Bool
isWildTFrom (TFrom _ ts pat0 _) = ts == P.SrcAny && go pat0
  where
  go pat = case pat of
    P.PWildcard      -> True
    P.PSource _ pat' -> go pat'
    _                -> False

-- | Check a single timeout transition.
tcTimeout :: TTimeout -> TC Timeout
tcTimeout (TTimeout src to body) = withSource src $
  do to'   <- tcExpr to timeCon
     body' <- tcExpr body transitionCon
     return Timeout { toTimeout = to', toBody = body' }

-- Case Expressions ------------------------------------------------------------

-- | Check the arms of a case expression, producing a 'Match' that uses parsed
-- patterns.  Produces one top-level pattern match on the scrutinee that binds
-- it to a fresh variable, then does all further case analysis using that
-- variable, instead of the scrutinee.
tcCase :: Expr -> Type -> [P.CaseArm] -> Type -> TC (Match Pattern)
tcCase s sty arms ety = genMatch [(s,sty)] =<< mapM check arms
  where
  check arm = withPatEnv sty (P.cPat arm) $
    do e <- tcExpr (P.cBody arm) ety
       return (P.cPat arm, MExpr e)
