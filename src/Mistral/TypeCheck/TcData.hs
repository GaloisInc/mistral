module Mistral.TypeCheck.TcData (
    tcDataDecls
  ) where

import           Mistral.Driver
import qualified Mistral.Parser.AST as P
import           Mistral.TypeCheck.AST
import           Mistral.TypeCheck.Env
import           Mistral.TypeCheck.Monad
import           Mistral.TypeCheck.Solver
import           Mistral.TypeCheck.Translate
import           Mistral.TypeCheck.Unify
import           Mistral.Utils.PP
import           Mistral.Utils.SCC
import           Mistral.Utils.Source

import           Control.Monad ( mapAndUnzipM, when )
import           Data.Foldable ( foldMap )
import           Data.Monoid ( mempty )
import qualified Data.Set as Set


tcDataDecls :: [P.Data] -> TC (Env,[Group Data])
tcDataDecls decls =
  do decls' <- go (scc decls)
     return (foldMap dataEnv (concatMap groupElems decls'), decls')
  where
  go gs = case gs of

    Recursive ds:rest ->
      do traceMsg $ hang (text "checking recursive data(s): ")
                       2 (vcat (map pp ds))
         ds' <- mapM tcData ds

         let g = Recursive ds'
         traceMsg (text "trying to make Serializable instance(s)")
         checkDerivedInstance serializableProp g

         (g :) `fmap` withEnv (foldMap dataEnv ds') (go rest)

    NonRecursive d:rest ->
      do traceMsg (hang (text "checking data: ") 2 (pp d))
         d' <- tcData d

         let g = NonRecursive d'
         traceMsg (text "trying to make a Serializable instance")
         checkDerivedInstance serializableProp g

         (g :) `fmap` withEnv (dataEnv d') (go rest)

    [] -> return []

-- | This amounts to translation into the core AST, as we don't kind check
-- anything anyway.
tcData :: P.Data -> TC Data
tcData d = withSource (P.dSource d) $
  do let (pmap,ps) = mkParamMap (P.dParams d)
     cs' <- mapM (tcConstr pmap) (P.dConstrs d)
     return Data { dName    = locThing (P.dName d)
                 , dParams  = ps
                 , dConstrs = cs' }

tcConstr :: ParamMap -> P.Constr -> TC Constr
tcConstr pmap c = withSource (P.cSource c) $
  do ps <- mapM (translateType pmap) (P.cParams c)
     return Constr { cName = locThing (P.cName c), cParams = ps }


dataEnv :: Data -> Env
dataEnv d = foldMap (constrEnv (dParams d) resTy) (dConstrs d)
  where
  resTy = tapp (TCon (dName d)) (map (TVar . TVBound) (dParams d))

constrEnv :: [TParam] -> Type -> Constr -> Env
constrEnv ps resTy c = addSchema (cName c) ty mempty
  where
  ty = Forall { sParams = ps
              , sProps  = []
              , sType   = foldr tarrow resTy (cParams c)
              }


-- Derived Instances -----------------------------------------------------------

-- | Check that we can make a derived instance for a one-parameter class.
checkDerivedInstance :: (Type -> Prop) -> Group Data -> TC ()
checkDerivedInstance mkProp g =
  do let dataAssumps = map (genAssumps mkProp) ds

     (_,goals) <- collectGoals $ localAssumps $ try $
       do -- assume that each data declaration (and their type parameters) are
          -- serializable
          mapM_ assume dataAssumps

          (params,ids) <- mapAndUnzipM instData ds

          -- generate assumptions for the instantiated parameters
          let paramAssump (_,ty) = assume Inst { iProp = mkSchema (mkProp ty) }
          mapM_ paramAssump (concat params)

          -- generate Serializable constraints for each type in each constructor
          mapM_ (dataGoals mkProp) ids

          -- try to solve the generated constraints
          let skolems = foldMap (Set.fromList . map fst) params
          withSkolems skolems simplifyConstraints

     -- if no goals remain, the constructors are serializable, and we can safely
     -- add the assumptions to the environment
     when (null goals) (mapM_ assume dataAssumps)

  where
  ds = groupElems g

-- | Manufacture an assumption that the type has an instance of Prop.  Return
-- that assumption, as well as a freshly-instantiated version of the data type
-- being checked.
genAssumps :: (Type -> Prop) -> Data -> Inst
genAssumps mkProp d = Inst { iProp = schema }
  where
  ps   = dParams d
  ty   = tapp (TCon (dName d)) [ TVar (TVBound p) | p <- ps ]

  -- XXX not quite right, what about phantom variables?
  cxt  = [ serializableProp (TVar (TVBound p)) | p <- ps ]

  schema = Forall { sParams = ps, sProps = cxt, sType = mkProp ty }

-- | Instantiate the type parameters of a data type.  The instantiated
-- parameters are passed out.
instData :: Data -> TC ([(TParam,Type)], Data)
instData d =
  do ps <- mapM mkParam (dParams d)
     return (ps, apply (boundBinds ps) d { dParams = [] })

  where
  mkParam p = do p' <- freshTVarTemplate p
                 return (p', TVar (TVFree p'))

dataGoals :: (Type -> Prop) -> Data -> TC ()
dataGoals mkProp d = mapM_ (constrGoals mkProp) (dConstrs d)

-- | Add Serializable constraints for the type of each field of a constructor.
constrGoals :: (Type -> Prop) -> Constr -> TC ()
constrGoals mkProp c = addGoals =<< userGoals (map mkProp (cParams c))
