module Mistral.TypeCheck.Translate (
    ParamMap
  , mkParamMap
  , translateSchema
  , translateType ) where

import qualified Mistral.Parser.AST as P
import           Mistral.TypeCheck.AST
import           Mistral.TypeCheck.Monad
import           Mistral.Utils.Panic ( panic )
import           Mistral.Utils.PP
import           Mistral.Utils.Source ( noSource )

import           Control.Monad ( unless, mzero )
import qualified Data.Map as Map

tcPanic :: [String] -> a
tcPanic = panic "Mistral.TypeCheck.Translate"


-- Type Conversion -------------------------------------------------------------

type ParamMap = Map.Map Name TParam

mkParamMap :: [P.TParam] -> (ParamMap,[TParam])
mkParamMap ps = (Map.fromList params, map snd params)
  where
  params   = zipWith step [0 ..] ps
  step i p = (P.tpName p, TParam { tpUserName = Just (P.tpName p)
                                 , tpIndex    = i })

-- | This should form the basis for kind checking, once we decide to go that
-- route.
translateSchema :: P.Schema -> TC Schema
translateSchema (P.Forall nps qs ty src) = withSource src $
  do let (pmap,ps) = mkParamMap nps
     ty' <- translateType pmap ty
     qs' <- mapM (translateType pmap) qs
     return (Forall ps qs' ty')

translateType :: ParamMap -> P.Type -> TC Type
translateType ps = go
  where
  go ty = case ty of

    -- try to resolve synonyms with parameters
    P.TApp f xs | P.TCon n <- noSource f ->
      do mb <- lookupTSyn n
         case mb of
           Nothing  -> tTApp f xs
           Just syn ->
             do xs' <- mapM go xs
                ty' <- instSchema xs' (synType syn)
                return (TSyn n xs' ty')

    P.TApp f xs -> tTApp f xs

    P.TFun a b ->
      do a' <- go a
         b' <- go b
         return (a' `tarrow` b')

    P.TVar n ->
      case Map.lookup n ps of
        Just tp -> return (TVar (TVBound tp))
        Nothing -> tcPanic [ "unbound type parameter, bug in mkSchema?" ]

    -- try to resolve synonyms with no parameters
    P.TCon n ->
      do mb <- lookupTSyn n
         case mb of
           Nothing  -> return (TCon n)
           Just syn ->
             do unless (null (sParams (synType syn))) $
                  do tcErr (hsep [ text "type synonym", backquotes (pp n)
                                 , text "was not fully applied"])
                     mzero
                ty' <- instSchema [] (synType syn)
                return (TSyn n [] ty')

    P.TTuple ts ->
      ttuple `fmap` mapM go ts

    P.TList ty' ->
      tlist `fmap` go ty'

    P.TSource src ty' ->
      withSource src (go ty')

  tTApp f xs =
    do f'  <- go f
       xs' <- mapM go xs
       return (tapp f' xs')
