{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Tableaux (plugin, tableaux, tableaux') where

import qualified Propositional as PL
import Solver.Tableaux
import Data.List (elemIndex)
import Control.Monad.State
import qualified Data.Map as M

import TcPluginM
import TcRnMonad ( TcPlugin(..), TcPluginResult(..)
                 , Ct(..), CtEvidence(..), CtLoc, ctLoc, ctPred
                 , mkNonCanonical, isTouchableTcM, unsafeTcPluginTcM
                 )
import TcRnTypes
import Plugins ( CommandLineOption, defaultPlugin, Plugin(..) )

-- external
import Control.Arrow       ((***))
import Control.Monad (when)
import Data.List           (partition)
import Data.Maybe          (mapMaybe)
import GHC.TcPluginM.Extra (evByFiat, lookupModule, lookupName)

-- GHC API
import FastString (fsLit)
import Module     (mkModuleName)
import OccName    (mkTcOcc)
import Plugins    (Plugin (..), defaultPlugin)
import TcEvidence (EvTerm)
import TcPluginM  (TcPluginM, tcLookupTyCon)
import TcRnTypes  (Ct, TcPlugin(..), TcPluginResult (..),
                   ctEvidence, ctEvPred)
import TyCon      (TyCon)
import Type       (EqRel (NomEq), PredTree (EqPred),
                   classifyPredType)
import Var (TyVar(..), Var(..))
#if __GLASGOW_HASKELL__ >= 711
import TyCoRep    (Type (..), TyLit (..))
#else
import TypeRep    (Type (..), TyLit (..))
#endif

plugin :: Plugin
plugin = defaultPlugin {
  tcPlugin = Just . thePlugin
  }

thePlugin :: [CommandLineOption] -> TcPlugin
thePlugin opts = TcPlugin
  { tcPluginInit  = pluginInit opts
  , tcPluginSolve = pluginSolve
  , tcPluginStop  = pluginStop
  }

data Solver = Solver
  { pbt   :: TyCon
  , andTc :: [TyCon]
  , orTc  :: [TyCon]
  , notTc :: TyCon
  , implTc :: [TyCon]
  }
  
pluginInit :: [CommandLineOption] -> TcPluginM Solver
pluginInit _opts = do
    md <- lookupModule tableauxModule tableauxPackage
    pbtNm <- lookupName md (mkTcOcc "ProofByTableaux")
    pbt <- tcLookupTyCon pbtNm
    md' <- lookupModule propModule tableauxPackage
    andNm <- forM ["&&", "And"] $ \tc -> lookupName md' (mkTcOcc tc)
    andTc <- mapM tcLookupTyCon andNm
    orNm <- forM ["||", "Or"] $ \tc -> lookupName md' (mkTcOcc tc)
    orTc <- mapM tcLookupTyCon orNm
    notNm <- lookupName md' (mkTcOcc "Not")
    notTc <- tcLookupTyCon notNm
    implNm <- forM ["-->", "Impl"] $ \tc -> lookupName md' (mkTcOcc tc)
    implTc <- mapM tcLookupTyCon implNm
    return Solver{..}
  where
    tableauxModule  = mkModuleName "Tableaux"
    propModule      = mkModuleName "Propositional"
    tableauxPackage = fsLit "gdp-demo"

pluginSolve :: Solver -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
pluginSolve _   _ _ []      = return (TcPluginOk [] [])
pluginSolve solver _ _ wanteds = do
    case failed of
      [] -> do
              when (length solved > 0) $ tcPluginIO $ putStrLn ("Solved " ++ show (length solved))
              return $ TcPluginOk (mapMaybe (\c -> (,c) <$> evMagic c) solved) []
      f  -> return $ TcPluginContradiction f
  where
    tableauxWanteds :: [(Ct, Type)]
    tableauxWanteds = mapMaybe (toTableaux solver) wanteds

    solved, failed :: [Ct]
    (solved, failed) = (map fst tableauxWanteds, [])

pluginStop :: Solver -> TcPluginM ()
pluginStop _ = return ()

toTableaux :: Solver -> Ct -> Maybe (Ct, Type)
toTableaux solver ct =
  case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq t1 t2 -> (ct,) <$> applyTableaux solver t1 t2
    _ -> Nothing
    
applyTableaux :: Solver -> Type -> Type -> Maybe Type
applyTableaux solver (TyConApp tc [x0]) x | tc == pbt solver && solveTabl solver x = Just x
applyTableaux _ _ _ = Nothing

type family ProofByTableaux p = p' | p' -> p

tableaux :: PL.Proof (ProofByTableaux p)
tableaux = error "proof by tableaux"

tableaux' :: ProofByTableaux p
tableaux' = error "proof by tableaux"

evMagic :: Ct -> Maybe EvTerm
evMagic ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
  EqPred NomEq t1 t2 -> Just (evByFiat "tableaux" t1 t2)
  _                  -> Nothing


solveTabl :: Solver -> Type -> Bool
solveTabl solver t = sat (evalState (typeToFormula t) (0, M.fromList []))
  where
    typeToFormula :: Type -> State (Int, M.Map Var Int) (Formula Int)
    typeToFormula = \case
      TyConApp tc [p,q] | tc `elem` andTc  solver -> And  <$> typeToFormula p <*> typeToFormula q
      TyConApp tc [p,q] | tc `elem` orTc   solver -> Or   <$> typeToFormula p <*> typeToFormula q
      TyConApp tc [p]   | tc == notTc  solver -> Not  <$> typeToFormula p
      TyConApp tc [p,q] | tc `elem` implTc solver -> Impl <$> typeToFormula p <*> typeToFormula q
      TyVarTy v                               -> do
        (idx, ctx) <- get
        case M.lookup v ctx of
          Just idx' -> return (Atom idx')
          Nothing   -> do
            let idx' = idx + 1
            put (idx', M.insert v idx' ctx)
            return (Atom idx')
      _ -> return $ Atom 17
      
