{-
       File      :      DoLinReg.hs
       Copyright : (c) Elena Badillo Goicoechea, 05/30/19, 
       Contains ...
       References (conceps and basic implementation):
          - https://samcgardner.github.io/2018/10/06/linear-regression-in-haskell.html
          - http://mccormickml.com/2014/03/04/gradient-descent-derivation/
-}

module RegModels where

import Data.List
import qualified Data.Vector as DV 
import RTypes
import DoLinReg

-- Trains a linear model
modReg :: Model -> Int -> Model
modReg mod0 iters = Model dataset' preds betas' r2' alpha  where
  betas' = linearReg coefs alpha dataset' iters
  coefs = RTypes.betas mod0
  dataset = RTypes.dataSet mod0
  dataset' = sliceDataset dataset preds
  r2' = adjRsq betas' dataset'
  alpha = RTypes.lrate mod0
  preds = RTypes.predvars mod0

{-- TASKS 1 - 5--}

{-- 1. Trains a univariate linear model for each predictive variable in the dataset
and stores them in a list--}
univars :: DataSet -> Int -> [Model]
univars dataset iters = [modReg (Model dataset [p] initbetas 0 0.1) iters| p <- dpreds] where
  dpreds = preds dataset 
  initbetas = Coefs (0, DV.fromList [0]) --replicate (length dpreds) 0





  

{-- Helper Funcs--}
sliceDataset :: DataSet -> [Int] -> DataSet
sliceDataset dataset indxs = DataSet (map (sliceObs indxs) obsList) dpreds where
  dpreds = preds dataset
  obsList = observs dataset

sliceObs ::  [Int] -> Obs -> Obs
sliceObs indxs obs  = Obs (y, (DV.fromList xlst')) where
  Obs (y, x) = obs
  xlst = DV.toList x
  xlst' = [xlst !! i | i <- indxs]



