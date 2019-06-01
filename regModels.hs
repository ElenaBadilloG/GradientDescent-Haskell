{-
       File      :      DoLinReg.hs
       Copyright : (c) Elena Badillo Goicoechea, 05/30/19, 
       Contains ...
       References (concepts and basic implementation):
          - https://samcgardner.github.io/2018/10/06/linear-regression-in-haskell.html
          - http://mccormickml.com/2014/03/04/gradient-descent-derivation/
          - https://www.classes.cs.uchicago.edu/archive/2018/fall/12100-1/pa/pa5/index.html
-}

module RegModels where

import Data.List
import qualified Data.Vector as DV 
import RTypes
import DoLinReg

-- Trains a linear model
modReg :: Model -> Int -> Model
modReg mod0 iters = Model dataset dataset' preds betas' r2' alpha  where
  betas' = linearReg coefs alpha dataset' iters
  coefs = RTypes.betas mod0
  dataset = RTypes.origdataSet mod0
  dataset' = sliceDataset dataset preds
  r2' = adjRsq betas' dataset'
  alpha = RTypes.lrate mod0
  preds = RTypes.predvars mod0

{-- 1. Trains a univariate linear model for each predictive variable in the dataset
and stores them in a list--}
univars :: DataSet -> DataSet -> Int -> [Model]
univars origdataset dataset iters = [modReg (Model origdataset dataset [p] initbetas 0 0.1) iters| p <- dpreds] where
  dpreds = preds dataset 
  initbetas = Coefs (0, DV.fromList [0]) 

{-- 2. Trains a linear model including all possible regressors in the dataset
    return model_all--}
allvars :: DataSet -> DataSet -> Int -> Model
allvars origdataset dataset iters = modReg (Model origdataset dataset dpreds initbetas 0 0.1) iters where
  initbetas = Coefs (0, DV.fromList (replicate (length dpreds) 0)) 
  dpreds = preds dataset

{-- 3. Selects the best possible bi-variate linear model, in terms of 
    # R-squared in the training set-}
bivars :: DataSet -> DataSet -> Int -> Model
bivars origdataset dataset iters = fst (maxR bichoices) where
  initbetas = Coefs (0, DV.fromList (replicate 2 0)) 
  bimods = map (\bp -> (modReg (Model origdataset dataset bp initbetas 0 0.1) iters)) bipredlst
  r2s = [r2 mod | mod <- bimods]
  bichoices = zip bimods r2s
  bipredlst = getBis dpreds
  dpreds = preds dataset


{--4. Lists the best k-variable model chosen by a greedy elimination algorithm, for each 1 ≤ k ≤ |all pred variables| --}

kvars :: DataSet -> DataSet -> Int -> [(Int,  Model)]
kvars origdataset dataset iters = if length dpreds == 0 then [] else 
       [bestK origdataset dataset iters] ++ kvars origdataset (dataSet (snd (bestK origdataset dataset iters))) iters where
  dpreds = predvars $ snd (bestK origdataset dataset iters)

bestMods :: DataSet -> DataSet -> Int -> [(Int, Coefs, Float)]
bestMods origdataset dataset iters = [(fst mod, betas (snd mod), r2 (snd mod)) | mod <- modlist] where
  modlist = [(length (preds origdataset), allmod)] ++ kvars origdataset dataset iters
  allmod = allvars origdataset dataset iters

{--4. Lists the best k-variable model chosen by a greedy elimination algorithm, for each 1 ≤ k ≤ |all pred variables| --}

{-- Helper Funcs--}

bestK :: DataSet -> DataSet -> Int -> (Int,  Model)
bestK origdataset dataset iters = ((length (predvars (fst bestKmod))), fst bestKmod) where
  mods = map (\p -> (modReg (Model origdataset dataset p (Coefs (0, DV.fromList (replicate (length p) 0))) 0 0.1) iters)) predslst
  predslst =  subsets ((length dpreds) - 1) dpreds -- tail $ map (\k -> filter ( /= k) dpreds ) [0..(length dpreds)-1]
  dpreds = preds dataset
  rMods = map (\m -> (m, r2 m)) mods
  bestKmod = maxR $ rMods

subsets :: Int -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets k (x:xs) = map (x:) (subsets (k - 1) xs) ++ subsets k xs

sliceDataset :: DataSet -> [Int] -> DataSet
sliceDataset dataset indxs = DataSet (map (sliceObs indxs) obsList) indxs where
  dpreds = preds dataset
  obsList = observs dataset

sliceObs ::  [Int] -> Obs -> Obs
sliceObs indxs obs  = Obs (y, (DV.fromList xlst')) where
  Obs (y, x) = obs
  xlst = DV.toList x
  xlst' = [xlst !! i | i <- indxs]

getBis :: [a] -> [[a]]
getBis dpreds = [[x,y] | (x:ys) <- tails dpreds, y <- ys]


maxR :: (Ord a, Ord b) => [(a,b)] -> (a,b)
maxR l = swap $ maximum $ map swap l
             where swap (x, y) = (y, x) 


-- FIX: MODEL TYPE ARCHITECTURE SO THAT IT CONTAINS BOTH THE ORIGONAL DATASET ANSD THE SLICED ONE
