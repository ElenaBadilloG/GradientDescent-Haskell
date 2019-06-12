{-
       File      :      RegModels.hs
       Copyright : (c) Elena Badillo Goicoechea, 06/11/19, 
       Contains greedy and non-greedy implemetations of linear model selection algorithms
-}

module RegModels where

import Data.List
import qualified Data.Vector as DV 
import RTypes
import DoLinReg
import Data.List (sortBy)
import Data.Function (on)

-- Trains a linear model
modReg :: Model (Record Float) -> Int -> Model (Record Float)
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
univars :: DataSet (Record Float) -> DataSet (Record Float)-> Int -> [(Maybe Int, Coefs, Float, [Int])]
univars origdataset dataset iters = [(elemIndex mod mods, betas mod, r2 mod, predvars mod)| mod <- mods] where
  mods = [modReg (Model origdataset dataset [p] initbetas 0 0.1) iters| p <- dpreds]
  dpreds = preds dataset 
  initbetas = Coefs (0, DV.fromList [0]) 

{-- 2. Trains a linear model including all possible regressors in the dataset
    return model_all--}
allvars :: DataSet (Record Float)-> DataSet (Record Float)  -> Int -> (Int, Coefs, Float, [Int])
allvars origdataset dataset iters = (length dpreds, betas mod, r2 mod, predvars mod) where
  mod = modReg (Model origdataset dataset dpreds initbetas 0 0.1) iters 
  initbetas = Coefs (0, DV.fromList (replicate (length dpreds) 0)) 
  dpreds = preds dataset

{-- 3. Selects the best possible bi-variate linear model, in terms of 
    # R-squared in the training set-}
bivars :: DataSet (Record Float) -> DataSet (Record Float)-> Int -> (Coefs, Float, [Int])
bivars origdataset dataset iters = (betas bmod, r2 bmod, predvars bmod) where
  bmod = fst (maxR bichoices)
  initbetas = Coefs (0, DV.fromList (replicate 2 0)) 
  bimods = map (\bp -> (modReg (Model origdataset dataset bp initbetas 0 0.1) iters)) bipredlst
  r2s = [r2 mod | mod <- bimods]
  bichoices = zip bimods r2s
  bipredlst = getBis dpreds
  dpreds = preds dataset


{--4. Lists the best k-variable model chosen by a greedy elimination algorithm, for each 1 ≤ k ≤ |all pred variables| --}
bestMods :: DataSet (Record Float) -> DataSet (Record Float) -> Int -> [(Int, Coefs, Float, [Int])]
bestMods origdataset dataset iters = [(fst mod, betas (snd mod), r2 (snd mod), predvars (snd mod)) | mod <- modlist] where
  modlist = kvars origdataset dataset iters --[allmod] ++s
  allmod = allvars origdataset dataset iters

{--5. Selects the best model (in terms of adjusted R2) from all model combinations--}
bestM :: DataSet (Record Float) -> DataSet (Record Float) -> Int -> (Int, Coefs, Float, [Int])
bestM origdataset dataset iters = last (sortM (bestMods origdataset dataset iters))



{-- Helper Funcs--}

{-- Sort (ascending) a list of 4-sized tuples according to the third value --}
sortM :: (Ord c) => [(a, b, c, d)] -> [(a, b, c, d)]
sortM t = sortBy (compare `on` (\(a,b,c,d)->c)) t

{-- Estimates a linear regression model for a list of given predictor combinations and chooses the best combination--}
bestK :: DataSet (Record Float) -> DataSet (Record Float)  -> Int -> (Int,  Model (Record Float))
bestK origdataset dataset iters = ((length (predvars (fst bestKmod))), fst bestKmod) where
  mods = map (\p -> (modReg (Model origdataset dataset p (Coefs (0, DV.fromList (replicate (length p) 0))) 0 0.1) iters)) predslst
  predslst =  subsets ((length dpreds) - 1) dpreds 
  dpreds = preds dataset
  rMods = map (\m -> (m, r2 m)) mods
  bestKmod = maxR $ rMods

kvars :: DataSet (Record Float) -> DataSet (Record Float) -> Int -> [(Int,  Model (Record Float))]
kvars origdataset dataset iters = if length dpreds == 0 then [] else 
       [bestK origdataset dataset iters] ++ kvars origdataset (dataSet (snd (bestK origdataset dataset iters))) iters where
  dpreds = predvars $ snd (bestK origdataset dataset iters)

{--Lists all subsets of a set--}
subsets :: Int -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets k (x:xs) = map (x:) (subsets (k - 1) xs) ++ subsets k xs

{-- Slices a dataset according to a provided list of indeces --}
sliceDataset :: DataSet (Record Float) -> [Int] -> DataSet (Record Float)
sliceDataset dataset indxs = DataSet (map (sliceObs indxs) obsList) indxs where
  dpreds = preds dataset
  obsList = observs dataset

{-- Slices a record according to a provided list of indeces --}
sliceObs ::  [Int] -> Record Float -> Record Float 
sliceObs indxs obs  = Record (y, (DV.fromList xlst')) where
  Record (y, x) = obs
  xlst = DV.toList x
  xlst' = [xlst !! i | i <- indxs]

{-- Gets all the 2-sized sets from a given list--}
getBis :: [a] -> [[a]]
getBis dpreds = [[x,y] | (x:ys) <- tails dpreds, y <- ys]

{--Choose the maximum tuple, according to the second compoenent--}
maxR :: (Ord a, Ord b) => [(a,b)] -> (a,b)
maxR l = swap $ maximum $ map swap l
             where swap (x, y) = (y, x) 
