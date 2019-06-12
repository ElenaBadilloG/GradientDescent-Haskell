{-
       File      :      DoLinReg.hs
       Copyright : (c) Elena Badillo Goicoechea, 06/11/19, 
       Contains a purely functional programming implementation of multivariate linear model 
       estimation using gradient descent.
-}

module DoLinReg where

import Data.List
import qualified Data.Vector as DV 
import RTypes

-- Finds coefficients for linear regression using gradient descent
linearReg :: Coefs -> Float -> DataSet (Record Float) -> Int -> Coefs
linearReg coefs alpha dataset iterations
  | iterations == 0 = coefs
  | otherwise =
    let betas = newBetas coefs alpha dataset
     in linearReg betas alpha dataset (iterations - 1)

-- Calculate new values for regression coefficients (Backward, Forward Step)
newBetas :: Coefs -> Float -> DataSet (Record Float) -> Coefs
newBetas betas lrate dataset =
  let deltas = map (getbDelta betas) observs
      beta1Deltas = predDeltas deltas observs
      newt0 = t0 - lrate * (avg deltas)
      newt1 = newcoef lrate beta1Deltas t1'
      t1' = DV.toList t1
   in Coefs (newt0, DV.fromList newt1)
  where
    Coefs (t0, t1) = betas
    DataSet observs preds = dataset

{-- Updates regression coefficients --}
newcoef :: Float -> [Float] -> [Float] -> [Float]
newcoef lrate predDeltas c = zipWith (-) c (map (*lrate) predDeltas)

-- Calculates the difference between h(x) and y: intercept delta
getbDelta :: Coefs -> Record Float -> Float
getbDelta betas observ = (beta0 + (vdot betas1 x))  - y
  where
    Coefs (beta0, betas1) = betas
    -- Coefs v
    Record (y, x) = observ

-- Calculates the partials for each beta1 
predDeltas :: [Float] -> [Record Float] -> [Float]
predDeltas deltas observs =
  let xs = map (\(Record (_, x)) -> x) observs
      xs' = map (DV.toList) xs
      zipped = zip deltas xs' --zipped = zip deltas xs
   in aggreg (map delTup zipped)

{-- Computes adjusted r-squared statitsic --}
adjRsq :: Coefs -> DataSet (Record Float) -> Float
adjRsq betas dataset = r2 - (1 - r2) * (k / (n - k - 1)) where

  yfit = map (\x -> beta0 + (vdot betas1 x)) xs
  diff = map (^2) $ zipWith (-) ys yfit
  var_diff = avg diff
  var_y = avg y'
  y' = map (^2) $ zipWith (-) ys (replicate (length ys) (avg ys))
  xs =  map (\(Record (_, x)) -> x) observs
  ys =  map (\(Record (y, _)) -> y) observs
  n = genericLength ys
  k = genericLength  (DV.toList betas1)
  r2 = 1-(var_diff/var_y)

  DataSet observs preds = dataset
  Coefs (beta0, betas1) = betas

{-- Helper Funcs--}

{--Dot product--}
vdot :: DV.Vector Float -> DV.Vector Float -> Float
vdot a b | length a' == length b' = sum (zipWith (*) a' b')
         | otherwise = error "Vector sizes must match" where
          a' = DV.toList a
          b' = DV.toList b

avg xs = fromRational $ (realToFrac (sum xs) / genericLength xs)

delTup :: (Float, [Float]) -> [Float]
delTup tup = map (* x) lst where
    x = fst tup 
    lst = snd tup

aggreg :: [[Float]] -> [Float]
aggreg lst = map (\i -> takeav lst i) [0..(length (head lst)-1)] where
    takeav xl i = avg [xs !! i | xs <- xl]