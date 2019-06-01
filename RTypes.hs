{-
       File      :      RTypes.hs
       Copyright : (c) Elena Badillo Goicoechea, 05/30/19, 
       Contains ...
       References (concepts and basic implementation):
          - https://samcgardner.github.io/2018/10/06/linear-regression-in-haskell.html
          - http://mccormickml.com/2014/03/04/gradient-descent-derivation/
          - https://www.classes.cs.uchicago.edu/archive/2018/fall/12100-1/pa/pa5/index.html
-}


module RTypes where

import qualified Data.Vector as DV 

--newtype Coefs = Coefs (Float, Float) deriving (Show)
newtype Coefs = Coefs (Float, DV.Vector Float) deriving (Eq, Show, Ord)

--newtype Obs = Obs (Float, Float) deriving (Show)
newtype Obs = Obs (Float, DV.Vector Float) deriving (Eq, Show, Ord)

data DataSet  = DataSet {observs :: [Obs], preds :: [Int]} deriving (Eq, Show, Ord)

--newtype TrainingSet = TrainingSet [Obs] deriving (Eq, Show)

data Model = Model {origdataSet :: DataSet, dataSet :: DataSet, predvars :: [Int], betas :: Coefs, r2 :: Float, lrate :: Float} deriving (Eq, Show, Ord) -- predvars


  {-- 1. Redefine types X
  	  2. Make them consistent with other modules X
  	  3. Try everything X
  	  4. Generalize obs to multivar using vectors X
  	  5. R2 X
  	  6. Data Model type def and methods X
  	  7. Implement tasks from PA5 X 
  	  8. Test set division
  	  9. IO: from xls to Training Set}