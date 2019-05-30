module RTypes where

import qualified Data.Vector as DV 

--newtype Coefs = Coefs (Float, Float) deriving (Show)
newtype Coefs = Coefs (Float, DV.Vector Float) deriving (Eq, Show)

--newtype Obs = Obs (Float, Float) deriving (Show)
newtype Obs = Obs (Float, DV.Vector Float) deriving (Eq, Show)

data DataSet  = DataSet {observs :: [Obs], preds :: [Int]} deriving (Eq, Show)

--newtype TrainingSet = TrainingSet [Obs] deriving (Eq, Show)

data Model = Model {dataSet :: DataSet, predvars :: [Int], betas :: Coefs, r2 :: Float, lrate :: Float} deriving (Eq, Show) -- predvars


  {-- 1. Redefine types X
  	  2. Make them consistent with other modules X
  	  3. Try everything X
  	  4. Generalize obs to multivar using vectors X
  	  5. R2 X
  	  6. Data Model type def and methods
  	  7. Test set division
  	  5. IO: from xls to Training Set
  	  5. Implement tasks from PA5 --}