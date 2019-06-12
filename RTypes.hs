{-
       File      :      RTypes.hs
       Copyright : (c) Elena Badillo Goicoechea, 06/11/19, 
       Contains type signatures for Coefs, Record, DataSet, and Model
-}


module RTypes where

import qualified Data.Vector as DV 

newtype Coefs = Coefs (Float, DV.Vector Float) deriving (Eq, Show, Ord)
newtype Record a = Record (Float, DV.Vector a) deriving (Eq, Show, Ord)
data DataSet a = DataSet {observs :: [a], preds :: [Int]} deriving (Eq, Show, Ord)
data Model a = Model {origdataSet :: DataSet a, dataSet :: DataSet a, predvars :: [Int], betas :: Coefs, r2 :: Float, lrate :: Float} deriving (Eq, Show, Ord) 

instance (Num a) => Monoid (Record a) where
  mempty = Record (0, DV.fromList [])

instance (Num a) => Semigroup (Record a) where 
    (<>) (Record (a1, v1)) (Record (a2, v2)) = Record (a1*a2, (DV.zipWith (*) v1 v2))

instance Functor (Record) where
  fmap f (Record (a, v1)) = Record (a, (DV.map f v1))

instance Functor DataSet where
  fmap f (DataSet obs p) = DataSet (map f obs) p
