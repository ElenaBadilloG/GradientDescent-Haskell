module Main where

import RTypes
import RegModels
import qualified Data.Vector as DV 

main :: IO ()
main = do
  let initbetas = Coefs (0, DV.fromList [0, 0, 0, 0])
  let alpha = 0.3
  let r2init = 0
  let dpredvars = [0,1,2,3]
  let dataset = DataSet [Obs (2, DV.fromList [1 ,2, 4, 1]), Obs (4, DV.fromList [2, 3, 2, 1]), Obs (6, DV.fromList [3, 2, 4, 1])] dpredvars 
  let origdataset = dataset
  let mpredvars = dpredvars
  let iters = 500
  let mod0 = Model origdataset dataset mpredvars initbetas r2init alpha

  print $ "BEST K-LENGTH LINEAR MODEL FOR EACH K = 1,..., |ALL PREDICTORS| : " ++ (show $ bestMods origdataset dataset iters) ---modReg mod0 iterations --adjustDeltas [1..3] [Obs (2, DV.fromList [1 ,2]), Obs (4, DV.fromList [2, 2]), Obs (6, DV.fromList [3, 2])]