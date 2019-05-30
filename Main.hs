module MainReg2 where

import RTypes
import RegModels
import qualified Data.Vector as DV 

main :: IO ()
main = do
  let initbetas = Coefs (0, DV.fromList [0, 0, 0])
  let alpha = 0.1
  let r2init = 0
  let dpredvars = [0,1,2]
  let dataset = DataSet [Obs (2, DV.fromList [1 ,2, 4]), Obs (4, DV.fromList [2, 2, 2]), Obs (6, DV.fromList [3, 2, 1])] dpredvars -- write func to pick predvars by DV.elemIndex 
  let mpredvars = dpredvars
  let iterations = 500
  let mod0 = Model dataset mpredvars initbetas r2init alpha

  print $ show $ modReg mod0 iterations --adjustDeltas [1..3] [Obs (2, DV.fromList [1 ,2]), Obs (4, DV.fromList [2, 2]), Obs (6, DV.fromList [3, 2])]