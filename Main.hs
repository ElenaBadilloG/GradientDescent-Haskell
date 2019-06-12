{-
       Copyright : (c) Elena Badillo Goicoechea, 06/11/19, 
       Contains I/O component of program, executing the following steps:

            1. Take args provided by the user [ i) json filepath, ii) no. of iterations, iii) task ]
            2. Read file and load data
            3. Parse data into appropriate data type
            4. Execute required task
            5. Print results
          
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}

module Main where

import RTypes
import RegModels as RM
import System.Environment 
import qualified Data.Vector as DV 
import Control.Applicative
import Control.Monad
import Text.JSON.Generic
import Data.List.Split (splitOn)
import Data.Char
import Data.List (sortBy)
import "regex-pcre" Text.Regex.PCRE


main = do
  [f, it, task, sc] <- getArgs
  fcontents  <- readFile f

  let iters = read it :: Int
  let scale = read sc :: Float
  let processed = processFile fcontents scale -- tuple: (no. predictors, dataset)
  let dataset = snd processed
  let initbetas = Coefs (0, DV.fromList (replicate (fst processed ) 0))
  let alpha = 0.1
  let r2init = 0
  let origdataset = dataset

  doTask task origdataset dataset iters   

{-- Cleans and parses content from a read file --}
processFile :: String -> Float -> (Int, DataSet (Record Float) )
processFile conts scale = (len, DataSet (map (lst2rec) doublst) dpredvars) where

  doublst = init (map (\xs -> [read x :: Float | x <- xs]) numlst)
  numlst = map concat (map (map line2lst) nums')
  dpredvars = [0..(length (last doublst) - 2)]
  nums' = map (splitOn "    ") nums
  nums =  map getNums recs
  recs = map processLine splitt
  splitt = splitOn "}" clean
  clean = removeChars ",\n.?!-;\"\'" conts
  len = (length (last doublst) - 2)

{-- Performs the algorithm specified in the users input args and prints reuslts to screen --}
doTask :: String -> DataSet (Record Float) -> DataSet (Record Float) -> Int -> IO()
doTask task origdataset dataset iters
  | task == "univar" = putStrLn $ "\n" ++ "BEST UNIVARIATE LINEAR MODEL: " ++ "\n \n" ++ (show $ RM.univars origdataset dataset iters) 
  | task == "allvar" = putStrLn $ "\n" ++ "FULL LINEAR MODEL: " ++ "\n \n" ++ (show $ RM.allvars origdataset dataset iters) 
  | task == "bivar" = putStrLn $ "\n" ++ "BEST BIVARIATE LINEAR MODEL: " ++ "\n \n" ++ (show $ RM.bivars origdataset dataset iters) 
  | task == "bestK" =  putStrLn $ "\n" ++ "BEST K-LENGTH LINEAR MODEL FOR EACH K = 1,..., |ALL PREDICTORS|: " ++ "\n \n" ++ (show $ RM.bestMods origdataset dataset iters)
  | otherwise = putStrLn $ "\n" ++ "BEST MODEL: " ++ "\n \n" ++ (show $ bestM origdataset dataset iters) 

{-- Remove preddefined characters from string --}
removeChars :: String -> String -> String
removeChars chars xs = [ x | x <- xs, not (x `elem` chars ) ]

{-- Remove letter and punctuation characters --}
getNums xs = map (\x -> if not (x `elem` ['a'..'z']++[':', ',']++['A'..'Z']) then x else ' ') xs

{-- Parses a line into readable form --}
processLine :: String -> String
processLine r1 = "{" ++ (removeChars "[] " (show r3)) ++ "}" where

  r2 = removeChars ",{}[[]\n.?!-;\'\'" r1
  r3 = splitOn "   " r2 

{-- Converts a string to a list of all the integers contained in it --}
line2lst :: String -> [String]
line2lst xs = let stringResult = xs =~ "[0-9]+" :: AllTextMatches [] String in
              getAllTextMatches stringResult

{-- Converts a list of integers into a Record data type --}
lst2rec :: [Float]-> Record Float
lst2rec ds = Record (y, x) where
  y = last ds
  x = DV.fromList (init ds)

  