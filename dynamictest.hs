module Main where

import Util
import Dynamic

import System.Environment (getArgs)
import Data.Array.Unboxed
import Random
import Control.Monad

main = do

  n:k:_ <- (fmap.fmap) read $ getArgs

  let test = dynamicVector n $ take n (cycle [True,False,False,False,False])
  print $ query test 0

  --let bits = sum $ map (last . elems . blocklocations) $ elems $ supers test
  --putStrLn $ "bits: " ++ show bits ++ " (" ++ show (bits///n) ++ ")"

  inds <- sequence $ replicate k (randomRIO (0,n-1))

  forM_ inds $ \x -> print (x,query test x,queryrank test x)

