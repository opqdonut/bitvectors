module Main where

import Util
import Static
import FingerTreeDynamic 

import Prelude 

import System.Environment (getArgs)
import Data.Array.Unboxed
import Random
import Control.Monad

gen n = cycle $ True : replicate (n-1) False

gen2 n k = map f . take n $ randoms (mkStdGen 0)
  where 
    f :: Int -> Bool
    f a = a `mod` k == 0
  


main = do

  s:n':k':_ <- getArgs

  let n = read n'
      k = read k'

  --let input = take n $ gen2 37
  let input0 = gen2 1024 37
  let input = take n $ cycle input0
  
  print (s,n,k)

  case s of --"so" -> test (staticVector_ord n input) n k
            "sg" -> test (staticVector_gap n input) n k
            "fd" -> test (fDynamic n input) n k


test t n k = do
  --putStrLn $ "blength,slength: " ++ show (blength test, slength test)

  print $ query t 0

  --let bits = sum $ map (last . elems . blocklocations) $ elems $ supers test
  --putStrLn $ "bits: " ++ show bits ++ " (" ++ show (bits///n) ++ ")"

  let go :: Int -> IO ()
      go 0 = return ()
      go i = {-# SCC "go" #-}
             do x <- {-# SCC "random" #-} randomRIO (0,n-1)
                {-# SCC "print" #-} print (x,query t x,queryrank t x)
                go (i-1)

  go k

