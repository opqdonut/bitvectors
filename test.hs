module Main where

import Util
import Static
import Dynamic

import System.Environment (getArgs)
import Data.Array.Unboxed
import Random
import Control.Monad

main = do

  s:n':k':_ <- getArgs

  let n = read n'
      k = read k'

  let input = take n (cycle [True,False,False,False,False,False,False])

  print (s,n,k)

  case s of "s" -> test (staticVector n input) n k
            "d" -> test (dynamicVector n input) n k


test t n k = do
  --putStrLn $ "blength,slength: " ++ show (blength test, slength test)

  print $ query t 0

  --let bits = sum $ map (last . elems . blocklocations) $ elems $ supers test
  --putStrLn $ "bits: " ++ show bits ++ " (" ++ show (bits///n) ++ ")"

  let go :: Int -> IO ()
      go 0 = return ()
      go n = {-# SCC "go" #-}
             do x <- {-# SCC "random" #-} randomRIO (0,n-1)
                {-# SCC "print" #-} print (x,query t x,queryrank t x)
                go (n-1)

  go k

