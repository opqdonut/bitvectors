module Main where

import Util
import Static
import FingerTreeDynamic 
import Encoding2 (EBlock,UBlock,NBlock)
import Static2
import Tree (Dynamic,SmallDynamic,SmallEliasDynamic)

import SmallBlock

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
            "s2" -> test (mkStatic n input) n k
            "fd" -> test (construct n input :: FDynamic EBlock) n k
            "fdn"-> test (construct n input :: FDynamic NBlock) n k
            "fun"-> test (construct n input :: FDynamic UBlock) n k
            "fs" -> test (construct n input :: FDynamic SmallBlock) n k
            "fse" -> test (construct n input :: FDynamic SmallElias) n k
            "d"  -> test (construct n input :: Dynamic) n k
            "sd" -> test (construct n input :: SmallDynamic) n k
            "sed"-> test (construct n input :: SmallEliasDynamic) n k



test t n k = do
  --putStrLn $ "blength,slength: " ++ show (blength test, slength test)

  print $ query t 0

  --let bits = sum $ map (last . elems . blocklocations) $ elems $ supers test
  --putStrLn $ "bits: " ++ show bits ++ " (" ++ show (bits///n) ++ ")"

  let go :: Int -> Int -> IO ()
      go x 0 = return ()
      go x i = {-# SCC "go" #-}
        do {-# SCC "go_x" #-}     putStr (show x)
           {-# SCC "go_query" #-} if query t x
                                    then putStr "T"
                                    else putStr "F"
           {-# SCC "go_rank" #-}  putStr (show $ queryrank t x)
           putStr " "
           go (x*17`mod`n) (i-1)

  go 1 k

