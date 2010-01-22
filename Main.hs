module Main where

import Static
import System.Environment (getArgs)
import Data.Array.Unboxed

import Random
import Control.Monad

main = do

  n:k:_ <- (fmap.fmap) read $ getArgs

  let test = staticVector n $ take n (cycle [True,False,False,False])

  print $ query test 0

  inds <- sequence $ replicate k (randomRIO (0,n-1))

  forM_ inds $ \x -> print (x,query test x,queryrank test x)

