module Main where

import Static
import System.Environment (getArgs)
import Data.Array.Unboxed

main = do

  n:_ <- (fmap.fmap) read $ getArgs

  let test = staticVector n $ take n (cycle [True,False,False,False])

  print $ query test 0
  print $ (slength test, blength test)
  print $ address test (n-1)

  print $ queryrank test (n-1)

