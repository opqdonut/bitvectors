module Main where

import Testing
import Util
import BitVector
import Encoding2
import FingerTreeDynamic

import System.Environment (getArgs)

main = do
  filename:bs':queries':[] <- getArgs
  
  let bs = read bs'
      queries = read queries'
      
  input <- bitsFromFile filename
      
  test (constructWithBlockSize bs input :: FDynamic EBlock)
    (length input) queries
