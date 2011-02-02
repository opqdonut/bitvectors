module Main where 

import qualified Encoding as E
import qualified Encoding2 as E2

import Util

import Data.Word
import Data.Array.Unboxed

import Random

import System.Environment
import System.Time

import Control.Parallel.Strategies

test :: [Bool] -> ()
test xs = let encoded = E.gap_encode xs
              block :: UArray Int Bool
              block = listArray' (length encoded) encoded
              out = E.gap_decode $ elems block
          in last out `seq` ()
              
              
test2 :: [Bool] -> ()
test2 xs = let out = E2.unGapBlock (E2.gapBlock xs)
           in last out `seq` ()
              
test3 :: [Bool] -> ()
test3 xs = let out = E2.unNibbleBlock (E2.nibbleBlock xs)
           in last out `seq` ()

test4 :: [Bool] -> ()
test4 xs = let block :: E2.UBlock
               block = E2.encode xs
           in last (E2.decode block) `seq` ()

gen2 n k = map f . take n $ randoms (mkStdGen 0)
  where 
    f :: Int -> Bool
    f a = a `mod` k == 0


diff start end =
  let timediff = diffClockTimes end start
      secondsfloat = realToFrac(tdSec timediff)
                     + realToFrac(tdPicosec timediff) / 1000000000000
  in print secondsfloat

main = do
  n':k':_ <- getArgs
  let n = read n'
      k = read k'
      input0 = gen2 n k `using` rdeepseq
      density = length (filter id input0)///length input0
  
  print density
      
  start <- getClockTime
  print (test input0)
  end <- getClockTime
  diff start end
  
  start2 <- getClockTime
  print (test2 input0)
  end2 <- getClockTime
  diff start2 end2
  
  start3 <- getClockTime
  print (test3 input0)
  end3 <- getClockTime
  diff start3 end3

  start4 <- getClockTime
  print (test4 input0)
  end4 <- getClockTime
  diff start4 end4

  