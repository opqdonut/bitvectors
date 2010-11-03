module Main where 

import qualified Encoding as E
import qualified Encoding2 as E2

import Util

import Data.Word
import Data.Array.Unboxed

import System.Environment
import System.Time

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

gen n = cycle $ True : replicate (n-1) False

diff start end =
  let timediff = diffClockTimes end start
      secondsfloat = realToFrac(tdSec timediff)
                     + realToFrac(tdPicosec timediff) / 1000000000000
  in print secondsfloat

main = do
  n':k':_ <- getArgs
  let n = read n'
      k = read k'
      input0 = gen
      
  start <- getClockTime
  let input = take n $ gen k
    in print (test input)
  end <- getClockTime
  diff start end
  
  start2 <- getClockTime
  let input = take n $ gen k
    in print (test2 input)
  end2 <- getClockTime
  diff start2 end2
  
  start3 <- getClockTime
  let input = take n $ gen k
    in print (test3 input)
  end3 <- getClockTime
  diff start3 end3

  