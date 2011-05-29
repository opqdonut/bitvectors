module Main where

import FingerTreeDynamic
import Tree
import Encoding2 (EBlock,UBlock,NBlock)
import BitVector
import Util

import System.Environment (getArgs)
import System.IO
import System.Random
import Control.Monad

randomInsert v = do
  i <- randomRIO (0,querysize v - 1)
  b <- randomRIO (False,True)
  return $ insert v i b

randomQuery v = do
  i <- randomRIO (0,querysize v - 1)
  return (i, query v i, queryrank v i)

randomIQ v = do
  v' <- randomInsert v
  randomQuery v'

mkMods v 0 = return []
mkMods v k = do
  v' <- randomInsert v
  fmap (v:) $ mkMods v' (k-1)

tst :: (DynamicBitVector a, BitVector a) => Int -> a -> IO ()
tst queries vec =
  replicateM_ queries $
    randomIQ vec >>= print

main = do
  
  which:filename:queries':_ <- getArgs

  let queries = read queries'
      
  input <- bitsFromFile filename
  
  let 
    t :: (BitVector a, DynamicBitVector a) => a -> IO ()
    t = tst queries
    
  case which
    of "d" -> t (construct' input :: Dynamic EBlock)
       "dn" -> t (construct' input :: Dynamic NBlock)
       --"sed" -> t (construct' input :: SmallEliasDynamic)
       "fdn" -> t (construct' input :: FDynamic NBlock)
       "fd" -> t (construct' input :: FDynamic EBlock)
       --"fun" -> t (construct' input :: FDynamic UBlock)
