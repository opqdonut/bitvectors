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

mkMods v 0 = return []
mkMods v k = do
  v' <- randomInsert v
  fmap (v:) $ mkMods v' (k-1)

tst :: (DynamicBitVector a, BitVector a) => Int -> Int -> a -> IO ()
tst mods queries vec = do
  vecs <- mkMods vec mods
  replicateM_ queries $
    forM vecs $ randomQuery >=> print

main = do
  
  which:filename:mods':queries':_ <- getArgs

  let mods = read mods'
      queries = read queries'
      
  input <- bitsFromFile filename
  
  let 
    t :: (BitVector a, DynamicBitVector a) => a -> IO ()
    t = tst mods queries
    
  case which
    of --"d" -> t (construct' input :: Dynamic)
       --"sed" -> t (construct' input :: SmallEliasDynamic)
       "fdn" -> t (construct' input :: FDynamic NBlock)
       --"fun" -> t (construct' input :: FDynamic UBlock)
