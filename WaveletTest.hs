module Main where

import Wavelet

import System.Environment (getArgs)
import System.IO
import System.Random
import Control.Monad

randomInsert wav = do
  i <- randomRIO (0,wlength wav - 1)
  si <- randomRIO (0,length (symbols wav) - 1)
  let s = symbols wav !! si
  return $ winsert wav s i

randomQuery wav = do
  i <- randomRIO (0,wlength wav - 1)
  return (i, wread wav i)

mkMods wav 0 = return [wav]
mkMods wav k = do
  wav' <- randomInsert wav
  fmap (wav':) $ mkMods wav' (k-1)

keep k [] = []
keep k (x:xs) = x:keep k (drop (k-1) xs)
                          
                          
main = do
  
  filename:mods':queries':store':_ <- getArgs

  let mods = read mods'
      queries = read queries'
      store = read store'
      
  dat <- readFile filename
  
  let base = mkWavelet'fd dat
      
  wavs <- fmap (keep store) $ mkMods base mods
  
  replicateM_ queries $
    forM wavs $ randomQuery >=> print
    
    