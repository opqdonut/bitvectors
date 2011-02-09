module Main where

import qualified Data.ByteString as B
import System.Environment
import Util
import System.Random
import Data.Word
import Control.Monad
import System.IO

generate :: [Bool] -> [Word8]
generate [] = []
generate bools =
  let (byte,rest) = splitAt 8 bools
      b = fromIntegral $ unbitify byte
  in b:generate rest
  
    

main = do
  
  [file,size',f',seed'] <- getArgs
  
  let size = read size' :: Int
      f = read f' :: Int
      seed = read seed' :: Int
      r = randomRs (0,f-1) (mkStdGen seed)
  
  h <- openFile file WriteMode
  
  mapM_ (B.hPut h . B.pack) . cut 512 . generate . map (==0) . take (size*1024) $ r
  
  hClose h
  