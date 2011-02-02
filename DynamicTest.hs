module Main where

import BitVector
import Encoding2
import FingerTreeDynamic

import System.Environment (getArgs)
import Random
import Control.Monad

dat = [True,False,True,False]

go 0 _ _ = return ()
go k n f = do i <- randomRIO (0,n-1)
              v <- randomRIO (False,True)
              print (k,n,i,v)
              let f' = insert f i v
              unless (v == query f' i) $
                fail ("didn't match!: " ++ show f)
              go (k-1) (n+1) f'

main = do
  
  (n:nIns:[]) <- fmap (map read) getArgs
  
  let f :: FDynamic EBlock
      f = fDynamic n dat 
      
  go nIns (length dat) f
  
  
      
