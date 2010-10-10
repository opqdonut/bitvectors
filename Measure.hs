{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}


module
  Measure
  (Measured(..),
   Monoid(..),
   SizeRank(..),
   index,
   rank,
   (+++))     
  where

import BitVector
       
import Data.Monoid
import Data.FingerTree (Measured(..))
       
data SizeRank = SizeRank {getSize :: {-# UNPACK #-} !Int,
                          getRank :: {-# UNPACK #-} !Int}
    deriving (Show,Eq)

instance Monoid SizeRank where
    {-# SPECIALIZE instance Monoid SizeRank #-}
    mappend (SizeRank a a') (SizeRank b b') =
        SizeRank (a+b) (a'+b')
    mempty = SizeRank 0 0
instance Measured SizeRank Bool where
    measure True  = SizeRank 1 1
    measure False = SizeRank 1 0
instance Measured SizeRank [Bool] where
  measure xs = SizeRank (length xs) (rank' xs)


index :: Int->SizeRank->Bool
index i = (>i).getSize
rank :: Int->SizeRank->Bool
rank i = (>i).getRank

(+++) :: Monoid a => a -> a -> a
(+++) = mappend
