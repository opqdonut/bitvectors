{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}


module
  Measure
  (Measured(..),
   Monoid(..),
   SizeRank(..),
   Cached(..),
   cached,
   index,
   rank,
   (+++))     
  where

import BitVector
       
import Data.Monoid
import Data.FingerTree (Measured(..))
       
data SizeRank = SizeRank {getSize :: !Int,
                          getRank :: !Int}
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
  measure xs = mconcat (map measure xs)

instance Measured SizeRank Gap where
  measure (Gap gap) = SizeRank (gap+1) 1

instance Measured SizeRank [Gap] where
  -- the last gap has no final 1
  measure gs = let SizeRank s r = mconcat (map measure gs)
               in SizeRank (s-1) (r-1)


index :: Int->SizeRank->Bool
index i = (>i).getSize
rank :: Int->SizeRank->Bool
rank i = (>i).getRank

(+++) :: Monoid a => a -> a -> a
(+++) = mappend

data Cached a v = Cached {cmeasure :: !a, unCached :: !v}
instance Monoid a => Measured a (Cached a v) where
  measure = cmeasure
  
cached :: Measured a v => v -> Cached a v
cached x = Cached (measure x) x

instance (BitVector a, Measured v a) => BitVector (Cached v a) where
  deconstruct a = deconstruct (unCached a)
  
  querysize a = querysize (unCached a)
  query a i = query (unCached a) i
  queryrank a i = queryrank (unCached a) i
  select a i = select (unCached a) i

instance (Construct a, Measured v a) => Construct (Cached v a) where
  construct l xs = cached $ construct l xs