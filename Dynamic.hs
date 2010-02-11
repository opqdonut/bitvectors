{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Dynamic where

import Util
import Encoding
import Tree

import Test.QuickCheck
import Test.QuickCheck.Property
import Data.Array.Unboxed (UArray,(!),bounds,elems)

type V = UArray Int

instance Measured SizeRank (UArray Int Bool) where
    measure arr = SizeRank (snd (bounds arr) +1) (count id $ elems arr)

data DynamicVector = DynamicVector {blength :: Int,
                                    tree :: Tree SizeRank (V Bool)}
                     deriving Show

dynamicVector :: Int -> [Bool] -> DynamicVector
dynamicVector n xs = DynamicVector blength (mkbal blocks)
    where blength = ilog2 n
          blocks = map (listArray' blength . pad blength)
                   $ cut blength xs

query :: DynamicVector -> Int -> Bool
query dv i = block!(i-(s-blength dv))
    where Just ((SizeRank s r),block) = find (index i) (tree dv)
          
          
prop_query :: [Bool] -> Gen Prop
prop_query xs = not (null xs) ==>
                forAll (choose (0,length xs-1)) $
                \i -> query (dynamicVector (length xs) xs) i == xs !! i



                   