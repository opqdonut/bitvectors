{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Dynamic where

import Util
import Encoding
import Tree

import Data.Maybe
import Control.Monad
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

dvToList = concatMap elems . toList . tree

instance Arbitrary DynamicVector where
    arbitrary = do xs <- listOf1 arbitrary
                   return $ dynamicVector (length xs) xs

chooseindex :: DynamicVector -> Gen Int
chooseindex dv = choose (0,pred . getSize . measure $ tree dv)
chooserank :: DynamicVector -> Gen Int
chooserank dv = choose (0,pred . getRank . measure $ tree dv)

query :: DynamicVector -> Int -> Bool
query dv i = block!(i-(s-blength dv))
    where Just ((SizeRank s r),block) = find (index i) (tree dv)
          
          
prop_query :: DynamicVector -> Gen Prop
prop_query dv = forAll (chooseindex dv) $
                  \i -> query dv i == dvToList dv !! i

queryrank :: DynamicVector -> Int -> Int
queryrank dv i = r - restrank
    where Just ((SizeRank s r),block) = find (index i) (tree dv)
          restrank = rank' $ drop (i-(s-blength dv)) $ elems block

prop_queryrank :: [Bool] -> Gen Prop
prop_queryrank xs = not (null xs) ==>
                    forAll (choose (0,length xs-1)) $
                    \i -> queryrank (dynamicVector (length xs) xs) i
                          == rank' (take i xs)

select :: DynamicVector -> Int -> Maybe Int
select dv i = do 
  (SizeRank s r, block) <- find (rank i) (tree dv)
  loc <- select' (i-(r-rank' (elems block))) (elems block) 
  return $ s-blength dv+loc

prop_select :: DynamicVector -> Gen Prop
prop_select dv = forAll (chooserank dv) $
                 \i -> select dv i == (select' i $ dvToList dv)
          