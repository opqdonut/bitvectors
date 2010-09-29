{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Dynamic (dynamicVector,DynamicVector(),BitVector(..))
  where

import Util
import Encoding
import Tree
import BitVector
import Measure

import Data.Maybe
import Debug.Trace
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Property
import Data.Array.Unboxed (UArray,(!),bounds,elems)

type V = UArray Int

instance Measured SizeRank (UArray Int Bool) where
    measure arr = let xs = gap_decode $ elems arr in
                  SizeRank (length xs) (rank' xs)

data DynamicVector = DynamicVector {blength :: Int,
                                    tree :: Tree SizeRank (V Bool)}
                     deriving Show

instance BitVector DynamicVector where
    query = _query
    queryrank = _queryrank
    select = _select
    construct = dynamicVector

blockify :: Int -> [Bool] -> [Tree SizeRank (V Bool)]
blockify i xs = go 0 [] bs where
    bs = gap_encode' xs
    enc l = let dat = concat . reverse $ l
            in leaf . listArray' (length dat) $ dat
    go _   l [] = enc l : []
    go acc l (b:bs)
        | acc < i = go (acc + length b) (b:l) bs
        -- NB! the [True]: is an ugly hack
        | otherwise = 
            let lef = enc ([True]:l) in
            lef `seq` (lef : go 0 [] (b:bs))

dynamicVector :: Int -> [Bool] -> DynamicVector
dynamicVector n xs = DynamicVector blength (build blocks)
    where blength = 4 * ilog2 n
          blocks = blockify blength xs

dvToList = concatMap (gap_decode . elems) . toList . tree

prop_construct :: [Bool] -> Property
prop_construct xs = not (null xs) ==>
                    dvToList (dynamicVector (length xs) xs) == xs

prop_ann :: [Bool] -> Property
prop_ann xs = not (null xs) ==>
              annotation (tree (dynamicVector (length xs) xs))
                             == SizeRank (length xs) (rank' xs)

instance Arbitrary DynamicVector where
    arbitrary = do xs <- listOf1 arbitrary
                   return $ dynamicVector (length xs) xs

chooseindex :: DynamicVector -> Gen Int
chooseindex dv = choose (0,pred . getSize . measure $ tree dv)
chooserank :: DynamicVector -> Gen Int
chooserank dv = choose (0,pred . getRank . measure $ tree dv)

_query :: DynamicVector -> Int -> Bool
_query dv i = bits!!(i-(s-len))
    where Just ((SizeRank s r),block) = find (index i) (tree dv)
          bits = gap_decode $ elems block
          len = length bits -- NB! remove somehow...
          
          
prop_query :: DynamicVector -> Gen Prop
prop_query dv = forAll (chooseindex dv) $
                  \i -> query dv i == dvToList dv !! i

_queryrank :: DynamicVector -> Int -> Int
_queryrank dv i = r - restrank
    where Just ((SizeRank s r),block) = find (index i) (tree dv)
          bits = gap_decode $ elems block
          len = length bits
          restrank = rank' $ drop (i-(s-len)) $ bits

prop_queryrank :: [Bool] -> Gen Prop
prop_queryrank xs = not (null xs) ==>
                    forAll (choose (0,length xs-1)) $
                    \i -> queryrank (dynamicVector (length xs) xs) i
                          == rank' (take i xs)

_select :: DynamicVector -> Int -> Maybe Int
_select dv i = do 
  (SizeRank s r, block) <- find (rank i) (tree dv)
  let bits = gap_decode $ elems block
  let len = length bits
  loc <- select' (i-(r-rank' bits)) bits
  return $ s-len+loc

prop_select :: DynamicVector -> Gen Prop
prop_select dv = forAll (chooserank dv) $
                 \i -> select dv i == (select' i $ dvToList dv)
          