{-# LANGUAGE MultiParamTypeClasses #-}

module FingerTreeDynamic where

import Measure
import Encoding
import Util
import BitVector

import Test.QuickCheck
import Test.QuickCheck.Property

import Prelude hiding (reverse,null)
import Data.FingerTree
import Data.Array.Unboxed (UArray,(!),bounds,elems)

newtype Block = Block (UArray Int Bool)
  deriving Show

open :: Block -> [Bool]
open (Block b) = gap_decode $ elems b

instance Measured SizeRank Block where
    measure b =
      let xs = open b
      in SizeRank (length xs) (rank' xs)

newtype FDynamic = FDynamic {unwrap :: (FingerTree SizeRank Block)}

{-
instance Show FDynamic where
  show f = "(FDynamic XX " ++ show (ftoList f) ++ ")"
-}

instance BitVector FDynamic where
  query = _query
  queryrank = _queryrank
  select = _select
  construct = fDynamic


build :: Int -> [Bool] -> FDynamic
build size xs = FDynamic (fromList . map make $ blocks)
  where blocks = cut size xs
        make xs = let encoded = gap_encode xs
                  in Block (listArray' (length encoded) encoded)
        
fDynamic :: Int -> [Bool] -> FDynamic
fDynamic n xs = build blocksize xs
  where blocksize = 4 * ilog2 n 
        
fingerTreeToList :: Measured v a => FingerTree v a -> [a]
fingerTreeToList f
  | null f    = []
  | otherwise = let (a:<as) = viewl f
                in a : fingerTreeToList as

ftoList :: FDynamic -> [Bool]
ftoList (FDynamic f) = concatMap open $ fingerTreeToList f

blocks (FDynamic f) = map open $ fingerTreeToList f

prop_build size dat = (size>0) ==> out == dat
  where out = ftoList $ build size dat
        
find :: FDynamic -> (SizeRank->Bool) -> Maybe (SizeRank,Block)
find (FDynamic f) p =
  let (before,after) = split p f
  in case viewl after of      
    elem :< _ -> Just (measure before, elem)
    EmptyL    -> Nothing
     

_query :: FDynamic -> Int -> Bool
_query f i = query bits i'
  where Just (SizeRank s r, block) = find f (index i)
        i' = i-s
        bits = open block
      
prop_query f =
  forAll (chooseIndex f) $
  \i -> let dat = ftoList f
        in _query f i == query dat i
           
_queryrank :: FDynamic -> Int -> Int
_queryrank f i = r + queryrank bits i'
  where Just (SizeRank s r, block) = find f (index i)
        i' = i-s
        bits = open block
        
prop_queryrank f =
  forAll (chooseIndex f) $
  \i -> let dat = ftoList f
        in _queryrank f i == queryrank dat i
  
_select :: FDynamic -> Int -> Maybe Int
_select f i = do
  (SizeRank s r, block) <- find f (rank i)
  let bits = open block
  fmap (+s) $ select bits (i-r)
  
prop_select f =
  forAll (chooseIndex f) $
  \i -> let dat = ftoList f
        in _select f i == select dat i


-- TEST INFRA
        
instance Arbitrary FDynamic where
  arbitrary = do xs <- listOf1 arbitrary
                 siz <- choose (1,length xs)
                 return $ build siz xs

chooseIndex :: FDynamic -> Gen Int
chooseIndex f = 
  let (SizeRank s _) = measure (unwrap f)
  in choose (0,s-1)
     
chooseRank :: FDynamic -> Gen Int
chooseRank f = 
  let (SizeRank _ r) = measure (unwrap f)
  in choose (0,r-1)