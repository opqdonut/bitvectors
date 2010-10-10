{-# LANGUAGE MultiParamTypeClasses #-}

module FingerTreeDynamic where

import Measure
import Encoding2
import Util
import BitVector

import Debug.Trace
import Test.QuickCheck hiding ((><))
import Test.QuickCheck.Property

import Prelude hiding (reverse,null)
import Data.FingerTree
import Data.Array.Unboxed (UArray,(!),bounds,elems)

instance Measured SizeRank Block where
    measure b =
      let is = blockGaps b
      in SizeRank (sum is + length is - 1) (length is - 1)

data FDynamic = FDynamic 
                {blocksize :: Int,
                 unwrap :: (FingerTree SizeRank Block)}


instance Show FDynamic where
  show f = "(FDynamic " ++ show (blocksize f) ++ " " ++ show (ftoList f) ++ ")"


instance BitVector FDynamic where
  query = _query
  queryrank = _queryrank
  select = _select
  construct = fDynamic


build :: Int -> [Bool] -> FingerTree SizeRank Block
build size xs = fromList . map gapBlock $ blocks
  where blocks = cut size xs
        
fDynamic :: Int -> [Bool] -> FDynamic
fDynamic n xs = FDynamic blocksize (build blocksize xs)
  where blocksize = 4 * ilog2 n 
        
fingerTreeToList :: Measured v a => FingerTree v a -> [a]
fingerTreeToList f
  | null f    = []
  | otherwise = let (a:<as) = viewl f
                in a : fingerTreeToList as

ftoList :: FDynamic -> [Bool]
ftoList (FDynamic _ f) = concatMap unGapBlock $ fingerTreeToList f

blocks (FDynamic _ f) = map unGapBlock $ fingerTreeToList f

prop_build size dat = (size>0) ==> out == dat
  where out = concatMap unGapBlock . fingerTreeToList $ build size dat

find :: FDynamic -> (SizeRank->Bool) -> Maybe (SizeRank,Block)
find (FDynamic _ f) p =
  let (before,after) = split p f
  in case viewl after of      
    elem :< _ -> Just (measure before, elem)
    EmptyL    -> Nothing
     

_query :: FDynamic -> Int -> Bool
_query f i = query bits i'
  where Just (SizeRank s r, block) = find f (index i)
        i' = i-s
        bits = unGapBlock block
      
prop_query f =
  forAll (chooseIndex f) $
  \i -> let dat = ftoList f
        in _query f i == query dat i
           
_queryrank :: FDynamic -> Int -> Int
_queryrank f i = r + queryrank bits i'
  where Just (SizeRank s r, block) = find f (index i)
        i' = i-s
        bits = unGapBlock block
        
prop_queryrank f =
  forAll (chooseIndex f) $
  \i -> let dat = ftoList f
        in _queryrank f i == queryrank dat i
  
_select :: FDynamic -> Int -> Maybe Int
_select f i = do
  (SizeRank s r, block) <- find f (rank i)
  let bits = unGapBlock block
  fmap (+s) $ select bits (i-r)
  
prop_select f =
  forAll (chooseIndex f) $
  \i -> let dat = ftoList f
        in _select f i == select dat i


_insert :: FDynamic -> Int -> Bool -> FDynamic
_insert (FDynamic size f) i val =
  FDynamic size (before >< (gapBlock newbits) <| after)
    where (before', after') = split (index i) f
          
          (before, block, after) =
            case viewl after' of
              b :< bs -> (before', b, bs)
              EmptyL ->
                case viewr before' of
                  bs :> b -> (bs, b, empty)
                  EmptyR -> error "_insert: This shouldn't happen!"
          
          (SizeRank s _) = measure before
          i' = i-s
          bits = unGapBlock block
          newbits = insert bits i' val

prop_insert f =
  forAll (chooseIndex f) $ \i ->
    forAll (choose (False,True)) $ \val ->
      val == _query (_insert f i val) i

-- TEST INFRA
        
instance Arbitrary FDynamic where
  arbitrary = do xs <- listOf1 arbitrary
                 return $ fDynamic (length xs) xs
  shrink f = do let dat = ftoList f
                dat' <- shrink dat
                return $ fDynamic (length dat') dat'

chooseIndex :: FDynamic -> Gen Int
chooseIndex f = 
  let (SizeRank s _) = measure (unwrap f)
  in choose (0,s-1)
     
chooseRank :: FDynamic -> Gen Int
chooseRank f = 
  let (SizeRank _ r) = measure (unwrap f)
  in choose (0,r-1)