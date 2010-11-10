{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,FlexibleContexts,ExistentialQuantification #-}

module FingerTreeDynamic where

import Measure
import Encoding2
import Util
import BitVector

import Data.List (unfoldr)

import Debug.Trace
import Test.QuickCheck hiding ((><))
import Test.QuickCheck.Property

import Prelude hiding (reverse,null)
import Data.FingerTree
import Data.Array.Unboxed (UArray,(!),bounds,elems)

data FDynamic a = 
  (Measured SizeRank a, Encoded a) =>
  FDynamic 
  {blocksize :: Int,
   unwrap :: (FingerTree SizeRank a)}

instance BitVector (FDynamic (EBlock EG)) where
  query = _query
  queryrank = _queryrank
  select = _select
  construct = fDynamic

instance BitVector (FDynamic (EBlock NG)) where
  query = _query
  queryrank = _queryrank
  select = _select
  construct = fDynamic
  
instance BitVector (FDynamic (EBlock UN)) where
  query = _query
  queryrank = _queryrank
  select = _select
  construct = fDynamic

instance Show (FDynamic a) where
  show f = "(FDynamic " ++ show (blocksize f) ++ " " ++ show (ftoList f) ++ ")"

build :: (Encoded a, Measured SizeRank a) =>
         Int -> [Bool] -> FingerTree SizeRank a
build size xs = fromList $ unfoldr go xs
  where go [] = Nothing
        go xs = let block = encode $ take size xs
                in block `seq` Just (block, drop size xs)

        
fDynamic :: (Encoded a, Measured SizeRank a) =>
            Int -> [Bool] -> FDynamic a
fDynamic n xs = FDynamic blocksize (build blocksize xs)
  where blocksize = roundUpToPowerOf 2 $ 2 * ilog2 n
        
fingerTreeToList :: Measured v a => FingerTree v a -> [a]
fingerTreeToList f
  | null f    = []
  | otherwise = let (a:<as) = viewl f
                in a : fingerTreeToList as

ftoList :: FDynamic a -> [Bool]
ftoList (FDynamic _ f) = concatMap decode $ fingerTreeToList f

blocks (FDynamic _ f) = map decode $ fingerTreeToList f

prop_build size dat = (size>0) ==> out == dat
  where out = concatMap decode . fingerTreeToList $ (build size dat :: FingerTree SizeRank (EBlock EG))

find :: FDynamic a -> (SizeRank->Bool) -> Maybe (SizeRank,a)
find (FDynamic _ f) p =
  let (before,after) = split p f
  in case viewl after of      
    elem :< _ -> Just (measure before, elem)
    EmptyL    -> Nothing
     

_query :: Encoded a => FDynamic a -> Int -> Bool
_query f i = query bits i'
  where Just (SizeRank s r, block) = find f (index i)
        i' = i-s
        bits = decode block
      
proto_query f =
  forAll (chooseIndex f) $
  \i -> let dat = ftoList f
        in _query f i == query dat i
prop_query :: FDynamic (EBlock EG) -> Property
prop_query = proto_query
prop_query_n :: FDynamic (EBlock NG) -> Property
prop_query_n = proto_query
           
           
_queryrank :: Encoded a => FDynamic a -> Int -> Int
_queryrank f i = r + queryrank bits i'
  where Just (SizeRank s r, block) = find f (index i)
        i' = i-s
        bits = decode block
        
proto_queryrank f =
  forAll (chooseIndex f) $
  \i -> let dat = ftoList f
        in _queryrank f i == queryrank dat i
prop_queryrank :: FDynamic (EBlock EG) -> Property
prop_queryrank = proto_queryrank
prop_queryrank_n :: FDynamic (EBlock NG) -> Property
prop_queryrank_n = proto_queryrank
                  
_select :: Encoded a => FDynamic a -> Int -> Maybe Int
_select f i = do
  (SizeRank s r, block) <- find f (rank i)
  let bits = decode block
  fmap (+s) $ select bits (i-r)
  
proto_select f =
  forAll (chooseIndex f) $
  \i -> let dat = ftoList f
        in _select f i == select dat i
prop_select :: FDynamic (EBlock EG) -> Property
prop_select = proto_select
prop_select_n :: FDynamic (EBlock NG) -> Property
prop_select_n = proto_select

_insert :: (Measured SizeRank a, Encoded a) => FDynamic a -> Int -> Bool -> FDynamic a
_insert (FDynamic size f) i val =
  FDynamic size (before >< (encode newbits) <| after)
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
          bits = decode block
          newbits = insert bits i' val

proto_insert f =
  forAll (chooseIndex f) $ \i ->
    forAll (choose (False,True)) $ \val ->
      val == _query (_insert f i val) i
prop_insert :: FDynamic (EBlock EG) -> Property
prop_insert = proto_insert
prop_insert_n :: FDynamic (EBlock NG) -> Property
prop_insert_n = proto_insert

-- TEST INFRA
        
arbitrary_impl :: (Encoded a, Measured SizeRank a) => Gen (FDynamic a)
arbitrary_impl = do xs <- listOf1 arbitrary
                    return $ fDynamic (length xs) xs
shrink_impl f = do let dat = ftoList f
                   dat' <- shrink dat
                   return $ fDynamic (length dat') dat'               
                
instance Arbitrary (FDynamic (EBlock EG)) where
  arbitrary = arbitrary_impl
  shrink = shrink_impl

instance Arbitrary (FDynamic (EBlock NG)) where
  arbitrary = arbitrary_impl
  shrink = shrink_impl

chooseIndex :: Measured SizeRank a => FDynamic a -> Gen Int
chooseIndex f = 
  let (SizeRank s _) = measure (unwrap f)
  in choose (0,s-1)
     
chooseRank :: Measured SizeRank a => FDynamic a -> Gen Int
chooseRank f = 
  let (SizeRank _ r) = measure (unwrap f)
  in choose (0,r-1)