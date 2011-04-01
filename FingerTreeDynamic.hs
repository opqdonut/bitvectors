{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,FlexibleContexts,ExistentialQuantification #-}

module FingerTreeDynamic where

import Measure
import Encoding2
import Util
import BitVector
import SmallBlock
import Testing

import Data.List (unfoldr)

import Debug.Trace
import Test.QuickCheck hiding ((><))
import Test.QuickCheck.Property

import Prelude hiding (reverse,null)
import Data.FingerTree
import Data.Array.Unboxed (UArray,(!),bounds,elems)

data FDynamic a = (Measured SizeRank a, BitVector a) =>
                  FDynamic {blocksize :: Int,
                            unwrap :: FingerTree SizeRank (Cached SizeRank a)}

instance BitVector (FDynamic EBlock) where
  query = _query
  queryrank = _queryrank
  select = _select
  construct = fDynamic
  querysize = _size
  
instance DynamicBitVector (FDynamic EBlock) where
  insert = _insert

instance BitVector (FDynamic NBlock) where
  query = _query
  queryrank = _queryrank
  select = _select
  construct = fDynamic
  querysize = _size
  
instance DynamicBitVector (FDynamic NBlock) where
  insert = _insert
  
instance BitVector (FDynamic UBlock) where
  query = _query
  queryrank = _queryrank
  select = _select
  construct = fDynamic
  querysize = _size
  
{-
instance DynamicBitVector (FDynamic UBlock) where
  insert = _insert
-}

instance BitVector (FDynamic SmallBlock) where
  query = _query
  queryrank = _queryrank
  select = _select
  construct _ xs = FDynamic 64 (build 64 xs)
  querysize = _size

instance BitVector (FDynamic SmallElias) where
  query = _query
  queryrank = _queryrank
  select = _select
  construct _ xs = FDynamic 0 . fromList . map cached . smallElias $ xs
  querysize = _size

instance Show (FDynamic a) where
  show f = "(FDynamic " ++ show (blocksize f) ++ " " ++ show (ftoList f) ++ ")"

build :: (BitVector a, Measured SizeRank a) =>
         Int -> [Bool] -> FingerTree SizeRank a
build size xs = fromList $ unfoldr go xs
  where go [] = Nothing
        --- XXX the "construct size" is a bit ugly
        go xs = let block = construct size $ take size xs
                in block `seq` Just (block, drop size xs)

        
fDynamic :: (Encoded a, BitVector a, Measured SizeRank a) =>
            Int -> [Bool] -> FDynamic a
fDynamic n xs = FDynamic blocksize . fromList . map cached . encodeMany blocksize $ gapify xs
  where blocksize = max
                    (roundUpToMultipleOf 8 $ 8 * ilog2 n)
                    16
        
fingerTreeToList :: Measured v a => FingerTree v a -> [a]
fingerTreeToList f
  | null f    = []
  | otherwise = let (a:<as) = viewl f
                in a : fingerTreeToList as

ftoList :: FDynamic a -> [Bool]
ftoList (FDynamic _ f) = concatMap deconstruct $ fingerTreeToList f

blocks (FDynamic _ f) = map decode $ fingerTreeToList f

prop_build size dat = (size>0) ==> out == dat
  where out = concatMap (unGapify.decode) . fingerTreeToList $ (build size dat :: FingerTree SizeRank EBlock)

_size :: (BitVector a, Measured SizeRank a) => FDynamic a -> Int
_size = getSize . measure . unwrap

find :: FDynamic a -> (SizeRank->Bool) -> Maybe (SizeRank,a)
find (FDynamic _ f) p =
  let (before,after) = {-# SCC "split-p-f" #-} split p f
      m = {-# SCC "split-measure" #-} measure before
  in case viewl after of      
    elem :< _ -> Just (m, unCached elem)
    EmptyL    -> Nothing
     

_query :: BitVector a => FDynamic a -> Int -> Bool
_query f i = query block i'
  where Just (SizeRank s r, block) = find f (index i)
        i' = i-s
      
_queryrank :: BitVector a => FDynamic a -> Int -> Int
_queryrank f i = r + queryrank block i'
  where Just (SizeRank s r, block) = find f (index i)
        i' = i-s
        
_select :: BitVector a => FDynamic a -> Int -> Maybe Int
_select f i = do
  (SizeRank s r, block) <- find f (rank i)
  fmap (+s) $ select block (i-r)
  
balanceAt :: (Measured SizeRank a, Encoded a) =>
             Int -> a -> FingerTree SizeRank (Cached SizeRank a) -> FingerTree SizeRank (Cached SizeRank a)
balanceAt lim elem after
  --- XXX the order of the cases is important!
  | encodedSize elem > 2*lim
    --- XXX! might produce elems that are too small!
    = let (a,b) = cleave elem in cached a <| cached b <| after
  | null after
    = singleton (cached elem)
  | encodedSize elem < lim
    = let (a :< after') = viewl after in (cached (combine elem (unCached a)) <| after')
  | otherwise
    = cached elem <| after

_insert :: (DynamicBitVector a, Measured SizeRank a, Encoded a) => FDynamic a -> Int -> Bool -> FDynamic a
_insert (FDynamic size f) i val =
  FDynamic size (before >< balanced)
    where (before', after') = split (index i) f
          
          (before, block, after) =
            case viewl after' of
              b :< bs -> (before', unCached b, bs)
              EmptyL ->
                case viewr before' of
                  bs :> b -> (bs, unCached b, empty)
                  EmptyR -> error "_insert: This shouldn't happen!"
          
          (SizeRank s _) = measure before
          i' = i-s
          newblock = insert block i' val
          
          balanced = balanceAt size newblock after

proto_insert f =
  forAll (chooseFIndex f) $ \i ->
    forAll (choose (False,True)) $ \val ->
      val == _query (_insert f i val) i
prop_insert :: FDynamic EBlock -> Property
prop_insert = proto_insert
prop_insert_n :: FDynamic NBlock -> Property
prop_insert_n = proto_insert

-- TEST INFRA
        
arbitrary_impl :: (Encoded a, BitVector a, Measured SizeRank a) => Gen (FDynamic a)
arbitrary_impl = do NonEmpty xs <- arbitrary
                    len <- choose (1,2^32)
                    return $ fDynamic len xs
shrink_impl f = do let dat = ftoList f
                       siz = querysize f
                   NonEmpty dat' <- shrink (NonEmpty dat)
                   siz' <- shrink siz
                   return $ fDynamic (length dat') dat'               
                
instance Arbitrary (FDynamic EBlock) where
  arbitrary = arbitrary_impl
  shrink = shrink_impl

instance Arbitrary (FDynamic NBlock) where
  arbitrary = arbitrary_impl
  shrink = shrink_impl

chooseFIndex :: Measured SizeRank a => FDynamic a -> Gen Int
chooseFIndex f = 
  let (SizeRank s _) = measure (unwrap f)
  in choose (0,s-1)
     
chooseFRank :: Measured SizeRank a => FDynamic a -> Gen Int
chooseFRank f = 
  let (SizeRank _ r) = measure (unwrap f)
  in choose (0,r-1)
     
prop_fd_UBlock = test_BitVector (construct' :: [Bool] -> FDynamic UBlock)
prop_fd_NBlock = test_BitVector (construct' :: [Bool] -> FDynamic NBlock)
prop_fd_EBlock = test_BitVector (construct' :: [Bool] -> FDynamic EBlock)

-- XXX broken cuz smallblock doesn't store it's length anymore...
--prop_fd_SmallBlock = test_BitVector (construct' :: [Bool] -> FDynamic SmallBlock)
  
prop_fd_SmallElias =
  test_BitVector (construct' :: [Bool] -> FDynamic SmallElias)