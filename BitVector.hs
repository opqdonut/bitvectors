{-# LANGUAGE FlexibleInstances #-}

module BitVector where

import Util
import Test.QuickCheck

class BitVector a where
  query :: a -> Int -> Bool
  queryrank  :: a -> Int -> Int
  select :: a -> Int -> Maybe Int
  construct :: Int -> [Bool] -> a

  queryrank0 :: a -> Int -> Int 
  queryrank0 a i = i - queryrank a i + 1

  --TODO: select0 :: a -> Int -> Maybe Int

class DynamicBitVector a where
  insert :: a -> Int -> Bool -> a

instance BitVector [Bool] where
  query = (!!)
  queryrank xs i = rank' $ take (i+1) xs
  select xs i = select' i xs
  construct = const id
  
instance DynamicBitVector [Bool] where
  insert xs i val = a ++ val:b
    where (a,b) = splitAt i xs
    

rank' :: [Bool] -> Int
rank' xs = count id xs

select' :: Int -> [Bool] -> Maybe Int
select' i xs = go 0 i xs
    where go loc 0    (True:xs)  = Just loc
          go loc rank (True:xs)  = go (loc+1) (rank-1) xs
          go loc rank (False:xs) = go (loc+1) rank xs
          go _   _    []         = Nothing

prop_select' :: [Bool] -> Int -> Bool
prop_select' xs i = case select' i xs of
                      Nothing -> i >= rank' xs || i < 0
                      Just loc -> i == rank' (take loc xs)

prop_insert_query :: [Bool] -> Property
prop_insert_query xs =
  forAll (choose (0,length xs)) $ \i ->
    forAll (choose (False,True)) $ \val ->
      val == query (insert xs i val) i
      
prop_queryrank_bs :: NonEmptyList Bool -> Property
prop_queryrank_bs (NonEmpty xs) =
  forAll (chooseIndex xs) $ \i ->
    queryrank xs i == rank' (take (i+1) xs)
    
    
prop_queryrank0 :: NonEmptyList Bool -> Property
prop_queryrank0 (NonEmpty xs) =
  forAll (chooseIndex xs) $ \i ->
    queryrank0 xs i == count (==False) (take (i+1) xs)
