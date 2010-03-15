{-# LANGUAGE FlexibleInstances, BangPatterns #-}

module Util
  where

import BitVector

import Data.Bits
import Data.MemoCombinators as Memo

import Data.Array.Unboxed
import Test.QuickCheck

cut :: Int -> [a] -> [[a]]
cut n xs = go xs
    where go [] = []
          go xs = take n xs : go (drop n xs)
--takeWhile (not.null) . map (take n) $ iterate (drop n) xs

count :: (a -> Bool) -> [a] -> Int
count f xs = length $ filter f xs

--(!) = V.index

ilog2 :: Int -> Int
ilog2 n = go 0 n
    where go !acc 0 = acc
          go !acc n = go (acc+1) (shiftR n 1)

roundUpToPowerOf k x = go 1
    where go acc | acc >= x  = acc
                 | otherwise = go (k*acc)

infixl 7 `mydiv`
mydiv a b = let (x,y) = quotRem a b in
            if y==0 then x else x+1
a /// b = fromIntegral a / fromIntegral b

listArray' n xs = listArray (0,n-1) xs

instance BitVector [Bool] where
    query = (!!)
    queryrank xs i = rank' $ take i xs
    select = select
    construct = const id
    

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

binom_max = 100

binom :: Integer -> Integer -> Integer
binom = Memo.memo2
          (Memo.unsafeArrayRange (0,binom_max))
          (Memo.unsafeArrayRange (0,binom_max))
          binom'
    where binom' _ 0 = 1
          binom' 0 _ = 0
          binom' n k =
              binom (n-1) k + binom (n-1) (k-1)

