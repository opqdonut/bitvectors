{-# LANGUAGE FlexibleInstances #-}

module Util
  where

import BitVector

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
ilog2 n = ceiling (logBase 2 (fromIntegral n + 1))

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

