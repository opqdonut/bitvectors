{-# LANGUAGE FlexibleInstances #-}

module BitVector where

import Util

class BitVector a where
    query :: a -> Int -> Bool
    queryrank :: a -> Int -> Int
    select :: a -> Int -> Maybe Int
    construct :: Int -> [Bool] -> a

instance BitVector [Bool] where
    query = (!!)
    queryrank xs i = rank' $ take (i+1) xs
    select xs i = select' i xs
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
