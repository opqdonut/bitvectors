module BitVector where

class BitVector a where
    query :: a -> Int -> Bool
    queryrank :: a -> Int -> Int
    select :: a -> Int -> Maybe Int
    construct :: Int -> [Bool] -> a

