module Util
  where

import Data.Array.Unboxed

cut :: Int -> [a] -> [[a]]
cut n xs = go xs
    where go [] = []
          go xs = take n xs : go (drop n xs)
--takeWhile (not.null) . map (take n) $ iterate (drop n) xs

count :: (a -> Bool) -> [a] -> Int
count f xs = length $ filter f xs

--(!) = V.index

ilog2 :: Int -> Int
ilog2 n = floor (logBase 2 (fromIntegral n)) +1

infixl 7 `mydiv`
mydiv a b = let (x,y) = quotRem a b in
            if y==0 then x else x+1
a /// b = fromIntegral a / fromIntegral b

listArray' n xs = listArray (0,n-1) xs
