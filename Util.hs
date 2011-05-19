{-# LANGUAGE FlexibleInstances, BangPatterns #-}

module Util
  where

import Data.List
import qualified Data.Map as M
import Data.Bits
import Data.MemoCombinators as Memo

import qualified Data.ByteString as B

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

bitify 0 = []
bitify n | even n    = False : bitify (n`div`2)
         | otherwise = True  : bitify (n`div`2) 

pad len xs = take len $ xs ++ repeat False

unbitify :: [Bool] -> Integer
unbitify xs = go 0 1 xs
    where go acc _ [] = acc
          go acc k (True :xs) = go (acc+k) (2*k) xs
          go acc k (False:xs) = go acc     (2*k) xs


prop_unbitify_bitify i  = i>=0 ==> unbitify (bitify i) == i
prop_bitify_unbitify xs' = 
    let xs = xs'++[True] in bitify (unbitify xs) == xs

prop_bitify_ilog x = x>0 ==> length (bitify x) == ilog2 x


roundUpToPowerOf k x = go 1
    where go acc | acc >= x  = acc
                 | otherwise = go (k*acc)

roundUpToMultipleOf k x = k * (x `mydiv` k)

infixl 7 `mydiv`
mydiv a b = let (x,y) = quotRem a b in
            if y==0 then x else x+1
a /// b = fromIntegral a / fromIntegral b

listArray' n xs = listArray (0,n-1) xs


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

occurrences :: Eq a => a -> [a]  -> Int
occurrences x xs = length $ filter (==x) xs

entropy :: Int -> String -> Double
entropy 0 xs =
  sum [ let f_a = occurrences a xs /// n in negate $ f_a * logBase 2 f_a
      | a <- alphabet]
    where n = length xs
          alphabet = nub xs
          
entropy k xs =
  x / fromIntegral n
    where n = length xs
          -- all k-substrings of xs
          subs = take n . map (take k) $ tails xs 
          chars = map (:[]) . take n . drop k $ xs
          combine (xs,n) (xs',n') = (xs++xs',n+n')
          m = M.fromListWith combine $ zip subs (zip chars $ repeat 1)
          x = sum [ n_xs * entropy 0 xs | (xs,n_xs) <- M.elems m ]

bitsFromFile :: String -> IO [Bool]
bitsFromFile name = do
  d <- B.readFile name
  return $ concatMap (pad 8.bitify) (B.unpack d)

-------------
-- Test utils
-------------

chooseIndex :: [a] -> Gen Int
chooseIndex xs = choose (0,length xs - 1)
