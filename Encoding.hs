module Encoding (order)
  where

import Util

import Test.QuickCheck
import Data.List
import Data.Bits
import Data.Maybe
import Debug.Trace



{-
bitify :: Int -> Integer -> [Bool]
bitify n i = map (testBit i) [0..n-1]
-}

bitify 0 = []
bitify n | even n    = False : bitify (n`div`2)
         | otherwise = True  : bitify (n`div`2) 

pad len xs = take len $ xs ++ repeat False

unbitify :: [Bool] -> Integer
unbitify xs = go 0 1 xs
    where go acc _ [] = acc
          go acc k (True :xs) = go (acc+k) (2*k) xs
          go acc k (False:xs) = go acc     (2*k) xs


prop_unbitify_bitify i  = unbitify (bitify i) == i
prop_bitify_unbitify xs = bitify (unbitify xs) == xs

prop_bitify_ilog x = x>0 ==> length (bitify x) == ilog2 x

binom _ 0 = 1
binom 0 _ = 0
binom n k = binom (n-1) k + binom (n-1) (k-1)

calc_o t c xs = go 0 t c xs
    where go acc t c (False:xs)
              = go acc                   (t-1) c     xs
          go acc t c (True:xs)
              = go (acc + binom (t-1) c) (t-1) (c-1) xs
          go acc _ _ [] = acc

calc_c xs = count id xs

encode t xs = (c,o)
    where c = calc_c xs
          o = calc_o t c xs

decode t (c,o) = go t c o
 where 
   go 0 _ _ = []
   go t c o
       | o >= binom (t-1) c = True: go (t-1) (c-1) (o-binom (t-1) c)
       | otherwise          = False:go (t-1) c     o
                             
prop_encode_decode t (c,o) =
    t > 0 && t <= 15 && t >= c && c >= 0 && o >= 0 && binom t c > o 
          ==> encode t (decode t (c,o)) == (c,o)
prop_decode_encode xs = let t = length xs in
                        t < 20 ==> decode t (encode t xs) == xs



order :: Int -> ([Bool] -> [Bool],
                 [Bool] -> [Bool])
order t = (enc,dec)
    where l = ilog2 t
          enc x = let (c,o) = encode t x
                  in pad l (bitify c) ++ bitify o
          dec x = let (c',o') = splitAt l x
                  in decode t (unbitify c',unbitify o')

prop_order_1 xs = let t = length xs
                      (e,d) = order t
                  in t <= 15 ==> d (e xs) == xs
                  
