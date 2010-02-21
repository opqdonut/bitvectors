module Encoding (order,pad,gap_encode',gap_encode,gap_decode)
  where

import Util

import Test.QuickCheck
import Test.QuickCheck.Property
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


prop_unbitify_bitify i  = i>=0 ==> unbitify (bitify i) == i
prop_bitify_unbitify xs' = 
    let xs = xs'++[True] in bitify (unbitify xs) == xs

prop_bitify_ilog x = x>0 ==> length (bitify x) == ilog2 x

binom _ 0 = 1
binom 0 _ = 0
binom n k = binom (n-1) k + binom (n-1) (k-1)

calc_o :: Int -> Integer -> [Bool] -> Integer
calc_o t c xs = go 0 t c xs
    where go acc t c (False:xs)
              = go acc                   (t-1) c     xs
          go acc t c (True:xs)
              = go (acc + binom (t-1) c) (t-1) (c-1) xs
          go acc _ _ [] = acc

calc_c :: [Bool] -> Integer
calc_c xs = fromIntegral $ count id xs

encode :: Int -> [Bool] -> (Integer,Integer)
encode t xs = (c,o)
    where c = calc_c xs
          o = calc_o t c xs

decode :: Int -> (Integer,Integer) -> [Bool]
decode t (c,o) = go t c o
 where 
   go 0 _ _ = []
   go t c o
       | o >= binom (t-1) c = True: go (t-1) (c-1) (o-binom (t-1) c)
       | otherwise          = False:go (t-1) c     o
                             
prop_encode_decode =
    forAll (do t <- choose (1,18)
               c <- choose (0,fromIntegral t -1)
               o <- choose (0,binom t c -1)
               return (t,c,o))
      (\(t,c,o) -> encode t (decode t (c,o)) == (c,o))

prop_decode_encode xs = let t = length xs in
                        t < 18 ==> decode t (encode t xs) == xs



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

safeinit :: [a] -> [a]
safeinit [] = []
safeinit xs = init xs                  

elias_encode :: Int -> [Bool]
elias_encode i = let l  = ilog2 i
                     ll = ilog2 l
                 in replicate ll False ++
                    [True] ++
                    safeinit (bitify l) ++
                    safeinit (bitify i)

elias_decode :: [Bool] -> (Int,[Bool])
elias_decode (True:xs) = (0,xs)
elias_decode xs = let (llpart, True:rest) = break id xs
                      ll = length llpart
                      (lpart, rest') = splitAt (ll-1) rest
                      l  = fromIntegral $ unbitify (lpart++[True])
                      (xpart, rest'') = splitAt (l-1) rest'
                      x = fromIntegral $ unbitify (xpart++[True])
                  in (x, rest'')

prop_elias :: Int -> Property
prop_elias i = i >= 0 ==> elias_decode (elias_encode i) == (i,[])     

gap_encode' :: [Bool] -> [[Bool]]
gap_encode' xs | all not xs = [elias_encode $ length xs]
gap_encode' xs = let (gap,True:rest) = break id xs
                     code = elias_encode $ length gap
                 in code : gap_encode' rest

gap_encode :: [Bool] -> [Bool]
gap_encode xs = concat $ gap_encode' xs

gap_decode :: [Bool] -> [Bool]
gap_decode [] = []
gap_decode xs = case elias_decode xs of
                  (i,[]) -> replicate i False
                  (i,rest) -> replicate i False ++ True:gap_decode rest

prop_gap :: [Bool] -> Bool
prop_gap xs = gap_decode (gap_encode xs) == xs

