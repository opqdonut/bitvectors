module Encoding (order)
  where

import Util

import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.List
import Data.Bits
import Data.Maybe
import Debug.Trace

data BiMap a b = BiMap (M.Map a b) (M.Map b a)
fromList :: (Ord a,Ord b) => [(a,b)] -> BiMap a b
fromList xs = BiMap (M.fromList xs) (M.fromList $ map turn xs)
    where turn (a,b) = (b,a)

decode :: Ord a => BiMap a b -> a -> b
decode (BiMap m _) a = fromJust $ M.lookup a m

encode :: Ord b => BiMap a b -> b -> a
encode (BiMap _ m) b = fromJust $ M.lookup b m



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


strings :: Int -> Int -> [[Bool]]
strings  0 0 = [[]]
strings  _ 0 = []
strings  i n = map (True:) (strings (i-1) (n-1))
               ++ map (False:) (strings i (n-1))

ordertable :: Int -> BiMap (Integer, Integer) [Bool]
ordertable t = fromList [((fromIntegral c,o),x) | c<-[0..t],
                                     (o,x)<-subtable c]
    where
      subtable :: Int -> [(Integer,[Bool])]
      subtable i = zip [0..] . sort $ strings i t


order :: Int -> ([Bool] -> [Bool],
                 [Bool] -> [Bool])
order t = (enc,dec)
    where table = ordertable t
          l = ilog2 t
          enc x = let (c,o) = encode table (pad t x)
                    in pad l (bitify c) ++ bitify o
          dec x = let (c',o') = splitAt l x
                  in decode table (unbitify c',unbitify o')

                  