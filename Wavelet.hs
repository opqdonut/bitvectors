{-# LANGUAGE FlexibleInstances #-}

module Wavelet where

import Data.List
import Test.QuickCheck

import Static2
import BitVector
import Util

type Symbol = Char

alphabetSplit :: [Symbol] ->  -- left alphas
                 [Symbol] ->  -- right alphas
                 [Symbol] ->  -- data
                 ([Bool],   -- guide
                  [Symbol],   -- left data
                  [Symbol])   -- right data
alphabetSplit left right xs = (guide,l,r)
  where guide = map (`elem` right) xs
        l = filter (`elem` left) xs
        r = filter (`elem` right) xs
        
alphabetMerge :: [Bool] ->  -- guide
                 [Symbol] ->  -- left
                 [Symbol] ->  -- right
                 [Symbol]
alphabetMerge []            []     []     = []
alphabetMerge (False:guide) (l:ls) rs     = l:alphabetMerge guide ls rs
alphabetMerge (True:guide)  ls     (r:rs) = r:alphabetMerge guide ls rs
        
prop_alphabetMergeSplit :: [Symbol] -> Gen Bool
prop_alphabetMergeSplit xs = 
  do
    let symbs = nub $ sort xs
    n<-choose (0,length symbs - 1)
    let (lsymbs,rsymbs) = splitAt n symbs
    let (g,l,r) = alphabetSplit lsymbs rsymbs xs
    let xs' = alphabetMerge g l r
    return (xs == xs')
                                            
                                            
halve :: [a] -> ([a],[a])
halve xs = go xs
  where go [] = ([],[])
        go (x:xs) = let (a,b) = go xs in (x:b,a)
        
data WaveletTree a = Leaf Symbol
                   | Node [Symbol] a (WaveletTree a) (WaveletTree a)
                   deriving Show
                     
symbols :: WaveletTree a -> [Symbol]
symbols (Leaf s) = [s]
symbols (Node ss _ _ _) = ss

mkWavelet :: BitVector a =>
             [Symbol] ->     -- symbols
             [Symbol] ->     -- data
             WaveletTree a

mkWavelet [x] xs = if all (==x) xs
                   then Leaf x
                   else error ("Bad leaf! " ++ show x ++ " " ++ show xs)

mkWavelet symbs xs = Node symbs vec left right
  where (lsymbs,rsymbs) = halve symbs  
        (guide,lxs,rxs) = alphabetSplit lsymbs rsymbs xs
        vec = construct (length guide) guide
        left  = mkWavelet lsymbs lxs
        right = mkWavelet rsymbs rxs
        
mkWavelet' :: BitVector a => [Symbol] -> WaveletTree a
mkWavelet' xs = mkWavelet (nub $ sort xs) xs
        
instance Arbitrary (WaveletTree [Bool]) where
  arbitrary = do xs <- listOf1 arbitrary
                 return $ mkWavelet' xs

wread :: BitVector a => WaveletTree a -> Int -> Symbol
wread (Leaf symbol) i = symbol
wread (Node _ b left right) i =
  if query b i
    then wread right (queryrank  b i - 1)
    else wread left  (queryrank0 b i - 1)

prop_wread :: NonEmptyList Symbol -> Property
prop_wread (NonEmpty xs) =
  forAll (chooseIndex xs) $ \i ->
    let w = mkWavelet' xs :: WaveletTree [Bool]
    in wread w i == xs !! i
       
wrank :: BitVector a => WaveletTree a -> Symbol -> Int -> Int
wrank (Leaf symbol) symbol' i =
  if symbol==symbol'
  then i+1
  else error "This shouldn't happen!"
wrank (Node _ guide left right) symbol i
  | symbol `elem` symbols left  = wrank left  symbol (queryrank0 guide i - 1)
  | symbol `elem` symbols right = wrank right symbol (queryrank  guide i - 1)
                                  
prop_wrank :: NonEmptyList Symbol -> Property
prop_wrank (NonEmpty xs) = 
  forAll (chooseIndex xs) $ \i ->
    forAll (elements symbols) $ \s ->
      wrank w s i == count (==s) (take (i+1) xs)
  where symbols = nub $ sort xs
        w :: WaveletTree [Bool]
        w = mkWavelet symbols xs
          
