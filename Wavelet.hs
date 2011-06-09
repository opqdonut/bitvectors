{-# LANGUAGE FlexibleInstances #-}

module Wavelet where

import Debug.Trace
import Data.List hiding (insert)
import qualified Data.Map as M
import Data.Ord
import Test.QuickCheck

import FingerTreeDynamic
import Static2
import Encoding2
import BitVector
import Util

type Symbol = Char

alphabetSplit :: [Symbol] ->  -- left alphas
                 [Symbol] ->  -- right alphas
                 [Symbol] ->  -- data
                 ([Bool],     -- guide
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
halve xs = splitAt (length xs `div` 2) xs
        
data WaveletTree a
  = Leaf Symbol
  | Node [Symbol] a (WaveletTree a) (WaveletTree a)
  deriving Show
                     
symbols :: WaveletTree a -> [Symbol]
symbols (Leaf s) = [s]
symbols (Node ss _ _ _) = ss

mkWavelet :: Construct a =>
             [Symbol] ->     -- alphabet
             [Symbol] ->     -- data
             WaveletTree a
-- base case: alphabet of size one gives a leaf
mkWavelet [x] xs = if all (==x) xs
                   then Leaf x
                   else error ("Bad leaf! " ++ show x ++ " " ++ show xs)
-- otherwise split the alphabet and recurse
mkWavelet symbs xs = Node symbs vec left right
  where (lsymbs,rsymbs) = halve symbs  
        (guide,lxs,rxs) = alphabetSplit lsymbs rsymbs xs
        vec = construct' guide
        left  = mkWavelet lsymbs lxs
        right = mkWavelet rsymbs rxs
        
histogram :: [Symbol] -> M.Map Symbol Int
histogram = foldl' (\m sym -> M.insertWith' (+) sym 1 m) M.empty

mkWavelet' :: Construct a => [Symbol] -> WaveletTree a
mkWavelet' xs = mkWavelet symbols xs
  where h = histogram xs
        symbols = reverse . map fst . sortBy (comparing snd) $ M.assocs h
        
instance Arbitrary (WaveletTree [Bool]) where
  arbitrary = do xs <- listOf1 arbitrary
                 return $ mkWavelet' xs

wlength :: BitVector a => WaveletTree a -> Int
wlength (Node _ guide _ _) = querysize guide
wlength (Leaf _) = error "Not yet implemented!"

wread :: BitVector a => WaveletTree a -> Int -> Symbol
wread (Leaf symbol) i = symbol
wread (Node _ b left right) i
  | val == False = wread left  (queryrank0 b i - 1)
  | val == True  = wread right (queryrank  b i - 1)
    where val = query b i

proto_wread :: BitVector a => 
               ([Symbol] -> WaveletTree a) -> NonEmptyList Symbol -> Property
proto_wread construct (NonEmpty xs) =
  forAll (chooseIndex xs) $ \i ->
    let w = construct xs
    in wread w i == xs !! i
       
mkWavelet'list :: [Symbol] -> WaveletTree [Bool]
mkWavelet'list = mkWavelet'
mkWavelet'ng :: [Symbol] -> WaveletTree NBlock
mkWavelet'ng = mkWavelet'
mkWavelet'static :: [Symbol] -> WaveletTree Static
mkWavelet'static = mkWavelet'
mkWavelet'fd :: [Symbol] -> WaveletTree (FDynamic EBlock)
mkWavelet'fd = mkWavelet'

prop_wread_list   = proto_wread mkWavelet'list
prop_wread_ng     = proto_wread mkWavelet'ng
prop_wread_static = proto_wread mkWavelet'static
prop_wread_fd     = proto_wread mkWavelet'fd
       
wrank :: BitVector a => WaveletTree a -> Symbol -> Int -> Int
-- we fell of the tree while hunting for occurrences:
wrank _             _       (-1) = 0
wrank (Leaf symbol) symbol' i =
  if symbol==symbol'
  then i+1
  else error "This shouldn't happen!"
wrank (Node _ guide left right) symbol i
  | symbol `elem` symbols left  = wrank left  symbol (queryrank0 guide i - 1)
  | symbol `elem` symbols right = wrank right symbol (queryrank  guide i - 1)
                                  
proto_wrank :: BitVector a => 
               ([Symbol] -> WaveletTree a) -> NonEmptyList Symbol -> Property
proto_wrank construct (NonEmpty xs) = 
  forAll (chooseIndex xs) $ \i ->
    forAll (elements symbols) $ \s ->
      wrank w s i == count (==s) (take (i+1) xs)
  where w = construct xs
        symbols = (nub . sort) xs
        
prop_wrank_list   = proto_wrank mkWavelet'list
prop_wrank_ng     = proto_wrank mkWavelet'ng
prop_wrank_static = proto_wrank mkWavelet'static
prop_wrank_fd     = proto_wrank mkWavelet'fd
                    
{-
wselect :: BitVector a => WaveletTree a -> Symbol -> Int -> Int
wselect (Leaf symbol) symbol' i =
  if symbol==symbol'
  then i-1
  else error "This shouldn't happen!"
wselect (Node _ guide left right) symbol i
  | symbol `elem` symbols left  = select guide
  | symbol `elem` symbols right = select0 guide
-}

winsert :: (BitVector a, DynamicBitVector a) => WaveletTree a -> Symbol -> Int -> WaveletTree a
winsert (Node syms guide left right) symbol i
  | symbol `elem` symbols left =
    Node syms (insert guide i False) (winsert left symbol (queryrank0 guide (i-1))) right
  | symbol `elem` symbols right = 
    Node syms (insert guide i True) left (winsert right symbol (queryrank guide (i-1) ))
winsert (Leaf symbol) symbol' _ = Leaf symbol

proto_winsert construct (NonEmpty xs) =
  forAll (chooseIndex xs) $ \i ->
    forAll (elements symbols) $ \s ->
    let w = construct xs
        w' = winsert w s i
    in wread w' i == s &&
       wread w' (i+1) == wread w i
  where 
    symbols = (nub . sort) xs

prop_winsert_fd = proto_winsert mkWavelet'fd