{-# LANGUAGE BangPatterns,FlexibleInstances,MultiParamTypeClasses,FlexibleContexts #-}

module Tree where

import Measure
import BitVector
import Encoding2 hiding ((+++))
import Util
import Testing
import SmallBlock

import Data.List hiding (find)
import Test.QuickCheck
import Test.QuickCheck.Property

import Debug.Trace

data Tree a v = Empty
              | Leaf {measureLeaf :: !a, val :: v}
              | Node {left :: !(Tree a v),
                      right :: !(Tree a v),
                      measureNode :: !a}
              deriving Show
                       
instance Measured a v => Measured a (Tree a v) where
  measure Empty = mempty
  measure (Leaf a v) = a
  measure (Node _ _ a) = a

leaf :: Measured ann val => val -> Tree ann val
leaf v = Leaf (measure v) v

node :: Measured ann val =>
        Tree ann val -> Tree ann val -> Tree ann val
node Empty r = r
node l Empty = l
node l r = Node l r (measure l +++ measure r) 

find :: Measured a v => (a -> Bool) -> Tree a v -> Maybe (a,v)
find p t = go mempty t
  where
    go !acc (Leaf ann v)
        | p (acc +++ ann) = Just (acc,v)
        | otherwise = Nothing
    go !acc (Node l r ann)
        | p (acc +++ measure l) = go acc l
        | p (acc +++ ann) = go (acc +++ measure l) r
        | otherwise = Nothing
                      
                      
---  building balanced trees

treeHeight Empty = 0
treeHeight (Leaf _ _) = 1
treeHeight (Node l r _) = 1 + max (treeHeight l) (treeHeight r)

showTreeShape i (Node l r _) = showTreeShape (i+1) l
                               ++ replicate i ' ' ++ "N\n"
                               ++ showTreeShape (i+1) r
showTreeShape i Empty        = replicate i ' ' ++ "E\n"
showTreeShape i (Leaf _ v)   = replicate i ' ' ++ "L " ++ show v ++"\n"

pairUp :: Measured a v => [Tree a v] -> [Tree a v]
pairUp []       = []
pairUp [x]      = [x]
pairUp (l:r:xs) = (node l r : pairUp xs)

iteratePairUp :: Measured a v => [Tree a v] -> Tree a v
iteratePairUp [t] = t
iteratePairUp ts = iteratePairUp $ pairUp ts

mkbal :: Measured a v => [v] -> Tree a v
mkbal xs = t
  where t = iteratePairUp $ map leaf xs

--- quick'n'dirty bitvector

build :: (BitVector a, Measured SizeRank a) =>
         Int -> [Bool] -> Tree SizeRank a
build size xs = mkbal $ unfoldr go xs
  where go [] = Nothing
        --- XXX the "construct size" is a bit ugly
        go xs = let block = construct size $ take size xs
                in block `seq` Just (block, drop size xs)

instance (Measured SizeRank a, BitVector a) =>
         BitVector (Tree SizeRank a) where
  
  construct n xs = build blocksize xs
    where blocksize = roundUpToPowerOf 2 $ 16 * ilog2 n
  
  query t i = query block (i-s)
    where Just ((SizeRank s r),block) = find (index i) t
          
  queryrank t i = r + queryrank block (i-s)
    where Just ((SizeRank s r),block) = find (index i) t
          
  select t i = do
    (SizeRank s r, block) <- find (rank i) t
    fmap (+s) $ select block (i-r)
    
  querysize = getSize . measure


newtype Dynamic = Dynamic (Tree SizeRank EBlock)

mkDynamic n xs = Dynamic (build blocksize xs) --(mkbal blocks)
  where blocksize = roundUpToMultipleOf 64 $ 4 * ilog2 n
        --blocks = encodeMany blocksize $ gapify xs

instance BitVector Dynamic where
  
  construct = mkDynamic
  
  query (Dynamic t) i = query t i
          
  queryrank (Dynamic t) i = queryrank t i
          
  select (Dynamic t) i = select t i
  
  querysize (Dynamic t) = querysize t
    
prop_Dynamic = test_BitVector (construct' :: [Bool] -> Dynamic)
       
-----

data SmallDynamic = SmallDynamic Int (Tree SizeRank SmallBlock)

instance BitVector SmallDynamic where
  
  construct siz xs = SmallDynamic siz (build 64 xs)
  
  query (SmallDynamic s t) i = query t i          
  queryrank (SmallDynamic s t) i = queryrank t i          
  select (SmallDynamic s t) i = select t i
  querysize (SmallDynamic s t) = s

prop_SmallDynamic = test_BitVector (construct' :: [Bool] -> SmallDynamic)

-----

newtype SmallEliasDynamic = SmallEliasDynamic (Tree SizeRank SmallElias)

instance BitVector SmallEliasDynamic where
  
  construct _ xs = SmallEliasDynamic . mkbal . smallElias $ xs
  query (SmallEliasDynamic t) i = query t i          
  queryrank (SmallEliasDynamic t) i = queryrank t i          
  select (SmallEliasDynamic t) i = select t i
  querysize (SmallEliasDynamic t) = querysize t

prop_SmallEliasDynamic = test_BitVector (construct' :: [Bool] -> SmallEliasDynamic)