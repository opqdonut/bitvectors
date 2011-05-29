{-# LANGUAGE BangPatterns,FlexibleInstances,MultiParamTypeClasses,FlexibleContexts #-}

module Tree where

import Measure
import BitVector
import Encoding2 hiding ((+++))
import Util
import Testing
import SmallBlock

import Data.List hiding (find,insert,delete)
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
                      
modify :: Measured a v =>
          (a -> Bool) -> ((a,v) -> v) -> Tree a v -> (Tree a v)
modify p f t = go mempty t
  where 
    go !acc orig@(Leaf ann v)
      | p (acc +++ ann) = leaf $ f (acc,v)
      | otherwise = orig
    go !acc orig@(Node l r ann)
      | p (acc +++ measure l) = node (go acc l) r
      | p (acc +++ ann)       = node l          (go (acc +++ measure l) r)
      | otherwise = orig
                      
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

build :: (Construct a, Measured SizeRank a) =>
         Int -> [Bool] -> Tree SizeRank a
build size xs = mkbal $ unfoldr go xs
  where go [] = Nothing
        --- XXX the "construct size" is a bit ugly
        go xs = let block = construct size $ take size xs
                in block `seq` Just (block, drop size xs)

instance (Measured SizeRank a, BitVector a) =>
         BitVector (Tree SizeRank a) where
           
  query t i = query block (i-s)
    where Just ((SizeRank s r),block) = find (index i) t
          
  queryrank t i = r + queryrank block (i-s)
    where Just ((SizeRank s r),block) = find (index i) t
          
  select t i = do
    (SizeRank s r, block) <- find (rank i) t
    fmap (+s) $ select block (i-r)
    
  querysize = getSize . measure

instance (Measured SizeRank a, DynamicBitVector a) =>
         DynamicBitVector (Tree SizeRank a) where
           
  insert t i v = modify (index i)
                 (\(SizeRank s r,block) -> insert block (i-s) v)
                 t
  delete t i = modify (index i)
               (\(SizeRank s r,block) -> delete block (i-s))
               t

type Dynamic a = Tree SizeRank a

mkDynamic n xs = (mkbal blocks)
  where blocksize = roundUpToMultipleOf 8 $ 8 * ilog2 n
        blocks = encodeMany blocksize $ gapify xs

instance Construct (Tree SizeRank EBlock) where
  construct n xs = mkbal blocks
    where blocksize = roundUpToMultipleOf 8 $ 8 * ilog2 n
          blocks = encodeMany blocksize $ gapify xs

instance Construct (Tree SizeRank NBlock) where
  construct n xs = mkbal blocks
    where blocksize = roundUpToMultipleOf 8 $ 8 * ilog2 n
          blocks = encodeMany blocksize $ gapify xs

instance Construct (Tree SizeRank UBlock) where
  construct n xs = mkbal blocks
    where blocksize = roundUpToMultipleOf 8 $ 8 * ilog2 n
          blocks = encodeMany blocksize $ gapify xs

prop_Dynamic_EBlock = test_BitVector (construct' :: [Bool] -> Dynamic EBlock)
prop_Dynamic_NBlock = test_BitVector (construct' :: [Bool] -> Dynamic NBlock)
prop_Dynamic_UBlock = test_BitVector (construct' :: [Bool] -> Dynamic UBlock)

prop_Dynamic_insert = test_insert (construct' :: [Bool] -> Dynamic EBlock)
prop_Dynamic_delete = test_delete (construct' :: [Bool] -> Dynamic EBlock)
prop_Dynamic_insert_N = test_insert (construct' :: [Bool] -> Dynamic NBlock)
prop_Dynamic_delete_N = test_delete (construct' :: [Bool] -> Dynamic NBlock)

-----

data SmallDynamic = SmallDynamic Int (Tree SizeRank SmallBlock)

instance BitVector SmallDynamic where
  query (SmallDynamic s t) i = query t i          
  queryrank (SmallDynamic s t) i = queryrank t i          
  select (SmallDynamic s t) i = select t i
  querysize (SmallDynamic s t) = s

instance Construct SmallDynamic where
  construct siz xs = SmallDynamic siz (build 64 xs)
  
prop_SmallDynamic = test_BitVector (construct' :: [Bool] -> SmallDynamic)

-----

newtype SmallEliasDynamic = SmallEliasDynamic (Tree SizeRank SmallElias)

instance BitVector SmallEliasDynamic where
  query (SmallEliasDynamic t) i = query t i          
  queryrank (SmallEliasDynamic t) i = queryrank t i          
  select (SmallEliasDynamic t) i = select t i
  querysize (SmallEliasDynamic t) = querysize t
  
instance Construct SmallEliasDynamic where
  construct _ xs = SmallEliasDynamic . mkbal . smallElias $ xs
  

prop_SmallEliasDynamic = test_BitVector (construct' :: [Bool] -> SmallEliasDynamic)