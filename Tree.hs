{-# LANGUAGE BangPatterns,FlexibleInstances,MultiParamTypeClasses,FlexibleContexts #-}

module Tree where

import Measure
import BitVector
import Encoding2 hiding ((+++))
import Util

import Data.List hiding (find)
import Test.QuickCheck
import Test.QuickCheck.Property


data Tree a v = Empty
              | Leaf {measureLeaf :: !a, val :: v}
              | Node {left :: Tree a v,
                      right :: Tree a v,
                      measureNode :: !a}
              deriving Show
                       
instance Measured a v => Measured a (Tree a v) where
  measure Empty = mempty
  measure (Leaf a _) = a
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
        | p (acc +++ ann) = let x = acc+++ann in x `seq` Just (x,v)
        | otherwise = Nothing
    go !acc (Node l r ann)
        | p (acc +++ measure l) = go acc l
        | p (acc +++ ann) = go (acc +++ measure l) r
        | otherwise = Nothing
                      
                      
---  building balanced trees

mkbal_build :: Measured a v => [Tree a v] -> Tree a v
mkbal_build ts = tree
    where [tree] = build' ts
          build' []       = []
          build' [x]      = [x]
          build' (l:r:xs) = build' (node l r : build' xs)

mkbal :: Measured a v => [v] -> Tree a v
mkbal xs = mkbal_build $ map leaf xs

{-
prop_mkbal :: [Bool] -> Gen Prop
prop_mkbal xs = 
  xs == toList (mkbal xs :: Tree SizeRank Bool)
-}

--- quick'n'dirty bitvector

build :: (BitVector a, Encoded a, Measured SizeRank a) =>
         Int -> [Bool] -> Tree SizeRank a
build size xs = mkbal $ unfoldr go xs
  where go [] = Nothing
        --- XXX the "construct size" is a bit ugly
        go xs = let block = construct size $ take size xs
                in block `seq` Just (block, drop size xs)

newtype Dynamic = Dynamic (Tree SizeRank UBlock)

mkDynamic n xs = Dynamic (build blocksize xs)
  where blocksize = roundUpToPowerOf 2 $ 16 * ilog2 n

instance BitVector Dynamic where
  
  construct = mkDynamic
  
  query (Dynamic t) i = query block (i-(s-len))
    where Just ((SizeRank s r),block) = find (index i) t
          len = getSize . measure $ block
          
  queryrank (Dynamic t) i = (r-r') + queryrank block (i-(s-s'))
    where Just ((SizeRank s r),block) = find (index i) t
          (SizeRank s' r') = measure block
          
  select (Dynamic t) i = do
    (SizeRank s r, block) <- find (rank i) t
    let (SizeRank s' r') = measure block
    fmap (+(s-s')) $ select block (i-(r-r'))
    
prop_query :: NonEmptyList Bool -> Gen Bool
prop_query (NonEmpty xs) = do
  i <- chooseIndex xs
  let a = query xs i
  let b = query (mkDynamic (length xs) xs) i
  return (a==b)
  
prop_queryrank :: NonEmptyList Bool -> Gen Bool
prop_queryrank (NonEmpty xs) = do
  i <- chooseIndex xs
  let a = queryrank xs i
  let b = queryrank (mkDynamic (length xs) xs) i
  return (a==b)
  
prop_select :: NonEmptyList Bool -> Gen Prop
prop_select (NonEmpty xs) =
  forAll (chooseIndex xs) $ \i ->
    let a = select xs i
        b = select (mkDynamic (length xs) xs) i
    in a==b