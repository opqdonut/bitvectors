{-# LANGUAGE BangPatterns, MultiParamTypeClasses, FlexibleInstances,
  ScopedTypeVariables #-}

module Tree where

import Util

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Property

class Monoid a => Measured a v where
    measure :: v -> a

{-
instance (Measured a v, Measured b v)
    => Measured (a,b) v where
    measure x = (measure x, measure x)

instance (Measured a v, Measured b v, Measured c v)
    => Measured (a,b,c) v where
    measure x = (measure x, measure x, measure x)
-}

(+++) :: Monoid a => a -> a -> a
(+++) = mappend

data Color = Color

data Tree ann val = Node { color :: Color,
                           annotation :: !ann,
                           left :: !(Tree ann val),
                           right :: !(Tree ann val) }
                  | Leaf { value :: val }

instance Measured a v => Measured a (Tree a v) where
    measure (Leaf v) = measure v
    measure (Node _ a _ _) = a


node :: Measured ann val => Tree ann val -> Tree ann val
     -> Tree ann val
node l r = Node Color (measure l +++ measure r) l r

find :: Measured a v => (a -> Bool) -> Tree a v -> Maybe (a,v)
find p t = go mempty t
  where
    go acc (Leaf v)
        | p (acc +++ measure v) = Just ((acc +++ measure v),v)
        | otherwise = Nothing
    go acc (Node _ ann l r)
        | p (acc +++ measure l) = go acc l
        | p (acc +++ ann) = go (acc +++ measure l) r
        | otherwise = Nothing


-- -- -- Testing -- -- --

data SizeRank = SizeRank {getSize :: Int, getRank :: Int}

instance Monoid SizeRank where
    mappend (SizeRank a a') (SizeRank b b') =
        SizeRank (a+b) (a'+b')
    mempty = SizeRank 0 0
instance Measured SizeRank Bool where
    measure True  = SizeRank 1 1
    measure False = SizeRank 1 0

index :: Int->SizeRank->Bool
index i = (>i).getSize
rank :: Int->SizeRank->Bool
rank i = (>i).getRank

mkbal :: [Bool] -> Tree SizeRank Bool
mkbal [x] = Leaf x
mkbal xs = let (a,b) = splitAt (length xs `div` 2) xs
           in node (mkbal a) (mkbal b)

metaprop_mkbal p xs = not (null xs) ==>
                      forAll (choose (0,length xs-1)) (p xs)

prop_index :: [Bool] -> Gen Prop
prop_index = 
    metaprop_mkbal $ \xs i ->
        case find (index i) (mkbal xs) of
          Just ((SizeRank s _),b) -> b == xs !! i && s == i+1
          Nothing -> False

prop_rank :: [Bool] -> Gen Prop
prop_rank =
    metaprop_mkbal $ \xs i ->
        case find (rank i) (mkbal xs) of
          Just ((SizeRank s r),_) ->
              count id (take (s-1) xs) == i
          Nothing ->
              i >= count id xs
