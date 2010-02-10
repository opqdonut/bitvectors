{-# LANGUAGE BangPatterns, MultiParamTypeClasses, FlexibleInstances #-}

module Tree where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Property

class Monoid a => Measured a v where
    measure :: v -> a

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

find :: Measured a v => (a -> Bool) -> Tree a v -> Maybe v
find p t = go mempty t
  where
    go acc (Leaf v)
        | p (acc +++ measure v) = Just v 
        | otherwise = Nothing
    go acc (Node _ ann l r)
        | p (acc +++ measure l) = go acc l
        | p (acc +++ ann) = go (acc +++ measure l) r
        | otherwise = Nothing


-- -- -- Testing -- -- --

instance Measured (Sum Int) Bool where
    measure = const (Sum 1)

index i = (>i).getSum

mkbal [x] = Leaf x
mkbal xs = let (a,b) = splitAt (length xs `div` 2) xs
           in node (mkbal a) (mkbal b)

prop_mkbal :: [Bool] -> Gen Prop
prop_mkbal xs =
    not (null xs) ==>
        forAll (choose (0,length xs-1))
                   (\i -> find (index i) (mkbal xs) == Just (xs !! i))
