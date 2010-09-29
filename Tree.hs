{-# LANGUAGE BangPatterns, MultiParamTypeClasses, FlexibleInstances,
  ScopedTypeVariables #-}

module Tree where

import Util
import BitVector
import Measure
import Test.QuickCheck
import Test.QuickCheck.Property

data Color = Color deriving Show

data Tree ann val = Node { color :: Color,
                           annotation :: !ann,
                           left :: !(Tree ann val),
                           right :: !(Tree ann val) }
                  | Leaf { annotation :: !ann,
                           value :: !val }
                  | Empty
                    deriving Show

toList :: Tree a v -> [v]
toList Empty = []
toList (Leaf _ v) = [v]
toList (Node _ _ l r) = toList l ++ toList r

instance Measured a v => Measured a (Tree a v) where
    measure Empty = mempty
    measure (Leaf a _) = a
    measure (Node _ a _ _) = a

leaf :: Measured ann val => val -> Tree ann val
leaf v = Leaf (measure v) v

node :: Measured ann val =>
        Tree ann val -> Tree ann val -> Tree ann val
node Empty r = r
node l Empty = l
node l r = Node Color (measure l +++ measure r) l r

find :: Measured a v => (a -> Bool) -> Tree a v -> Maybe (a,v)
find p t = go mempty t
  where
    go !acc (Leaf a v)
        | p (acc +++ a) = let x = acc+++a in x `seq` Just (x,v)
        | otherwise = Nothing
    go !acc (Node _ ann l r)
        | p (acc +++ measure l) = go acc l
        | p (acc +++ ann) = go (acc +++ measure l) r
        | otherwise = Nothing

{-
-- Okasaki's Red-Black balancing
balance :: Measured a v =>
           Color -> Tree a v -> Tree a v -> Tree a v
balance Red l r = node Red l r
balance Black (Node Red _ (Node Red _ w x) y) z = node Red (node Black w x) (node Black y z)
balance Black (Node Red _ w (Node Red _ x y)) z = node Red (node Black w x) (node Black y z)
balance Black w (Node Red _ (Node Red _ x y) z) = node Red (node Black w x) (node Black y z)
balance Black w (Node Red _ x (Node Red _ y z)) = node Red (node Black w x) (node Black y z)
-}

change :: Measured a v 
       => (Tree a v -> Tree a v)
       -> (a -> Bool)
       -> Tree a v
       -> Tree a v
change f p t = go mempty t
  where
    go acc tree@(Node _ ann l r)
        | p (acc +++ measure l) = node (go acc l) r
        | p (acc +++ ann) = node l (go (acc +++ measure l) r)
        | otherwise = tree
    go acc leaf@(Leaf {})
        | p (acc +++ measure leaf) = f leaf
        | otherwise = leaf

delete :: Measured a v => (a -> Bool) -> Tree a v -> Tree a v                           
delete = change (const Empty)

insert :: Measured a v => v -> (a -> Bool) -> Tree a v -> Tree a v
insert v = change (\x -> node x (leaf v))

-- -- -- Testing -- -- --

build :: Measured a v => [Tree a v] -> Tree a v
build ts = tree
    where [tree] = build' ts
          build' []       = []
          build' [x]      = [x]
          build' (l:r:xs) = build' (node l r : build' xs)

mkbal :: Measured a v => [v] -> Tree a v
mkbal xs = build $ map leaf xs

prop_mkbal :: [Bool] -> Gen Prop
prop_mkbal xs = not (null xs) ==>
                xs == toList (mkbal xs :: Tree SizeRank Bool)

metaprop_mkbal p xs = not (null xs) ==>
                      forAll (choose (0,length xs-1)) (p xs)

prop_sizerank :: [Bool] -> Gen Prop
prop_sizerank xs = not (null xs) ==>
                   let t = mkbal xs in
                   annotation t == SizeRank (length xs) (rank' xs)

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
              rank' (take (s-1) xs) == i
          Nothing ->
              i >= count id xs

prop_delete_1 :: [Bool] -> Gen Prop
prop_delete_1 =
    metaprop_mkbal $ \xs i ->
        let xs' = toList $ delete (index i) (mkbal xs)
            (a,_:c) = splitAt i xs
        in xs' == a++c

prop_insert_1 :: [Bool] -> Gen Prop
prop_insert_1 =
    metaprop_mkbal $ \xs i ->
        let xs' = toList $ insert True (index i) (mkbal xs)
            (a,b) = splitAt (i+1) xs
        in xs' == a++True:b

