{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE BangPatterns #-}

module BitVector where

import Util
import Test.QuickCheck

class BitVector a where
  query :: a -> Int -> Bool
  queryrank  :: a -> Int -> Int
  select :: a -> Int -> Maybe Int
  construct :: Int -> [Bool] -> a

  construct' :: [Bool] -> a
  construct' xs = construct (length xs) xs

  queryrank0 :: a -> Int -> Int 
  queryrank0 a i = i - queryrank a i + 1
  
  querysize :: a -> Int
  
  deconstruct :: a -> [Bool]
  deconstruct b = map (query b) [0 .. querysize b - 1]

  --TODO: select0 :: a -> Int -> Maybe Int

class DynamicBitVector a where
  insert :: a -> Int -> Bool -> a
  delete :: a -> Int -> a

instance BitVector [Bool] where
  query = (!!)
  queryrank xs i = rank' $ take (i+1) xs
  select xs i = select' i xs
  construct = const id
  querysize = length
  
instance DynamicBitVector [Bool] where
  insert xs i val = a ++ val:b
    where (a,b) = splitAt i xs
  delete xs i = a ++ b
    where (a,_:b) = splitAt i xs

-- gap encoded bit vectors
newtype Gap = Gap {unGap :: Int}
  deriving (Show,Eq)

gapify :: [Bool] -> [Gap]
gapify xs = loop xs 0
  where loop [] acc         = [Gap acc]
        loop (True:xs) acc  = Gap acc : loop xs 0
        loop (False:xs) acc = loop xs (acc+1)

unGapify :: [Gap] -> [Bool]
unGapify (Gap x:xs) =
  replicate x False ++ concatMap (\i -> True:replicate (unGap i) False) xs

concatGaps :: [Gap] -> [Gap] -> [Gap]
concatGaps [Gap a] (Gap b:gs) = Gap (a+b) : gs
concatGaps (a:as)  gs         = a : concatGaps as gs

prop_concatGaps xs ys =
  (gapify (xs ++ ys)) == concatGaps (gapify xs) (gapify ys)

splitGaps gaps = let n = length gaps
                     (x,y) = splitAt (n `div` 2) gaps
                 in (x,Gap 0 : y)

instance BitVector [Gap] where

  querysize gs = sum (map ((+1).unGap) gs) - 1
  
  construct _ = gapify
  
  query gaps index = loop index gaps
    where loop left (Gap gap:gaps)
            | gap<left  = loop (left-gap-1) gaps
            | gap==left = if null gaps
                          then error "Query past end"
                          else True
            | gap>left  = False

  queryrank gaps index = loop index 0 gaps
    where loop left ones (Gap gap:gaps)
            | gap<left  = loop (left-gap-1) (ones+1) gaps
            | gap==left = if null gaps
                          then error "Rank past end"
                          else (ones+1)
            | gap>left  = ones

  select gaps index = loop 0 index gaps
    where loop bits ones (Gap gap:gaps)
            | ones>0  = loop (bits+gap+1) (ones-1) gaps
            | ones==0 = if null gaps
                        then Nothing
                        else Just (bits+gap)
                             
  deconstruct = unGapify


instance DynamicBitVector [Gap] where
  insert gaps index False = loop gaps index
    where loop (Gap gap:gaps) index
            | gap <  index = Gap gap : loop gaps (index-gap-1)
            | gap >= index = Gap (gap+1) : gaps
          loop [] _ = error "Insert past end!"
  insert gaps index True = loop gaps index
    where loop (Gap gap:gaps) index
            | gap <  index = Gap gap : loop gaps (index-gap-1)
            | gap >= index = Gap index : Gap (gap-index) : gaps
          loop [] _ = error "Insert past end!"

  delete gaps index = loop gaps index
    where loop (Gap gap:gaps) index
            | gap <  index = Gap gap : loop gaps (index-gap-1)
            | gap == index = case gaps of
                               [] -> error "Delete past end!"
                               (Gap gap' : gaps') -> Gap (gap+gap') : gaps'
            | gap >  index = Gap (gap-1) : gaps
          loop [] _ = error "Delete past end!"


rank' :: [Bool] -> Int
rank' xs = count id xs

select' :: Int -> [Bool] -> Maybe Int
select' i xs = go 0 i xs
    where go loc 0    (True:xs)  = Just loc
          go loc rank (True:xs)  = go (loc+1) (rank-1) xs
          go loc rank (False:xs) = go (loc+1) rank xs
          go _   _    []         = Nothing

prop_select' :: [Bool] -> Int -> Bool
prop_select' xs i = case select' i xs of
                      Nothing -> i >= rank' xs || i < 0
                      Just loc -> i == rank' (take loc xs)

prop_insert_query :: [Bool] -> Property
prop_insert_query xs =
  forAll (choose (0,length xs)) $ \i ->
    forAll (choose (False,True)) $ \val ->
      val == query (insert xs i val) i
      
prop_queryrank_bs :: NonEmptyList Bool -> Property
prop_queryrank_bs (NonEmpty xs) =
  forAll (chooseIndex xs) $ \i ->
    queryrank xs i == rank' (take (i+1) xs)
    
    
prop_queryrank0 :: NonEmptyList Bool -> Property
prop_queryrank0 (NonEmpty xs) =
  forAll (chooseIndex xs) $ \i ->
    queryrank0 xs i == count (==False) (take (i+1) xs)
