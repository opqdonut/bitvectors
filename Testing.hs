module Testing where

import BitVector
import Util

import Test.QuickCheck
import Test.QuickCheck.Property

arbitraryBitVector :: BitVector a => Gen a
arbitraryBitVector = do
  bs <- listOf1 arbitrary
  return $ construct' bs

arbitraryIndex :: BitVector a => a -> Gen Int
arbitraryIndex b = choose (0,querysize b - 1)

meta_bitvector :: (BitVector a, Eq b) =>
                  ([Bool] -> a) ->
                  (a -> Int -> b) -> ([Bool] -> Int -> b) ->
                  NonEmptyList Bool -> Property
meta_bitvector mk f fList (NonEmpty xs) =
  forAll (chooseIndex xs) $ \i ->
  let a = fList xs i
      vector = mk xs
      b = f vector i
    in (a==b)
  
test_query :: BitVector a => ([Bool] -> a) -> NonEmptyList Bool -> Property
test_query mk = meta_bitvector mk query query

test_queryrank :: BitVector a => ([Bool] -> a) -> NonEmptyList Bool -> Property
test_queryrank mk = meta_bitvector mk queryrank queryrank

test_queryrank0 :: BitVector a => ([Bool] -> a) -> NonEmptyList Bool -> Property
test_queryrank0 mk = meta_bitvector mk queryrank queryrank

test_querysize :: BitVector a => ([Bool] -> a) -> NonEmptyList Bool -> Bool
test_querysize mk (NonEmpty xs) = 
  querysize xs == querysize (mk xs)

test_select :: BitVector a => ([Bool] -> a) -> NonEmptyList Bool -> Property
test_select mk = meta_bitvector mk select select

test_BitVector a = test_query a
                   .&&. test_queryrank a
                   .&&. test_queryrank0 a
                   .&&. test_querysize a
                   .&&. test_select a
