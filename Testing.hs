module Testing where

import BitVector
import Util
import IO

import Test.QuickCheck
import Test.QuickCheck.Property

arbitraryBitVector :: Construct a => Gen a
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
  
test_construct :: BitVector a => ([Bool] -> a) -> NonEmptyList Bool -> Property
test_construct mk (NonEmpty bs) = property $ bs == deconstruct (mk bs)

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

name string tst = whenFail (putStrLn string) tst

test_BitVector a = 
  name "In test_construct:" (test_construct a) .&&.
  name "In test_querysize:" (test_querysize a) .&&.  
  name "In test_query:" (test_query a) .&&.
  name "In test_queryrank:" (test_queryrank a) .&&.
  name "In test_queryrank0:" (test_queryrank0 a) .&&.
  name "In test_select:" (test_select a) 

test_insert mk (NonEmpty xs) = 
  forAll (chooseIndex xs) $ \i ->
  forAll (choose (False,True)) $ \val ->
  insert xs i val == deconstruct (insert (mk xs) i val)
  
test_delete mk (NonEmpty xs) = 
  forAll (chooseIndex xs) $ \i ->
  delete xs i == deconstruct (delete (mk xs) i)
  
---

test t n k = do
  --putStrLn $ "blength,slength: " ++ show (blength test, slength test)

  print $ query t 0

  --let bits = sum $ map (last . elems . blocklocations) $ elems $ supers test
  --putStrLn $ "bits: " ++ show bits ++ " (" ++ show (bits///n) ++ ")"

  let go :: Int -> Int -> IO ()
      go x 0 = return ()
      go x i = {-# SCC "go" #-}
        do {-# SCC "go_x" #-}     putStr (show x)
           {-# SCC "go_query" #-} if query t x
                                    then putStr "T"
                                    else putStr "F"
           {-# SCC "go_rank" #-}  putStr (show $ queryrank t x)
           putStr " "
           go (x*17`mod`n) (i-1)

  go 1 k

