module Static2 where

import Encoding2 hiding ((+++))
import Util
import Measure
import BitVector

import Data.Array.Unboxed
import Data.Word

import Test.QuickCheck

type V = UArray Int

data Static =
  Static {compressed :: Block,
          stride :: Int,
          ranks :: V Int,      -- i -> rank(B,i*stride)
          locations :: V Int,  -- \ mapping from unencoded locations
          offsets :: V Int     -- / to encoded locations
         }
  deriving Show
  
  
mkStatic n bs = 
  let stride = ilog2 n
      arraySize = n `mydiv` stride
      compressed = gapBlock bs
      p = process stride compressed
      (encposes,offsets,sizeranks) = unzip3 p
      ranks = map getRank sizeranks
      
      mkArr = listArray' arraySize
          
  in Static
     compressed
     stride
     (mkArr ranks)
     (mkArr encposes)
     (mkArr offsets)
     
measureGap :: Int -> SizeRank
measureGap gap = SizeRank (gap+1) 1
     
process :: Int
           -> Block
           -> [(Int,Int,SizeRank)]  -- [(encpos,offset,sr)]
process stride block = format 0 (SizeRank 0 0) 0 $ loop 0 (SizeRank 0 0)
  where loop i acc =
          case readElias block i
          of
            Nothing -> [(i,acc)]
            Just (gap,i') -> let acc' = acc +++ measureGap gap
                             in (i,acc):loop i' acc'
                                
        format :: Int->SizeRank->Int->[(Int,SizeRank)]->[(Int,Int,SizeRank)]
        format _      _   _      []                   = []
        format encpos sr nextpos (x@(encpos',sr'):xs)
          | siz' > nextpos = (encpos,offset,sr)
                             : format encpos sr (nextpos+stride) (x:xs)
          | otherwise = format encpos' sr' nextpos xs
            where 
              siz  = getSize sr
              siz' = getSize sr'
              offset = nextpos-siz

---

_query :: Static -> Int -> Bool
_query static i = 
  let arrayIndex = i `div` stride static
      i' = arrayIndex * stride static
      location = locations static ! arrayIndex
      offset = (offsets static ! arrayIndex) + (i-i')
      gaps = readEliass' (compressed static) location
  in
   queryGaps gaps offset
   
_queryrank :: Static -> Int -> Int
_queryrank static i =
  let arrayIndex = i `div` stride static
      i' = arrayIndex * stride static
      location = locations static ! arrayIndex
      offset = (offsets static ! arrayIndex) + (i-i')
      baseRank = ranks static ! arrayIndex
      gaps = readEliass' (compressed static) location
  in
   baseRank + queryrankGaps gaps offset
          
prop_query :: Property
prop_query = 
  forAll (listOf1 arbitrary) $ \bs ->
  forAll (choose (0,(length bs)-1)) $ \i ->
  query bs i == _query (mkStatic (length bs) bs) i
  
prop_queryrank :: Property
prop_queryrank =
  forAll (listOf1 arbitrary) $ \bs ->
  forAll (choose (0,(length bs)-1)) $ \i ->
  queryrank bs i == _queryrank (mkStatic (length bs) bs) i
  
instance BitVector Static where
  query = _query
  queryrank = _queryrank
  select = undefined
  construct = mkStatic