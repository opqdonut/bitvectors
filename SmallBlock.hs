{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}

module SmallBlock where

import Encoding2
import BitVector
import Testing
import Measure hiding ((+++))
import Util

import Data.Word
import Data.Bits
import Data.Bits.Extras

import Test.QuickCheck hiding ((.&.))

newtype SmallBlock = SmallBlock Word64
                deriving Show
                  
instance BitVector SmallBlock where
  
  construct siz xs
    | siz<=64=SmallBlock (getCode $ bitsToCode xs)
    | otherwise = error "bad length for SmallBlock"

  query (SmallBlock c) i = testBit c (fromIntegral i)
  
  queryrank (SmallBlock c) i =
    fromIntegral . populationCount $ ones (i+1) .&. c

  select (SmallBlock c) i = loop c i
    where lowestBit w = fromIntegral $ lowestBitPlus1 w - 1
          loop 0 0 = Nothing
          loop w 0 = Just $ lowestBit w
          loop w i = loop (w `clearBit` lowestBit w) (i-1)

  querysize (SmallBlock c) = 64

  deconstruct (SmallBlock c) =
    map (testBit c) [0..63]

instance Encoded SmallBlock where

  decode = gapify . deconstruct
    
  encode gs = construct' bs
    where bs = unGapify gs 
          
prop_encode_decode =
  forAll (vector 64) $ \dat ->
  let gs = gapify dat in
  gs == decode (encode gs :: SmallBlock)

instance Measured SizeRank SmallBlock where
  measure (SmallBlock c) = SizeRank
                           64
                           (fromIntegral . populationCount $ c)

---
  
mk :: [Bool] -> SmallBlock
mk = construct'

genList = do
    siz <- choose (1,64)
    bs <- vector siz
    return $ NonEmpty bs
  
prop_query     = genList >>= meta_bitvector mk query query
prop_queryrank = genList >>= meta_bitvector mk queryrank queryrank
prop_select    = genList >>= meta_bitvector mk select select

---- 


-- XXX newtype SmallElias = SmallElias Word64

newtype SmallElias = SmallElias Word64
  deriving Show
--newtype SmallNibble = SmallNibble Code

packElias :: [Gap] -> [Code]
packElias gs = go c cs
  where (c:cs) = map elias_encode gs
        go acc (x:xs)
          | codelength acc + codelength x > 63  = (acc+++one) : go x xs
          | otherwise = go (acc+++x) xs
        go acc [] = [acc]
        one = elias_encode (Gap 0)
        
smallElias :: [Bool] -> [SmallElias]
smallElias = map (SmallElias . getCode) . packElias . gapify

smallEliasToGaps :: SmallElias -> [Gap]
smallEliasToGaps (SmallElias c) = loop (Code 64 c)
    where loop c = case eliasDecode c
                   of Nothing -> []
                      Just (g,len) -> g:loop (dropCode len c)
                      
prop_smallElias (NonEmpty bs) = bs == concatMap (unGapify.smallEliasToGaps) (smallElias bs)

instance Measured SizeRank SmallElias where
  measure = measure . smallEliasToGaps
                      
instance BitVector SmallElias where
  construct _ bs = s
    where [s] = smallElias bs
          
  deconstruct = unGapify . smallEliasToGaps
          
  query s i = query (smallEliasToGaps s) i
  queryrank s i = queryrank (smallEliasToGaps s) i
  select s i = select (smallEliasToGaps s) i
  querysize s = querysize (smallEliasToGaps s)


genSmallElias = do
  bs <- listOf1 arbitrary
  return . head . smallElias $ bs
  
genSmallEliasList = fmap (NonEmpty . deconstruct) genSmallElias

prop_query_SE     = genSmallEliasList >>= meta_bitvector mk query query
prop_queryrank_SE = genSmallEliasList >>= meta_bitvector mk queryrank queryrank
prop_select_SE    = genSmallEliasList >>= meta_bitvector mk select select
