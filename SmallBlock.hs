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

newtype SmallBlock = SmallBlock Code
                deriving Show
                  
instance BitVector SmallBlock where
  
  construct siz xs
    | siz<=64=SmallBlock (bitsToCode xs)
    | otherwise = error "bad length for SmallBlock"

  query (SmallBlock c) i = testBit (getCode c) (fromIntegral i)
  
  queryrank (SmallBlock c) i =
    fromIntegral . populationCount . getCode $ takeCode (i+1) c

  select (SmallBlock c) i = loop (getCode c) i
    where lowestBit w = fromIntegral $ lowestBitPlus1 w - 1
          loop 0 0 = Nothing
          loop w 0 = Just $ lowestBit w
          loop w i = loop (w `clearBit` lowestBit w) (i-1)

  querysize (SmallBlock c) = codelengthG c

  deconstruct (SmallBlock c) =
    codeToBits c

instance Encoded SmallBlock where

  decode = gapify . deconstruct
    
  encode gs = construct' bs
    where bs = unGapify gs 
          
prop_encode_decode =
  forAll (choose (1,64)) $ \siz ->
    forAll (vector siz) $ \dat ->
    let gs = gapify dat in
    gs == decode (encode gs :: SmallBlock)

instance Measured SizeRank SmallBlock where
  measure (SmallBlock c) = SizeRank
                           (codelengthG c)
                           (fromIntegral . populationCount . getCode $ c)

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

