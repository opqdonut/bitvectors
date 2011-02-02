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

data SmallBlock = SmallBlock {measureSmallBlock :: {-# UNPACK #-} !SizeRank,
                              getData :: {-# UNPACK #-} !Word64}
                deriving Show
                  
smallBlockBitLength = bitSize (0 :: Word64)
                  
instance BitVector SmallBlock where
  
  construct siz xs
    | siz<=64=SmallBlock (measure xs) (fromIntegral . unbitify $ xs)
    | otherwise = error "bad length for SmallBlock"

  query b i = testBit (getData b) i
  
  queryrank b i = fromIntegral . populationCount $ 
                    getData b .&. ones (i+1)

  select b i = loop (getData b) i
    where lowestBit w = fromIntegral $ lowestBitPlus1 w - 1
          loop 0 0 = Nothing
          loop w 0 = Just $ lowestBit w
          loop w i = loop (w `clearBit` lowestBit w) (i-1)

  querysize = getSize . measureSmallBlock

  deconstruct (SmallBlock (SizeRank s _) w) =
    take s $ (bitify w ++ repeat False)

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
  measure = measureSmallBlock

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


