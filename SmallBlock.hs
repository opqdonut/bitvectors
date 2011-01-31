{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}

module SmallBlock where

import Encoding2
import BitVector
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
    | siz <= 64 = SmallBlock (measure xs)
                             (fromIntegral . unbitify $ xs)
    | otherwise = error "bad length for SmallBlock"

  query b i = testBit (getData b) i

  queryrank b i = fromIntegral . populationCount $ 
                    getData b .&. ones (i+1)

  select b i = undefined

  querysize = getSize . measureSmallBlock

instance Encoded SmallBlock where

  decode (SmallBlock (SizeRank s _) w) =
    gapify . take s $ (bitify w ++ repeat False)
    
  encode gs = construct (length bs) bs
    where bs = unGapify gs :: [Bool]
          
prop_encode_decode =
  forAll (choose (1,64)) $ \siz ->
    forAll (vector siz) $ \dat ->
    let gs = gapify dat in
    gs == decode (encode gs :: SmallBlock)

prop_query =
  forAll (choose (1,63)) $ \siz ->
    forAll (vector siz) $ \dat ->
    forAll (chooseIndex dat) $ \i ->
    let a = query dat i
        b = query (construct siz dat :: SmallBlock) i
    in (a==b)
  
prop_queryrank = 
  forAll (choose (1,63)) $ \siz ->
    forAll (vector siz) $ \dat ->
    forAll (chooseIndex dat) $ \i ->
    let a = queryrank dat i
        b = queryrank (construct siz dat :: SmallBlock) i
    in (a==b)

instance Measured SizeRank SmallBlock where
  measure = measureSmallBlock

