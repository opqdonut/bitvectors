{-# LANGUAGE BangPatterns #-}

module BitStream where

import Prelude hiding (head,tail,null)
import qualified Data.ByteString as BS
import Data.Bits
import Data.Word
import Test.QuickCheck

data BitStream = BitStream {startOffset :: !Word8,
                            endOffset :: !Word8,
                            bits :: BS.ByteString}
               deriving Show
                 
--bit :: Int -> Word8
--bit i = setBit 0 i

mkWord :: [Bool] -> (Word8,Word8)
mkWord xs = go 0 7 xs
  where 
    go acc i []         = (acc,fromIntegral i + 1)
    go acc (-1) _          = error "mkWord"
    go acc i (True:xs)  = go (setBit acc i) (i-1) xs
    go acc i (False:xs) = go acc            (i-1) xs

mkWords :: [Bool] -> ([Word8],Word8)
mkWords xs =
  let (word', rest) = splitAt 8 xs
      (word,off) = mkWord word'
  in case rest of
    [] -> ([word],off)
    _  -> let (words,off) = mkWords rest
          in (word:words,off)

listToBitStream :: [Bool] -> BitStream
listToBitStream xs = BitStream 0 endOff bits
  where (words,endOff) = mkWords xs
        bits = BS.pack words
        
head :: BitStream -> Bool
head (BitStream startOffset _ bits) =
  testBit (BS.head bits) (7-fromIntegral startOffset)

tail :: BitStream -> BitStream
tail (BitStream 7 endOff bits) = BitStream 0 endOff (BS.tail bits)
tail (BitStream startOff endOff bits) = BitStream (startOff + 1) endOff bits

null :: BitStream -> Bool
null (BitStream start end bits) =
  case BS.uncons bits of
    Nothing -> True
    Just (word, bits') -> BS.null bits' && 8-start == end
    
bitStreamToList :: BitStream -> [Bool]
bitStreamToList b = go b
  where go b = if null b
               then []
               else head b : go (tail b)
                    
prop_list_bitstream :: [Bool] -> Bool                    
prop_list_bitstream xs = xs == bitStreamToList (listToBitStream xs)
                              