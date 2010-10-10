{-# LANGUAGE RankNTypes,BangPatterns #-}

module Encoding2 where

import Data.List (isPrefixOf)
import Numeric (showHex)
import Data.Bits
--import Data.Bits.Extras
import Data.Word
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.ST

import Debug.Trace

import Test.QuickCheck hiding ((.&.))
                              
import Util

-- A Code is a smallish chunk of bits
data Code = Code {codelength :: !Word8, code :: !Word64}

instance Eq Code where
  a == b = codelength a == codelength b && getCode a == getCode b

getCode (Code len code) = code .&. ones (fromIntegral len)

instance Show Code where
  show (Code len code) = "Code " ++ show len ++ " " ++ showHex code ""

codeToBits :: Code -> [Bool]
codeToBits (Code len code) =
  map (testBit code) [fromIntegral len-1,fromIntegral len-2..0]
    
(+++) :: Code -> Code -> Code
a +++ b
  | codelength a + codelength b > 64 = error "out of space"
  | otherwise = Code
                (codelength a + codelength b)
                (shiftL (getCode a) (fromIntegral $ codelength b)
                 .|. (getCode b))
                
prop_plusplusplus :: Code -> Code -> Bool
prop_plusplusplus a b 
  | codelength a + codelength b > 64 = True
  | otherwise = codeToBits (a+++b) == codeToBits a ++ codeToBits b
  
instance Arbitrary Code where
  arbitrary = do code <- fmap fromIntegral $ (arbitrary :: Gen Integer)
                 codelength <- fmap fromIntegral $ (choose (0,64) :: Gen Integer)
                 return (Code codelength code)
  shrink (Code len code) = do l <- shrink len
                              return (Code l code)

{-# SPECIALIZE ones :: Int -> Word8 #-}
{-# SPECIALIZE ones :: Int -> Word64 #-}
ones :: Bits a => Int -> a
ones i = (setBit 0 i) - 1

elias_encode :: Int -> Code
elias_encode 0 = Code 1 1
elias_encode i 
  | l+ll+ll-1 > 64 = error "number too big for elias_encode"
  | otherwise = code
  where l  = ilog2 i
        ll = ilog2 l
        icode  = Code (fromIntegral $ l-1)  ((fromIntegral i) .&. ones (l-1))
        lcode  = Code (fromIntegral $ ll-1) ((fromIntegral l) .&. ones (ll-1))
        llcode = Code (fromIntegral $ ll+1) (setBit 0 0)
        code = llcode +++ lcode +++ icode
        
newtype Block = Block (UArray Int Word8)

instance Show Block where
  show (Block a) = "Block " ++ concatMap s (elems a)
    where 
      s :: Word8 -> String
      s w = (showHex w "") ++ ";"

blockToBits :: Block -> [Bool]
blockToBits (Block arr) = concatMap f (elems arr)
  where f w = map (testBit w) [7,6..0]

writeCode :: (forall s. STUArray s Int Word8 -> Int -> Code -> ST s Int)
writeCode arr index (Code 0 _) = return index
writeCode arr index c =
  do let (wordIndex,bitIndex) = index `divMod` 8
     word <- readArray arr wordIndex
     -- [64-length][nWrite][length-nWrite]
     -- => [64-nWrite][nWrite]
     -- => [bitIndex][nWrite][8-bitIndex-nWrite]
     let length = codelength c
         nWrite = min (8-bitIndex) (fromIntegral length)
         shifted = 
           flip shiftR (fromIntegral length-nWrite) (getCode c)
         toWrite = fromIntegral
                   . flip shiftL (8-bitIndex-nWrite)
                   $ shifted
     -- XXX overwriting doesn't work!
     writeArray arr wordIndex (word .|. toWrite)
     --trace (show ("XX",shifted,toWrite,wordIndex,bitIndex)) $
     writeCode arr (index+nWrite)
       (Code (length-fromIntegral nWrite) (code c))
     

makeBlock :: [Code] -> Block
makeBlock codes = Block array
  where 
    len = sum (map (fromIntegral.codelength) codes) `mydiv` 8
    array = runSTUArray $
                do arr <- newArray_ (0,len-1)
                   let loop [] _     = return ()
                       loop (c:cs) i = do i' <- writeCode arr i c
                                          loop cs i'
                   loop codes 0
                   return arr

prop_makeBlock :: [Code] -> Bool
prop_makeBlock codes = bits `isPrefixOf` bits'
  where block = makeBlock codes
        bits' = blockToBits block
        bits = concatMap codeToBits codes


readCode :: Block -> Int -> Int -> Code
readCode _ _ 0 = Code 0 0
readCode (Block arr) index _
  | index`div`8 > snd (bounds arr) = Code 0 0
readCode b@(Block arr) index len = Code (fromIntegral nRead) out +++ next
  where (wordIndex,bitIndex) = index `divMod` 8
        word = arr ! wordIndex
        nRead = min len (8-bitIndex)
        end = (8-bitIndex-nRead)
        mask = shiftL (ones nRead) end
        read = word .&. mask
        out = fromIntegral $ shiftR read end
        next = readCode b (index+nRead) (len-nRead)
        
prop_write_read_code :: Code -> Bool
prop_write_read_code c =
  trace (show c) $
  c == readCode (makeBlock [c]) 0 (fromIntegral $ codelength c)
  

myLeadingZeros :: Code -> Int
myLeadingZeros (Code length code) = loop 0 (fromIntegral $ length-1)
  where 
    loop acc (-1) = acc
    loop acc i    = if testBit code i
                    then acc
                    else loop (acc+1) (i-1)

readElias :: Block -> Int -> Maybe (Int,Int)
readElias b index =
  case ll of
    0 -> Just (0,index+1)
    _ -> 
      case codelength lcode of
        0 -> Nothing
        _ -> Just (out,index+ll+ll+l-1)
    where llcode = readCode b index 64
          ll = myLeadingZeros llcode
          lcode = readCode b (index+ll) ll
          l = fromIntegral $ getCode lcode
          code = readCode b (index+ll+ll) (l-1)
          finalcode = (Code 1 1) +++ code
          out = fromIntegral $ getCode finalcode
        
prop_read_write_elias = 
  forAll (choose (0,8007199254740992)) $ \i -> 
    let code = elias_encode i
        block = makeBlock [code]
        Just (out,len) = readElias block 0
    in (i == out && len == fromIntegral (codelength code))

readEliass :: Block -> [Int]        
readEliass block = loop 0
  where loop i = case readElias block i
                 of Just (val,i') -> val:loop i'
                    Nothing -> []
                    
prop_read_eliass =
  forAll (listOf1 $ choose (0,8007199254740992)) $ \is ->
    is == (readEliass . makeBlock . map elias_encode) is

gapEncode :: [Bool] -> [Code]
gapEncode xs = loop xs 0
  where
    loop [] !acc         = [elias_encode acc]
    loop (True:xs) !acc  = elias_encode acc : loop xs 0
    loop (False:xs) !acc = loop xs (acc+1)

gapBlock :: [Bool] -> Block
gapBlock = makeBlock . gapEncode

gapDecode :: [Int] -> [Bool]
gapDecode (x:xs) =
  replicate x False ++ concatMap (\i -> True:replicate i False) xs

unGapBlock :: Block -> [Bool]
unGapBlock = gapDecode . readEliass

prop_gap_block xs =
  xs == unGapBlock (gapBlock xs)

{-
elias_decode :: Code -> Int
elias_decode (Code length code) = if ll == 0 then 0 else i
  where ll = leadingZeros code - (64-length)
  -}