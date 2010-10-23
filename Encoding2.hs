{-# LANGUAGE RankNTypes,BangPatterns #-}

module Encoding2 where

import Data.List (isPrefixOf)
import Numeric (showHex)
import Data.Bits
import Data.Bits.Extras
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

instance Arbitrary Block where
  arbitrary = do xs <- listOf1 arbitrary
                 return . Block $ listArray' (length xs) xs

instance Show Block where
  show (Block a) = "Block " ++ concatMap s (elems a)
    where 
      s :: Word8 -> String
      s w = (showHex w "") ++ ";"

blockToBits :: Block -> [Bool]
blockToBits (Block arr) = concatMap f (elems arr)
  where f w = map (testBit w) [7,6..0]

bitLength :: Block -> Int
bitLength (Block arr) = 8 * (snd (bounds arr) + 1)

prop_bitLength :: Block -> Bool
prop_bitLength b = length (blockToBits b) == bitLength b

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

putInto :: Word64 -> Word8 -> Int -> Word64
putInto into word len = output
  where shifted = shiftL (fromIntegral word) (len-8)
        output = into .|. shifted
                 
prop_putInto = 
  forAll (choose (0,63)) $ \i ->
    putInto 0 1 (i+8) == shiftL 1 i

readInto :: Block -> Int -> Int -> Word64 -> Word64
readInto (Block arr) wordI len into = putInto into word len
  where word = arr!wordI

readCode :: Block -> Int -> Int -> Code
readCode b index len 
  | realLen == 0 = Code 0 0
  | otherwise    = Code (fromIntegral realLen) code
  where realLen = min len (bitLength b - index)
        (wordIndex,bitIndex) = index `divMod` 8
        start = readInto b wordIndex (realLen+bitIndex) 0
        nRead = min len (8-bitIndex)
        len' = realLen-nRead
        wordIndex' = wordIndex + 1
        code = loop start wordIndex' len'
        loop !w _ 0 = w
        loop !w !wi !len = let len' = max 0 (len-8)
                           in loop (readInto b wi len w) (wi+1) len'
       
prop_readCode block =
  forAll (choose (0,bitLength block-1)) $ \i ->
    forAll (choose (0,64)) $ \len ->
      codeToBits (readCode block i len) 
      == (take len . drop i $ blockToBits block)

prop_write_read_code :: Code -> Bool
prop_write_read_code c =
  c == readCode (makeBlock [c]) 0 (fromIntegral $ codelength c)
  
myLeadingZeros :: Code -> Word32
myLeadingZeros c
  -- "bug": leadingZeros 0 === leadingZeros 1
  | getCode c == 0 = fromIntegral $ codelength c
  | otherwise = leadingZeros (getCode c)
       - (64 - fromIntegral (codelength c))
       
prop_myLeadingZeros :: Code -> Bool
prop_myLeadingZeros code = a == b
  where a = fromIntegral . length . takeWhile not . codeToBits $ code
        b = myLeadingZeros code

{-
myLeadingZeros' :: Block -> Int -> Maybe Int
myLeadingZeros' (Block b) i = loop i 0
  where loop !i !acc =
          let (wordI,bitI') = i `divMod` 8
              bitI = 7-bitI'
              word = b!wordI
              zeros = leadingZeros word - (8 - bitI')
          in if (wordI > snd (bounds b))
             then Nothing
             else 
                  then loop (i+1) (acc+1)
                  else Just acc
-}

fixedLeadingZeros :: Word8 -> Int
fixedLeadingZeros 0 = 8
fixedLeadingZeros w = fromIntegral $ leadingZeros w

myLeadingZeros' :: Block -> Int -> Maybe Int
myLeadingZeros' b i
  | i >= bitLength b = Nothing
myLeadingZeros' (Block arr) i =  zeros
  where 
    (wordI,bitI') = i `divMod` 8
    bitI = 8-bitI'
    word = arr!wordI
    start = word .&. ones bitI
    lzeros = (fixedLeadingZeros start) - bitI'
    firstzeros = lzeros
    zeros = if start == 0 
            then loop firstzeros (wordI+1)
            else Just firstzeros
    maxWi = snd (bounds arr)
    loop acc wi 
      | wi > maxWi = Nothing
      | otherwise  = case fixedLeadingZeros (arr!wi)
                     of 8 -> loop (acc + 8) (wi + 1)
                        i -> Just (acc + fromIntegral i)
        
myLeadingZeros'' :: [Bool] -> Int -> Maybe Int
myLeadingZeros'' bs i =
  case break id (drop i bs)
  of (_,[]) -> Nothing
     (a,b)  -> Just (length a)
        
prop_myLeadingZeros' block =
  forAll (choose (0,bitLength block-1)) $ \i ->
    myLeadingZeros' block i == myLeadingZeros'' (blockToBits block) i
        
readElias :: Block -> Int -> Maybe (Int,Int)
readElias b index =
  case myLeadingZeros' b index of
    Nothing -> Nothing
    Just 0 -> Just (0,index+1)
    Just ll -> 
      let lcode = readCode b (index+ll) ll
          l = fromIntegral $ getCode lcode
          code = readCode b (index+ll+ll) (l-1)
          finalcode = (Code 1 1) +++ code
          out = fromIntegral $ getCode finalcode
      in Just (out,index+ll+ll+l-1)
        
        
prop_read_write_elias = 
  forAll (choose (0,8007199254740992)) $ \i -> 
    let code = elias_encode i
        block = makeBlock [code]
        Just (out,len) = readElias block 0
    in (i == out && len == fromIntegral (codelength code))

readEliass' :: Block -> Int -> [Int]
readEliass' block i = loop i
  where loop i = case readElias block i
                 of Just (val,i') -> val:loop i'
                    Nothing -> []
                    
readEliass block = readEliass' block 0                    

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
--gapDecode [] = []
gapDecode (x:xs) =
  replicate x False ++ concatMap (\i -> True:replicate i False) xs

unGapBlock :: Block -> [Bool]
unGapBlock = gapDecode . readEliass

blockGaps :: Block -> [Int]
blockGaps = readEliass

prop_gap_block xs =
  xs == unGapBlock (gapBlock xs)


{-
elias_decode :: Code -> Int
elias_decode (Code length code) = if ll == 0 then 0 else i
  where ll = leadingZeros code - (64-length)
  -}