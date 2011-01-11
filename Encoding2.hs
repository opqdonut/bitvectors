{-# LANGUAGE RankNTypes,BangPatterns,FlexibleInstances,MultiParamTypeClasses #-}

module Encoding2
  where

import Data.List (isPrefixOf,unfoldr)
import Numeric (showHex)
import Data.Bits
import Data.Bits.Extras
import Data.Word
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.ST

import Debug.Trace

import Test.QuickCheck hiding ((.&.))
                              
import Measure (SizeRank(..))
import Data.FingerTree (Measured(..))
  
import Data.Monoid

import Util
import BitVector

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
                 .|. getCode b)
                
prop_plusplusplus :: Code -> Code -> Bool
prop_plusplusplus a b 
  | codelength a + codelength b > 64 = True
  | otherwise = codeToBits (a+++b) == codeToBits a ++ codeToBits b
  
instance Arbitrary Code where
  arbitrary = do code <- fmap fromIntegral (arbitrary :: Gen Integer)
                 codelength <- fmap fromIntegral (choose (0,64) :: Gen Integer)
                 return (Code codelength code)
  shrink (Code len code) = do l <- shrink len
                              return (Code l code)

{-# SPECIALIZE ones :: Int -> Word8 #-}
{-# SPECIALIZE ones :: Int -> Word64 #-}
ones :: Bits a => Int -> a
ones i = setBit 0 i - 1

elias_encode :: Gap -> Code
elias_encode (Gap 0) = Code 1 1
elias_encode (Gap i)
  | l+ll+ll-1 > 64 = error "number too big for elias_encode"
  | otherwise = code
  where l  = ilog2 i
        ll = ilog2 l
        icode  = Code (fromIntegral $ l-1)  (fromIntegral i .&. ones (l-1))
        lcode  = Code (fromIntegral $ ll-1) (fromIntegral l .&. ones (ll-1))
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
      s w = showHex w "" ++ ";"

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
           shiftR (getCode c) (fromIntegral length-nWrite) 
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
  
myLeadingZeros :: Code -> Maybe Int
myLeadingZeros c = if getCode c == 0
                   then Nothing
                   else Just (fixedLeadingZeros (getCode c)
                              - (64 - fromIntegral (codelength c)))

-- "bug": leadingZeros 0 === leadingZeros 1 === bitlength - 1
fixedLeadingZeros :: ExtraBits a => a -> Int
fixedLeadingZeros w@0 = bitSize w
fixedLeadingZeros w = fromIntegral $ leadingZeros w

readElias :: Block -> Int -> Maybe (Gap,Int)
readElias b index =
  let code = (readCode b index 64)
      len = fromIntegral $ codelength code
  in case myLeadingZeros code
     of Nothing -> Nothing
        Just 0 -> Just (Gap 0,index+1)
        Just ll ->
          let l = fromIntegral $ shiftR (getCode code) (len-2*ll)
              almost = ones (l-1) .&. shiftR (getCode code) (len-(2*ll+l-1))
              final = setBit almost (l-1)
          in Just (Gap $ fromIntegral final, index+2*ll+l-1)
    
    
prop_read_write_elias = 
  forAll (choose (0,8007199254740992)) $ \i -> 
    let g = Gap i
        code = elias_encode g
        block = makeBlock [code]
        Just (out,len) = readElias block 0
    in (g == out && len == fromIntegral (codelength code))

readEliass' :: Block -> Int -> [Gap]
readEliass' block = unfoldr (readElias block)
                    
readEliass block = readEliass' block 0                    

prop_read_eliass =
  forAll (listOf1 $ choose (0,8007199254740992)) $ \is ->
    let gs = map Gap is
    in gs == (readEliass . makeBlock . map elias_encode) gs

gapEncode_ :: (Gap->Code) -> Code -> [Bool] -> [Code]
gapEncode_ enc terminator xs = loop xs 0
  --- XXX could use construct for [Gap]
  where
    loop [] !acc         = [enc (Gap acc), terminator]
    loop (True:xs) !acc  = enc (Gap acc) : loop xs 0
    loop (False:xs) !acc = loop xs (acc+1)
    
gapEncode :: [Bool] -> [Code]
gapEncode = gapEncode_ elias_encode (Code 0 0)

gapBlock :: [Bool] -> Block
gapBlock = makeBlock . gapEncode

gapDecode :: [Gap] -> [Bool]
--gapDecode [] = []
gapDecode (Gap x:xs) =
  replicate x False ++ concatMap (\i -> True:replicate (unGap i) False) xs

unGapBlock :: Block -> [Bool]
unGapBlock = gapDecode . readEliass

prop_gap_block xs =
  xs == unGapBlock (gapBlock xs)


----


nibbleEncode :: Gap -> Code
nibbleEncode (Gap i)
  | i<2^3 = Code 4 (fromIntegral i)
  | otherwise = go base (shiftR (fromIntegral i) 3)
    where base = Code 4 (fromIntegral i .&. ones 3)
          snip i = Code 4 (setBit 0 3 .|. i .&. ones 3)
          go acc 0 = acc 
          go acc i = go (snip i +++ acc) (shiftR i 3)

nibbleTerminator :: Int
nibbleTerminator = 8

nibbleTerminatorCode :: Code
nibbleTerminatorCode = Code 4 8

nibble :: Block -> Int -> Int
nibble (Block a) i = fromIntegral $
                     let (wi,bi) = i `divMod` 8 
                         w = a!wi
                     in case bi of
                       0 -> w `shiftR` 4
                       4 -> w .&. ones 4

readNibble :: Block -> Int -> Maybe (Gap,Int)
readNibble b i = if nibble b i == 8
                 then Nothing
                 else loop 0 i
  where loop !acc !i =
          let n = nibble b i
              value = n .&. ones 3
              acc' = (acc `shiftL` 3) .|. value
          in case testBit n 3 of
            False -> Just (Gap acc',i+4)
            True -> loop acc' (i+4)
            
prop_nibble = 
  forAll (choose (0,2^30)) $ \i ->
    let Just (Gap j,_) = readNibble (makeBlock [nibbleEncode (Gap i)]) 0
    in j == i
       
readNibbles :: Block -> [Gap]
readNibbles block = unfoldr (readNibble block) 0

prop_readNibbles =
  forAll (listOf1 $ choose (0,2^30)) $ \is ->
    let gs = map Gap is
        out = (readNibbles 
               . makeBlock
               . (++[nibbleTerminatorCode])
               . map nibbleEncode) gs
    in gs == out

gapNibble = gapEncode_ nibbleEncode nibbleTerminatorCode
nibbleBlock = makeBlock . gapNibble
unNibbleBlock = gapDecode . readNibbles

prop_nibble_block xs =
  xs == unNibbleBlock (nibbleBlock xs)


{-
elias_decode :: Code -> Int
elias_decode (Code length code) = if ll == 0 then 0 else i
  where ll = leadingZeros code - (64-length)
  -}
-----


-----

newtype EBlock = EBlock {unEBlock :: Block}
newtype NBlock = NBlock {unNBlock :: Block}
data UBlock = UBlock {ubitlength :: !Int, unUBlock :: !Block}

class Encoded a where
  decode :: a -> [Bool]
  encode :: [Bool] -> a
  encodedSize :: a -> Int
  
  combine :: a -> a -> a
  combine x y = encode (decode x ++ decode y)
  
  cleave :: a -> (a,a)
  cleave b = let bits = decode b
                 n = length bits
                 (x,y) = splitAt (n `div` 2) bits
             in (encode x, encode y)
  

instance Encoded EBlock where
  decode = unGapBlock . unEBlock
  encode = EBlock . gapBlock
  encodedSize = bitLength . unEBlock
  
instance Encoded NBlock where
  decode = unNibbleBlock . unNBlock
  encode = NBlock . nibbleBlock
  encodedSize = bitLength . unNBlock
  
instance Encoded UBlock where
  decode b = take (ubitlength b) $ blockToBits (unUBlock b)
  encode xs = UBlock (length xs) . makeBlock . map f . cut 64 $ xs
    where
      f xs = Code (fromIntegral $ length xs)
                  (fromIntegral . unbitify . reverse $ xs)
  encodedSize = ubitlength
      
instance Measured SizeRank EBlock where
  measure = measure . readEliass . unEBlock

instance Measured SizeRank NBlock where
  measure = measure . readNibbles . unNBlock 
         
--- XXX could be more efficient
instance Measured SizeRank UBlock where
  measure (UBlock siz block) =
    let bits = take siz $ blockToBits block
        rank = rank' bits
    in SizeRank siz rank
    
       

instance BitVector EBlock where
  construct _ = encode
  query b i = let gaps = readEliass (unEBlock b)
              in query gaps i
  queryrank b i = let gaps = readEliass (unEBlock b)
                  in queryrank gaps i
  select b i = let gaps = readEliass (unEBlock b)
               in select gaps i
  querysize = querysize . readEliass . unEBlock
  
instance BitVector NBlock where
  construct _ = encode
  query b i = let gaps = readNibbles (unNBlock b)
              in query gaps i
  queryrank b i = let gaps = readNibbles (unNBlock b)
                  in queryrank gaps i
  select b i = let gaps = readNibbles (unNBlock b)
               in select gaps i
  querysize = querysize . readEliass . unNBlock
  
instance BitVector UBlock where
  construct _ = encode
  query b i = query (decode b) i
  queryrank b i = queryrank (decode b) i
  select b i = select (decode b) i
  querysize = querysize . decode