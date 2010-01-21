module Static
  where

--import Data.Array
import Data.Array.Unboxed
import Data.List
import Debug.Trace

type Rank = Int
type Loc = Int

data Blocks = Blocks
    {  }

data SuperBlock = SuperBlock
    { 
      start :: Loc,
      startrank :: Rank,
      blocklocations :: UArray Int Loc,
      blockranks :: UArray Int Rank}
  deriving Show

data StaticVector = StaticVector
    { blength :: Loc,
      slength :: Loc,
      bits   :: UArray Int Bool,
      supers :: Array Int SuperBlock }
    
cut :: Int -> [a] -> [[a]]
cut n xs = takeWhile (not.null) . map (take n) $ iterate (drop n) xs

count :: (a -> Bool) -> [a] -> Int
count f xs = length $ filter f xs

rank = count id

partialsums = scanl (+) 0

ilog2 :: Int -> Int
ilog2 n = floor (logBase 2 (fromIntegral n))

listArray' xs = listArray (0,length xs-1) xs



encodeBlock :: [Bool] -> [Bool]
encodeBlock = id
decodeBlock = id        

staticVector' :: Int -> (Loc,Rank) -> [Bool]
                 -> ((Loc,Rank),([Bool],SuperBlock))
staticVector' blength (l,r) vals =
    let newl = l + length vals
        newr = r + rank vals

        blocks = cut blength vals
        ranks = partialsums $ map rank blocks

        encoded = map encodeBlock blocks
        locs = partialsums $ map length encoded
    in ((newl,newr),
        (concat encoded,
         SuperBlock l r (listArray' locs) (listArray' ranks)))

staticVector :: Int -> [Bool] -> StaticVector
staticVector n vals =
    let slength = (ilog2 n)^2
        blength = ilog2 n `div` 2

        (_, done) = mapAccumL (staticVector' blength) (0,0) $ cut slength vals
        (bitss, supers) = unzip done

    in StaticVector
         blength
         slength
         (listArray' (concat bitss))
         (listArray' supers)

    
test = staticVector 100 $ take 100 (cycle [True,False,False,False])

address :: StaticVector -> Int -> (Int,Int,Int)
address s i = 
    let (superi,rem) = quotRem i (slength s)
        (blocki,biti) = quotRem rem (blength s)
    in (superi,blocki,biti)

retrieveblock :: StaticVector -> Int -> Int -> [Bool]
retrieveblock s superi blocki =
    let super = supers s ! superi
        blockloc = start super + (blocklocations super ! blocki)
        blocklength = (blocklocations super ! (blocki+1))
                      - (blocklocations super ! blocki)
        encodedblock = map (bits s!) [blockloc..blockloc+blocklength-1]
    in decodeBlock encodedblock

query :: StaticVector -> Int -> Bool
query s i =
    let (superi,blocki,biti) = address s i
        super = supers s ! superi
        block = retrieveblock s superi blocki
    in block !! biti
          
queryrank s i =
    let (superi,blocki,biti) = address s i
        super = supers s ! superi
        blockrank = blockranks super ! blocki
        block = retrieveblock s superi blocki
    in  startrank super + blockrank + rank (take biti block)
