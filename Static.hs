module Static
  where

--import Data.Array
import Data.Array.Unboxed
import Data.List

type Rank = Int
type Loc = Int

data Blocks = Blocks
    { blocklocations :: UArray Int Loc,
      blockranks :: UArray Int Rank }

data SuperBlock = SuperBlock
    { 
      location :: Loc,
      rank :: Rank,
      blocks :: Blocks }

data StaticVector = StaticVector
    { blength :: Loc,
      slength :: Loc,
      bits   :: UArray Int Bool,
      supers :: Array Int SuperBlock }
    
cut :: Int -> [a] -> [[a]]
cut n xs = map (take n) $ iterate (drop n) xs

ilog2 :: Int -> Int
ilog2 n = floor (logBase 2 (fromIntegral n))

listArray' xs = listArray (0,length xs-1) xs



encodeBlock :: [Bool] -> [Bool]
encodeBlock = id



encodeSuper :: Int -> [Bool] -> (Blocks, [Bool])
encodeSuper n vals = 
   let encoded = map encodeBlock $ cut n vals
       lengths = map length encoded
       locs = scanl (+) 0 lengths
   in (Blocks
        (listArray' locs)
        undefined
      ,concat encoded)
        


staticVector :: Int -> [Bool] -> StaticVector
staticVector n vals =
    let slength = (ilog2 n)^2
        blength = ilog2 n `div` 2

        supers0 = cut slength vals
        (blocks, bitss) = unzip $ map (encodeSuper blength) supers0
        superlocs = scanl (+) 0 $ map length bitss

        supers :: [SuperBlock]
        supers = zipWith (\loc blocks -> SuperBlock loc undefined blocks)
                 superlocs blocks 

    in StaticVector
         blength
         slength
         (listArray' (concat bitss))
         (listArray' supers)

    
test = staticVector 100 $ take 100 (cycle [True,False,False,False])

query :: StaticVector -> Int -> Bool
query s i =
    let (superi,rem) = quotRem i (slength s)
        super = supers s ! superi
        (blocki,biti) = quotRem rem (blength s)
        blockloc = blocklocations (blocks super) ! blocki
        index = location super + blockloc + biti
    in bits s ! index
          