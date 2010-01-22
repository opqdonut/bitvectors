{-# LANGUAGE BangPatterns #-}

module Static
  where

import Data.Array.Unboxed
import Data.Array.ST
import Control.Parallel.Strategies
--import qualified Data.StorableVector as V
import Data.List
import Debug.Trace

type Rank = Int
type Loc = Int
type V = UArray Int

data SuperBlock = SuperBlock {
      bits  :: !(V Bool),
      start :: !Loc,
      startrank :: ! Rank,
      blocklocations :: ! (V Loc),
      blockranks :: ! (V Rank) }
  deriving Show

data StaticVector = StaticVector {
      blength :: !Loc,
      slength :: !Loc,
      supers :: !(Array Int SuperBlock) }
                  
cut :: Int -> [a] -> [[a]]
cut n xs = go xs
    where go [] = []
          go xs = take n xs : go (drop n xs)
--takeWhile (not.null) . map (take n) $ iterate (drop n) xs

count :: (a -> Bool) -> [a] -> Int
count f xs = length $ filter f xs

rank = count id

partialsums = scanl (+) 0

--(!) = V.index

ilog2 :: Int -> Int
ilog2 n = floor (logBase 2 (fromIntegral n) + 1)

infixl 7 `mydiv`
mydiv a b = let (x,y) = quotRem a b in
            if y==0 then x else x+1

listArray' n xs = listArray (0,n-1) xs


mapAccumLArray :: Int -> (acc -> x -> (acc, y)) -> acc -> [x]
               -> Array Int y
mapAccumLArray n f init xs =
    runSTArray $ do
      arr <- newArray_ (0,n-1)
      let loop i s (x:xs) = do let (s',y) = f s x
                               y `seq` writeArray arr i y
                               loop (i+1) s' xs
          loop _ _ [] = return ()
      loop 0 init xs
      return arr
            
-- -- -- --

encodeBlock :: [Bool] -> [Bool]
encodeBlock = id
decodeBlock = id        

staticVector' :: Int -> (Loc,Rank) -> [Bool]
                 -> ((Loc,Rank),SuperBlock)
staticVector' blength (l,r) vals =
    let newl = l + length vals
        newr = r + rank vals
        nb = length vals `mydiv` blength

        blocks = cut blength vals
        ranks = partialsums $ map rank blocks

        locs = partialsums $ map (length.encodeBlock) blocks
    in ((newl,newr),
        SuperBlock 
          (listArray' (last locs) (concatMap encodeBlock blocks))
          l r
          (listArray' (nb+1) locs)
          (listArray' (nb+1) ranks))

staticVector :: Int -> [Bool] -> StaticVector
staticVector n vals =
    let slength = (ilog2 n)^2
        blength = ilog2 n `mydiv` 2
        supers =
            {-# SCC "mAL" #-}
            mapAccumLArray (n`mydiv`slength)
                           (staticVector' blength)
                           (0,0)
                           (cut slength vals)

    in StaticVector blength slength supers
       

address :: StaticVector -> Int -> (Int,Int,Int)
address s i = 
    let (superi,rem) = quotRem i (slength s)
        (blocki,biti) = quotRem rem (blength s)
    in (superi,blocki,biti)

retrieveblock :: StaticVector -> Int -> Int -> [Bool]
retrieveblock s superi blocki =
    let super = supers s ! superi
        blockloc = (blocklocations super ! blocki)
        blocklength = (blocklocations super ! (blocki+1))
                      - (blocklocations super ! blocki)
        encodedblock = map (bits super!) [blockloc..blockloc+blocklength-1]
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

test = let n = 8*1024*10
       in staticVector n $ take n (cycle [True,False,False,False])
