{-# LANGUAGE BangPatterns #-}

module Static (staticVector_gap,StaticVector(),BitVector(..),
               blength,slength,blocklocations,supers)
  where

import Encoding2 hiding (Encoded,encode,decode)
import Util
import BitVector

import Data.Array.Unboxed
import Data.Array.ST
import Control.Parallel.Strategies
--import qualified Data.StorableVector as V
import Data.List
import Debug.Trace

type Rank = Int
type Loc = Int
type V = UArray Int

type Encode = [Bool] -> [Code]
type Decode = Block -> Int -> [Bool]

data SuperBlock = SuperBlock {
      bits  :: !Block,
      start :: !Loc,
      startrank :: ! Rank,
      blocklocations :: ! (V Loc),
      blockranks :: ! (V Rank) }
  deriving Show

data StaticVector = StaticVector {
      encode :: Encode,
      decode :: Decode,
      blength :: !Loc,
      slength :: !Loc,
      supers :: !(Array Int SuperBlock) }

instance BitVector StaticVector where
  query = _query
  queryrank = _queryrank
  select = undefined
  querysize = undefined --- XXX
                
instance Construct StaticVector where
  construct = staticVector_gap

rank :: [Bool] -> Rank                  
rank = count id

partialsums :: [Loc] -> [Loc]
partialsums = scanl (+) 0

mapAccumLArray :: Int -> (acc -> x -> (acc, y)) -> acc -> [x]
               -> Array Int y
mapAccumLArray n f init xs =
    runSTArray $ do
      arr <- newArray_ (0,n-1)
      let loop i _ _
            | i==n = return ()
          loop _ _ [] = return ()
          loop i s (x:xs) =
              {-# SCC "loop" #-}
              do let (s',y) = f s x
                 y `seq` writeArray arr i y
                 loop (i+1) s' xs
      loop 0 init xs
      return arr
            
unfoldrArray :: Int -> (b -> (b,a)) -> b -> Array Int a
unfoldrArray n f init =
  runSTArray $ do
    arr <- newArray_ (0,n-1)
    let loop i s
          | i==n = return ()
          | otherwise =
            do let (s',y) = f s
               y `seq` writeArray arr i y
               loop (i+1) s'
    loop 0 init
    return arr

-- -- -- --

staticVector' :: Int              -- superblock length
                 -> Int           -- block length
                 -> Encode        -- encoding function
                 -> (Loc,Rank,[Bool]) -- aggregated location and rank, bits
                 -> ((Loc,Rank,[Bool]),SuperBlock)
staticVector' slength blength encode (l,r,bits) =
    let newl = l + length vals
        newr = r + rank vals
        (vals,rest) = splitAt slength bits
        nb = length vals `mydiv` blength

        chopped = cut blength vals

        ranks = partialsums . map rank $ chopped
        locs = partialsums . map (sum.map (fromIntegral.codelength).encode)
               $ chopped
    in --trace ("ratio "++show (length vals,last locs))
      ((newl,newr,rest),
        SuperBlock 
          ({-# SCC "bits" #-} makeBlock (concatMap encode chopped))
          l r
          ({-# SCC "locs" #-} listArray' (nb+1) locs)
          ({-# SCC "ranks" #-} listArray' (nb+1) ranks))

{-
staticVector_ord :: Int -> [Bool] -> StaticVector
staticVector_ord n =
    let slength = (ilog2 n)^2
        -- we want blength to be 2^k-1 to make order encoding perform well
        blength = pred . roundUpToPowerOf 2 $
                  slength `mydiv` (2 * ilog2 n)
        (enc,dec) = order blength
    in staticVector_prim n slength blength (enc,dec)
-}

staticVector_gap :: Int -> [Bool] -> StaticVector
staticVector_gap n =
    let slength = (ilog2 n)^2
        -- we want blength to be 2^k
        blength = roundUpToPowerOf 2 $ slength `mydiv` (2 * ilog2 n)
        (enc,dec) = (eliasEncode . gapify,
                     \b i -> unGapify $ readEliass' b i)
    in staticVector_prim n slength blength (enc,dec)

staticVector_prim :: Int -> Int -> Int
                     -> (Encode,Decode)
                     -> [Bool]
                  -> StaticVector
staticVector_prim n slength blength (enc,dec) vals =
    let supers =
            {-# SCC "supers" #-}
            unfoldrArray (n`mydiv`slength)
                         (staticVector' slength blength enc)
                         (0,0,vals)

    in StaticVector enc dec blength slength supers
       

address :: StaticVector -> Int -> (Int,Int,Int)
address s i = 
    let (superi,rem) = quotRem i (slength s)
        (blocki,biti) = quotRem rem (blength s)
    in (superi,blocki,biti)

-- TODO: no need to decode here, length and rank are recoverable from encoding.
-- rewrite as loops.

retrieveblock :: StaticVector -> Int -> Int -> [Bool]
retrieveblock s superi blocki =
    let super = supers s ! superi
        blockloc = (blocklocations super ! blocki)
        blocklength = (blocklocations super ! (blocki+1))
                      - (blocklocations super ! blocki)
    in decode s (bits super) blockloc

_query :: StaticVector -> Int -> Bool
_query s i =
    let (superi,blocki,biti) = address s i
        super = supers s ! superi
        block = retrieveblock s superi blocki
    in block !! biti
          
_queryrank s i =
    let (superi,blocki,biti) = address s i
        super = supers s ! superi
        blockrank = blockranks super ! blocki
        block = retrieveblock s superi blocki
    in  startrank super + blockrank + rank (take biti block)

test = let n = 8*1024*10
       in staticVector_gap n $ take n (cycle [True,False,False,False])
