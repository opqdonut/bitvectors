{-# LANGUAGE BangPatterns #-}

module Static (staticVector_ord,staticVector_gap,StaticVector(),BitVector(..),
               blength,slength,blocklocations,supers)
  where

import Encoding
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

data SuperBlock = SuperBlock {
      bits  :: !(V Bool),
      start :: !Loc,
      startrank :: ! Rank,
      blocklocations :: ! (V Loc),
      blockranks :: ! (V Rank) }
  deriving Show

data StaticVector = StaticVector {
      encode :: [Bool] -> [Bool],
      decode :: [Bool] -> [Bool],
      blength :: !Loc,
      slength :: !Loc,
      supers :: !(Array Int SuperBlock) }

instance BitVector StaticVector where
    query = _query
    queryrank = _queryrank
    construct = staticVector_gap
    select = undefined
                
rank :: [Bool] -> Rank                  
rank = count id

partialsums :: [Loc] -> [Loc]
partialsums = scanl (+) 0

mapAccumLArray :: Int -> (acc -> x -> (acc, y)) -> acc -> [x]
               -> Array Int y
mapAccumLArray n f init xs =
    runSTArray $ do
      arr <- newArray_ (0,n-1)
      let loop _ _ [] = return ()
          loop i s (x:xs) =
              {-# SCC "loop" #-}
              do let (s',y) = f s x
                 y `seq` writeArray arr i y
                 loop (i+1) s' xs
      loop 0 init xs
      return arr
            
-- -- -- --

staticVector' :: Int              -- block length
              -> ([Bool]->[Bool]) -- encoding function
              -> (Loc,Rank)       -- aggregated location and rank
              -> [Bool]           -- the bits in this superblock
              -> ((Loc,Rank),SuperBlock)
staticVector' blength encode (l,r) vals =
    let newl = l + length vals
        newr = r + rank vals
        nb = length vals `mydiv` blength

        ranks = partialsums . map rank $ cut blength vals
        locs = partialsums . map (length.encode) $ cut blength vals
    in --trace ("ratio "++show (length vals,last locs))
      ((newl,newr),
        SuperBlock 
          ({-# SCC "bits" #-} listArray' (last locs) (concatMap encode $ cut blength vals))
          l r
          ({-# SCC "locs" #-} listArray' (nb+1) locs)
          ({-# SCC "ranks" #-} listArray' (nb+1) ranks))

staticVector_ord :: Int -> [Bool] -> StaticVector
staticVector_ord n =
    let slength = (ilog2 n)^2
        -- we want blength to be 2^k-1 to make order encoding perform well
        blength = pred . roundUpToPowerOf 2 $
                  slength `mydiv` (2 * ilog2 n)
        (enc,dec) = order blength
    in staticVector_prim n slength blength (enc,dec)

staticVector_gap :: Int -> [Bool] -> StaticVector
staticVector_gap n =
    let slength = (ilog2 n)^2
        -- we want blength to be 2^k-1 to make order encoding perform well
        blength = roundUpToPowerOf 2 $ slength `mydiv` (2 * ilog2 n)
        (enc,dec) = (gap_encode,gap_decode) -- order blength
    in staticVector_prim n slength blength (enc,dec)

staticVector_prim :: Int -> Int -> Int -> (Encoder,Decoder) -> [Bool]
                  -> StaticVector
staticVector_prim n slength blength (enc,dec) vals =
    let supers =
            {-# SCC "supers" #-}
            mapAccumLArray (n`mydiv`slength)
                           (staticVector' blength enc)
                           (0,0)
                           (cut slength vals)

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
        encodedblock = map (bits super!) [blockloc..blockloc+blocklength-1]
    in decode s encodedblock

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
