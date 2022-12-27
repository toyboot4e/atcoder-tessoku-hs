#!/usr/bin/env stack
{- stack script --resolver lts-16.11
--package array --package bytestring --package containers
--package hashable --package unordered-containers
--package vector --package vector-algorithms --package primitive --package transformers
-}

{- ORMOLU_DISABLE -}
{-# LANGUAGE BangPatterns, BlockArguments, LambdaCase, MultiWayIf, PatternGuards, TupleSections #-}
{-# LANGUAGE NumDecimals, NumericUnderscores #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{- ORMOLU_ENABLE -}

-- {{{ Imports

module Main (main) where

import Control.Monad
import Control.Monad.ST
import Data.Char
import Data.List

{- ORMOLU_DISABLE -}

-- array
import Data.Array.IArray
import Data.Array.IO
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unsafe

-- import qualified Data.Array as A
import Data.Array.Unboxed (UArray)

-- bytestring: https://www.stackage.org/lts-16.11/package/bytestring-0.10.10.0
import qualified Data.ByteString.Char8 as BS

-- vector: https://www.stackage.org/lts-16.11/package/vector-0.12.1.2
import qualified Data.Vector.Unboxed as VU

import qualified Data.IntMap.Strict as IM

import Data.Hashable

import qualified Data.HashMap.Strict as HM
{- ORMOLU_ENABLE -}

getLineIntList :: IO [Int]
getLineIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getLineIntVec :: IO (VU.Vector Int)
getLineIntVec = VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

{-# INLINE tabulateMap #-}
tabulateMap :: forall i e. (Ix i, Hashable i) => (HM.HashMap i e -> i -> e) -> (i, i) -> HM.HashMap i e -> HM.HashMap i e
tabulateMap f bounds_ cache0 =
  foldl' step cache0 (range bounds_)
  where
    step :: HM.HashMap i e -> i -> HM.HashMap i e
    step cache i =
      let e = f cache i
       in HM.insert i e cache

main :: IO ()
main = do
  [nItems, wLimit] <- getLineIntList
  wvs <- VU.replicateM nItems $ (\[a, b] -> (a, b)) <$> getLineIntList

  let dp = tabulateMap f bounds_ HM.empty
      bounds_ = ((0, 0), (nItems, wLimit))
      f :: HM.HashMap (Int, Int) Int -> (Int, Int) -> Int
      f _ (0, _) = 0
      f cache (i, w) =
        let wv = wvs VU.! (i - 1)
            v1 = cache HM.! (i - 1, w)
            v2 =
              if w - fst wv >= 0
                then (snd wv +) $ cache HM.! (i - 1, w - fst wv)
                else 0
         in max v1 v2

  print $ maximum [dp HM.! (nItems, w) | w <- [0 .. wLimit]]
