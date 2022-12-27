#!/usr/bin/env stack
{- stack script --resolver lts-16.11
--package array --package bytestring --package containers
--package vector --package vector-algorithms --package primitive --package transformers
-}

{- ORMOLU_DISABLE -}
{-# LANGUAGE BangPatterns, BlockArguments, LambdaCase, MultiWayIf, PatternGuards, TupleSections #-}
{-# LANGUAGE NumDecimals, NumericUnderscores #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{- ORMOLU_ENABLE -}

-- {{{ Imports

module Main (main) where

import Control.Monad
import Data.Char
import Data.List

{- ORMOLU_DISABLE -}

-- array
import Data.Array.IArray
import Data.Array.IO
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unsafe

import qualified Data.Array as A
import qualified Data.Array.Unboxed as AU

-- bytestring: https://www.stackage.org/lts-16.11/package/bytestring-0.10.10.0
import qualified Data.ByteString.Char8 as BS

-- vector: https://www.stackage.org/lts-16.11/package/vector-0.12.1.2
import qualified Data.Vector.Unboxed as VU

{- ORMOLU_ENABLE -}

getLineIntList :: IO [Int]
getLineIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getLineIntVec :: IO (VU.Vector Int)
getLineIntVec = VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

tabulateLazy :: Ix i => (i -> e) -> (i, i) -> Array i e
tabulateLazy f bounds_ = array bounds_ [(x, f x) | x <- range bounds_]

-- TODO: tabulateMemo

-- TODO: tabulateST?

tabulateIO :: forall i e. (Ix i) => (IOArray i e -> i -> IO e) -> (i, i) -> e -> IO (IOArray i e)
tabulateIO f bounds_ e0 = do
  -- FIXME: "Could not deduce (MArray IOUArray e IO)" with `IOUArray`
  tbl <- newArray bounds_ e0 :: IO (IOArray i e)
  forM_ (range bounds_) $ \i -> do
    e <- f tbl i
    writeArray tbl i e
  return tbl

main :: IO ()
main = do
  s1 <- BS.getLine
  s2 <- BS.getLine

  let dp = tabulateIO f rng (0 :: Int)
      rng = ((0, 0), (BS.length s2, BS.length s1))
      f :: IOArray (Int, Int) Int -> (Int, Int) -> IO Int
      f _ (_, 0) = return 0
      f _ (0, _) = return 0
      f tbl (i2, i1)
        | same  = (+ 1) <$> readArray tbl (i2 - 1, i1 - 1)
        | otherwise = do
            v1 <- readArray tbl (i2, i1 - 1)
            v2 <- readArray tbl (i2 - 1, i1)
            return $ max v1 v2
        where
          same = BS.index s1 (i1 - 1) == BS.index s2 (i2 - 1)

  subsets' <- dp
  subsets <- unsafeFreeze subsets' :: IO (AU.UArray (Int, Int) Int)

  print $ subsets ! (BS.length s2, BS.length s1)
