#!/usr/bin/env stack
{- stack script --resolver lts-16.31 --package array --package bytestring  --package vector -}

{- ORMOLU_DISABLE -}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, QuantifiedConstraints, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Array.ST
import Data.Array.Unboxed (UArray)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as VU

{- ORMOLU_ENABLE -}

getLineIntList :: IO [Int]
getLineIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

-- Helped by @kakkun61, @gksato
tabulateST :: forall i e. (Ix i, forall s. MArray (STUArray s) e (ST s)) => (forall s. STUArray s i e -> i -> ST s e) -> (i, i) -> e -> UArray i e
tabulateST f bounds_ e0 =
  runSTUArray uarray
  where
    uarray :: forall s. MArray (STUArray s) e (ST s) => ST s (STUArray s i e)
    uarray = do
      tbl <- newArray bounds_ e0 :: ST s (STUArray s i e)
      forM_ (range bounds_) $ \i -> do
        e <- f tbl i
        writeArray tbl i e
      return tbl

main :: IO ()
main = do
  [nItems, wLimit] <- getLineIntList
  wvs <- VU.replicateM nItems $ (\[a, b] -> (a, b)) <$> getLineIntList

  let dp = tabulateST f rng (0 :: Int)
      rng = ((0, 0), (nItems, wLimit))
      f :: STUArray s (Int, Int) Int -> (Int, Int) -> ST s Int
      f _ (0, _) = return 0
      f tbl (i, w) = do
        let wv = wvs VU.! (i - 1)
        v1 <- readArray tbl (i - 1, w)
        v2 <- if w - fst wv >= 0 then (snd wv +) <$> readArray tbl (i - 1, w - fst wv) else return 0
        return $ max v1 v2

  print $ maximum [dp ! (nItems, w) | w <- [0 .. wLimit]]
