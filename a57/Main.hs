#!/usr/bin/env stack
-- stack script --resolver lts-16.11 --package bytestring --package vector --package vector-algorithms --package containers --package array --package primitive

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- {-# LANGUAGE TypeSynonymInstances #-}

-- {{{ Imports

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Char
import Data.List
import Data.Maybe

{- ORMOLU_DISABLE -}

-- bytestring: https://www.stackage.org/lts-16.11/package/bytestring-0.10.10.0
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS

-- vector: https://www.stackage.org/lts-16.11/package/vector-0.12.1.2
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

{- ORMOLU_ENABLE -}

-- }}}

-- {{{ Segment tree

-- | Segment tree, backed by a binary tree.
data SegmentTree f s = SegmentTree f (VUM.MVector s Int)

-- | `SegmentTree` folding function.
class FF a where
  foldFunction :: a -> Int -> Int -> Int

-- | Updates a `SegmentTree` node value and their parents up to top root.
updateNode :: (FF f, PrimMonad m) => SegmentTree f (PrimState m) -> Int -> Int -> m ()
updateNode tree@(SegmentTree _ vec) i x = do
  VUM.write vec i x
  updateParent tree (x `div` 2)

-- | (Internal) Recursive parent node update.
updateParent :: (FF f, PrimMonad m) => SegmentTree f (PrimState m) -> Int -> m ()
updateParent _ 0 = return ()
updateParent tree@(SegmentTree f vec) x = do
  c1 <- VUM.read vec (x * 2)
  c2 <- VUM.read vec (x * 2 + 1)
  updateNode tree x (foldFunction f c1 c2)

-- | Range Maximum Querie: `SegmentTree` of maximums
newtype RMQ s = RMQ (SegmentTree FoldWithMax s)

type IORMQ = RMQ RealWorld

type STRMQ s = RMQ s

-- | `FoldMethod` of `RMQ`
data FoldWithMax = FoldWithMax

instance FF FoldWithMax where
  foldFunction _ = max

-- | Creates a new RMQ from a depth
newRMQ :: (PrimMonad m) => Int -> m (RMQ (PrimState m))
newRMQ depth = RMQ . SegmentTree FoldWithMax <$> VUM.replicate (2 ^ depth) 0

-- | Range Sum Queries: `SegmentTree` of sums
newtype RSQ s = RSQ (SegmentTree FoldWithSum s)

type IORSQ = RSQ RealWorld

type STRSQ s = RSQ s

-- | Creates a new RSQ from a depth
newRSQ :: (PrimMonad m) => Int -> m (RSQ (PrimState m))
newRSQ depth = RSQ . SegmentTree FoldWithSum <$> VUM.replicate (2 ^ depth) 0

-- | `FoldMethod` of `RSQ`
data FoldWithSum = FoldWithSum

instance FF FoldWithSum where
  foldFunction _ = (+)

-- }}}

getLineIntList :: IO [Int]
getLineIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getLineIntVec :: IO (VU.Vector Int)
getLineIntVec = VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [n, q] <- getLineIntList
  return ()
