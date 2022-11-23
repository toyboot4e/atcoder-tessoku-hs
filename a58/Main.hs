#!/usr/bin/env stack
-- stack script --resolver lts-16.11 --package bytestring --package vector --package vector-algorithms --package containers --package array --package primitive

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

-- bits: https://www.stackage.org/lts-16.11/package/bits-0.5.2
import Data.Bits

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

-- {{{ Bits

-- | Log base of two or bit floor.
-- | <https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Bits.html#v:countLeadingZeros>
log2 :: (FiniteBits b) => b -> Int
log2 x = finiteBitSize x - 1 - countLeadingZeros x

-- | Ceiling of log base 2 of an `Int`.
-- |
-- | # Example
-- |
-- | ```hs
-- | > log2 3
-- | 1
-- | > log2CeilInt 3
-- | 2
-- | ```
log2CeilInt :: Int -> Int
log2CeilInt x = msb + ceiling
  where
    msb = log2 x
    ceiling = if (clearBit x msb) > 0 then 1 else 0

-- | Calculates the smallest integral power of two that is not smaller than `x`.
-- |
-- | # Example
-- |
-- | ```hs
-- | > bitCeil 3
-- | 4
-- | ```
bitCeil :: Int -> Int
bitCeil = bit . log2CeilInt

-- }}}

-- {{{ Segment tree

-- | A mutable segment tree backed by a complete binary tree.
-- |
-- | # Overview
-- |
-- | A segment tree is a cache of a communicative folding function.
-- | Each node corresponds to a folding range and the node contains the folding result.
-- |
-- | A segment tree instance is once initialized and never be resized.
-- |
-- | # Operations
-- |
-- | Modification takes $O(log N)$, so creation takes $N(log N)$.
-- | Retrieval takes $O(log N)$.
-- |
-- | # (Internal) Indices
-- |
-- | The complete binary tree has `2 ^ depth - 1` elements.
-- |
-- | - Child elements of a parent node `i` has index `2 * i + 1` and `2 * i + 2`.
-- | - The leaf value indices starts with `length / 2 - 1`.
-- |
-- | Example:
-- |
-- | ```
-- |            0
-- |      1           2
-- |   3     4     5     6
-- | 07 08 09 10 11 12 13 14
-- | ```
data MSegmentTree s a = MSegmentTree (a -> a -> a) (VUM.MVector s a)

-- TODO: Generic queries and immutable segment tree (with `Show` instance)

-- | Creates a new segment tree for `n` leaves.
newTree :: (VUM.Unbox a, PrimMonad m) => (a -> a -> a) -> Int -> a -> m (MSegmentTree (PrimState m) a)
newTree !f !n !value = MSegmentTree f <$> VUM.replicate n' value
  where
    !n' = shiftL (bitCeil n) 1

-- | Updates an `MSegmentTree` leaf value and their parents up to top root.
updateLeaf :: (VU.Unbox a, PrimMonad m) => MSegmentTree (PrimState m) a -> Int -> a -> m ()
updateLeaf tree@(MSegmentTree _ vec) !i !value = _updateElement tree i' value
  where
    -- length == 2 * (the number of the leaves)
    !offset = (VUM.length vec) `div` 2 - 1
    !i' = i + offset

-- | (Internal) Updates an `MSegmentTree` element (node or leaf) value and their parents up to top root.
_updateElement :: (VU.Unbox a, PrimMonad m) => MSegmentTree (PrimState m) a -> Int -> a -> m ()
_updateElement tree@(MSegmentTree _ vec) !i !value = do
  VUM.write vec i value
  _updateParent tree ((i - 1) `div` 2)

-- | (Internal) Recursivelly updates the parent nodes.
_updateParent :: (VU.Unbox a, PrimMonad m) => MSegmentTree (PrimState m) a -> Int -> m ()
_updateParent _ (-1) = pure () -- REMARK: (-1) `div` 2 == 0
_updateParent _ 0 = pure ()
_updateParent tree@(MSegmentTree f vec) !iParent = do
  !c1 <- VUM.read vec (iParent * 2 + 1)
  !c2 <- VUM.read vec (iParent * 2 + 2)
  _updateElement tree iParent (f c1 c2)

-- | Retrieves the folding result over the inclusive range `[l, r]` from `MSegmentTree`.
queryByRange :: forall a m. (VU.Unbox a, PrimMonad m) => MSegmentTree (PrimState m) a -> (Int, Int) -> m a
queryByRange (MSegmentTree !f !vec) (!lo, !hi) = fromJust <$> loop 0 (0, initialHi)
  where
    !initialHi = (VUM.length vec) `div` 2 - 1
    loop :: Int -> (Int, Int) -> m (Maybe a)
    loop !i (!l, !h)
      | lo <= l && h <= hi = Just <$> VUM.read vec i
      | h < lo || hi < l = pure Nothing
      | otherwise = do
        let d = (h - l) `div` 2
        !ansL <- loop (2 * i + 1) (l, l + d)
        !ansH <- loop (2 * i + 2) (l + d + 1, h)
        pure . Just $ case (ansL, ansH) of
          (Just !a, Just !b) -> f a b
          (Just !a, _) -> a
          (_, Just !b) -> b
          (_, _) -> error "query error (segment tree)"

-- }}}

getLineIntList :: IO [Int]
getLineIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getLineIntVec :: IO (VU.Vector Int)
getLineIntVec = VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [n, q] <- getLineIntList
  qs <- V.replicateM q getLineIntList

  rmq <- newTree max n (0 :: Int)

  forM_ qs $ \q -> do
    case q of
      -- insert
      [1, (i + 1), value] -> do
        updateLeaf rmq i value
      -- get
      [2, (l + 1), (r + 1)] -> do
        !value <- queryByRange rmq (l, r - 1)
        print value

  pure ()
