#!/usr/bin/env stack
{- stack script --resolver lts-16.31
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

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Control.Monad.Trans.State.Strict as State
import Data.Array
import Data.Bifunctor
import Data.Bits
import Data.Char
import Data.Functor
import Data.IORef
import Data.List
import Data.Maybe
import Data.Ord
import Debug.Trace
import GHC.Event (IOCallback)
import GHC.Exts
import GHC.Float (int2Float)
import System.IO
import Text.Printf

{- ORMOLU_DISABLE -}

-- array
import qualified Data.Array.Unboxed as AU
import qualified Data.Array.MArray as MA
import qualified Data.Array.IArray as IA
import qualified Data.Array.ST as STA

-- bytestring: https://www.stackage.org/lts-16.11/package/bytestring-0.10.10.0
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS

-- vector: https://www.stackage.org/lts-16.11/package/vector-0.12.1.2
import qualified Data.Vector.Fusion.Bundle as VFB
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

-- vector-algorithms: https://www.stackage.org/haddock/lts-16.31/vector-algorithms-0.8.0.3/Data-Vector-Algorithms-Intro.html
import qualified Data.Vector.Algorithms.Intro as VAI

-- containers: https://www.stackage.org/lts-16.11/package/containers-0.6.2.1
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

{- ORMOLU_ENABLE -}

-- }}}

-- {{{ cheatsheet

-- Option - Maybe cheatsheet
-- https://notes.iveselov.info/programming/cheatsheet-rust-option-vs-haskell-maybe

-- safe list access
-- safeHead :: [a] -> Maybe a
-- safeHead [] = Nothing
-- safeHead (a : as) = Just a
--
-- safeTail :: [a] -> Maybe [a]
-- safeTail [] = Nothing
-- safeTail (a : as) = Just as

-- sortWith
--
-- sortWithDesc :: Ord o => (a -> o) -> [a] -> [a]
-- sortWithDesc = sortBy . flip . comparing
--
-- maximumWith :: Foldable t => Ord o => (a -> o) -> t a -> a
-- maximumWith = maximumBy . comparing
--
-- minimumWith :: Foldable t => Ord o => (a -> o) -> t a -> a
-- minimumWith = minimumBy . comparing

-- }}}

-- {{{ Binary search

-- | Binary search for sorted items in an inclusive range (from left to right only)
-- |
-- | It returns the `(ok, ng)` index pair at the boundary.
-- |
-- | Example
-- | -------
-- |
-- | With an OK predicate `(<= 5)`, list `[0..9]` can be seen as:
-- |
-- | > [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
-- | >  <-------------->  <-------->
-- | >         ok             ng
-- |
-- | In this case `bsearch` returns the `(ok, ng)` = `(5, 6)` pair:
-- |
-- | > > let xs = [0..9] in do
-- | > >   print $ bsearch (0, 9) (\i -> xs !! i <= 5)
-- | > (5, 6)
bsearch :: (Int, Int) -> (Int -> Bool) -> (Maybe Int, Maybe Int)
bsearch (low, high) isOk = bimap wrap wrap (loop (low - 1, high + 1))
  where
    loop (ok, ng)
      | abs (ok - ng) == 1 = (ok, ng)
      | isOk m = loop (m, ng)
      | otherwise = loop (ok, m)
      where
        m = (ok + ng) `div` 2
    wrap :: Int -> Maybe Int
    wrap x
      | x == low - 1 || x == high + 1 = Nothing
      | otherwise = Just x

-- }}}

-- {{{ Union-Find tree

-- | Union-find implementation (originally by `@pel`)
newtype UnionFind s = UnionFind (VM.MVector s UfNode)

type IOUnionFind = UnionFind RealWorld

type STUnionFind s = UnionFind s

-- | `Child parent | Root size`. Not `Unbox` :(
data UfNode = Child {-# UNPACK #-} !Int | Root {-# UNPACK #-} !Int

-- | Creates a new Union-Find tree of the given size.
{-# INLINE newUF #-}
newUF :: (PrimMonad m) => Int -> m (UnionFind (PrimState m))
newUF n = UnionFind <$> VM.replicate n (Root 1)

-- | Returns the root node index.
{-# INLINE root #-}
root :: (PrimMonad m) => UnionFind (PrimState m) -> Int -> m Int
root uf@(UnionFind vec) i = do
  node <- VM.read vec i
  case node of
    Root _ -> return i
    Child p -> do
      r <- root uf p
      -- NOTE(perf): path compression (move the queried node to just under the root, recursivelly)
      VM.write vec i (Child r)
      return r

-- | Checks if the two nodes are under the same root.
{-# INLINE same #-}
same :: (PrimMonad m) => UnionFind (PrimState m) -> Int -> Int -> m Bool
same uf x y = liftM2 (==) (root uf x) (root uf y)

-- | Just an internal helper.
unwrapRoot :: UfNode -> Int
unwrapRoot (Root s) = s
unwrapRoot (Child _) = undefined

-- | Unites two nodes.
{-# INLINE unite #-}
unite :: (PrimMonad m) => UnionFind (PrimState m) -> Int -> Int -> m ()
unite uf@(UnionFind vec) x y = do
  px <- root uf x
  py <- root uf y
  when (px /= py) $ do
    sx <- unwrapRoot <$> VM.read vec px
    sy <- unwrapRoot <$> VM.read vec py
    -- NOTE(perf): union by rank (choose smaller one for root)
    let (par, chld) = if sx < sy then (px, py) else (py, px)
    VM.write vec chld (Child par)
    VM.write vec par (Root (sx + sy))

-- | Returns the size of the root node, starting with `1`.
{-# INLINE size #-}
size :: (PrimMonad m) => UnionFind (PrimState m) -> Int -> m Int
size uf@(UnionFind vec) x = do
  px <- root uf x
  unwrapRoot <$> VM.read vec px

-- }}}

-- {{{ Bits

-- TODO: super efficient bit operations

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
    ceiling = if clearBit x msb > 0 then 1 else 0

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
-- | A segment tree is a cache of a folding function.
-- | Each node corresponds to a folding range and the node contains the folding result.
-- |
-- | A segment tree has a constant size and never be resized.
-- |
-- | # Operations
-- |
-- | Modification takes $O(log N)$, so creation takes $N(log N)$.
-- | Lookup takes $O(log N)$.
-- |
-- | # (Internal) Indices
-- |
-- | The complete binary tree has `2 ^ depth - 1` elements.
-- |
-- | - Child elements of a parent node `i` has index `2 * i + 1` and `2 * i + 2`.
-- | - The leaf indices start with `length / 2 - 1`.
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
{-# INLINE newTree #-}
newTree :: (VUM.Unbox a, PrimMonad m) => (a -> a -> a) -> Int -> a -> m (MSegmentTree (PrimState m) a)
newTree !f !n !value = MSegmentTree f <$> VUM.replicate n' value
  where
    !n' = shiftL (bitCeil n) 1

-- | Updates an `MSegmentTree` leaf value and their parents up to top root.
{-# INLINE updateLeaf #-}
updateLeaf :: (VU.Unbox a, PrimMonad m) => MSegmentTree (PrimState m) a -> Int -> a -> m ()
updateLeaf tree@(MSegmentTree _ vec) !i !value = _updateElement tree i' value
  where
    -- length == 2 * (the number of the leaves)
    !offset = VUM.length vec `div` 2 - 1
    !i' = i + offset

-- | (Internal) Updates an `MSegmentTree` element (node or leaf) value and their parents up to top root.
{-# INLINE _updateElement #-}
_updateElement :: (VU.Unbox a, PrimMonad m) => MSegmentTree (PrimState m) a -> Int -> a -> m ()
_updateElement tree@(MSegmentTree _ vec) !i !value = do
  VUM.write vec i value
  _updateParent tree ((i - 1) `div` 2)

-- | (Internal) Recursivelly updates the parent nodes.
{-# INLINE _updateParent #-}
_updateParent :: (VU.Unbox a, PrimMonad m) => MSegmentTree (PrimState m) a -> Int -> m ()
_updateParent _ (-1) = pure () -- REMARK: (-1) `div` 2 == -1
_updateParent _ 0 = pure ()
_updateParent tree@(MSegmentTree f vec) !iParent = do
  !c1 <- VUM.read vec (iParent * 2 + 1)
  !c2 <- VUM.read vec (iParent * 2 + 2)
  _updateElement tree iParent (f c1 c2)

-- | Retrieves the folding result over the inclusive range `[l, r]` from `MSegmentTree`.
{-# INLINE queryByRange #-}
queryByRange :: forall a m. (VU.Unbox a, PrimMonad m) => MSegmentTree (PrimState m) a -> (Int, Int) -> m a
queryByRange (MSegmentTree !f !vec) (!lo, !hi) = fromJust <$> loop 0 (0, initialHi)
  where
    !initialHi = VUM.length vec `div` 2 - 1
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

-- {{{ Misc

-- compress duduplicates sorted list, nub deduplicates non-sorted list
-- TODO: std?
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x : xs) = x : compress (dropWhile (== x) xs)

-- e.g. binary ocombinations:
-- combination 2 [0..8]
combinations :: Int -> [a] -> [[a]]
combinations len elements = comb len (length elements) elements
  where
    comb 0 _ _ = [[]]
    comb r n a@(x : xs)
      | n == r = [a]
      | otherwise = map (x :) (comb (r - 1) (n - 1) xs) ++ comb r (n - 1) xs
    comb _ _ _ = error "unreachable"

getLineIntList :: IO [Int]
getLineIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getLineIntVec :: IO (VU.Vector Int)
getLineIntVec = VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getLineIntVecSorted :: IO (VU.Vector Int)
getLineIntVecSorted = VU.modify VAI.sort <$> getLineIntVec

getLineIntVecSortedDown :: IO (VU.Vector Int)
getLineIntVecSortedDown = VU.modify (VAI.sortBy (comparing Down)) <$> getLineIntVec

{-# INLINE vLength #-}
vLength :: (VG.Vector v e) => v e -> Int
vLength = VFB.length . VG.stream

{-# INLINE vRange #-}
vRange :: Int -> Int -> VU.Vector Int
vRange i j = VU.enumFromN i (j + 1 - i)

-- }}}

main :: IO ()
main = do
  [n, m] <- getLineIntList
  input <- replicateM m getLineIntList

  -- flip : not working?
  let links = accumArray (\xs x -> x : xs) [] (1, n) $ concatMap (\[!a, !b] -> [(a, b), (b, a)]) input

  let step bs i =
        let ls = sort $ links ! i
            result = mconcat $ intersperse (BSB.stringUtf8 ", ") $ map BSB.intDec ls
         in mconcat
              [ bs,
                BSB.intDec i,
                BSB.stringUtf8 ": ",
                BSB.char7 '{',
                result,
                BSB.stringUtf8 "}\n"
              ]

  BSB.hPutBuilder stdout $ foldl' step (mempty @BSB.Builder) [1 .. n]
