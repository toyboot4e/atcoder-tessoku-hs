#!/usr/bin/env stack
-- stack script --resolver lts-16.11 --package bytestring --package vector --package vector-algorithms --package containers --package array --package primitive

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- {{{ Imports

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Array
import Data.Bifunctor
import Data.Bits
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import Data.Ord
import GHC.Event (IOCallback)
import GHC.Exts
import GHC.Float (int2Float)
import System.IO
import Text.Printf

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

-- {{{ Union-Find

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
  s <- unwrapRoot <$> VM.read vec px
  return s

-- }}}

getLineIntList :: IO [Int]
getLineIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getLineIntVec :: IO (VU.Vector Int)
getLineIntVec = VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [n, q] <- getLineIntList
  qs <- V.replicateM q getLineIntList
  uf <- newUF $ n + 1

  V.forM_ qs $ \[q, u, v] -> do
    if q == 1
      then -- unite
        unite uf u v
      else do -- questioned
        b <- same uf u v
        putStrLn $ if b then "Yes" else "No"

  return ()

