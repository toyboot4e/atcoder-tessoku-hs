#!/usr/bin/env stack
-- stack script --resolver lts-16.31 --package vector --package primitive

{-# LANGUAGE BangPatterns, TypeFamilies, MultiParamTypeClasses #-}

module Main (main) where

import Control.Monad
import Control.Monad.Primitive
import qualified Data.Vector.Mutable as VM

newtype UnionFind s = UnionFind (VM.MVector s UfNode)

type IOUnionFind = UnionFind RealWorld
type STUnionFind s = UnionFind s

-- | `Child parent | Root size`
-- | Not `Unbox` :(
data UfNode = Child {-# UNPACK #-} !Int | Root {-# UNPACK #-} !Int

-- newUF :: (PrimMonad m) => Int -> m (UnionFind (PrimState s))
newUF n = UnionFind <$> VM.replicate n (Root 1)

main :: IO ()
main = do
  let n = 100
  -- こっちは普通に動く？
  uf <- UnionFind <$> VM.replicate (100) (Root 1)
  return ()

