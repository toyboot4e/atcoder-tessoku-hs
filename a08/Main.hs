#!/usr/bin/env stack
-- stack script --resolver lts-16.31 --package bytestring --package vector --package containers --package array

{-# LANGUAGE BangPatterns #-}

import Control.Monad
import Data.Char
import Data.List

-- {{{ Template

{- ORMOLU_DISABLE -}

-- array
import qualified Data.Array.Unboxed as AU
import qualified Data.Array.MArray as MA
import qualified Data.Array.IArray as IA
import qualified Data.Array.ST as STA

-- bytestring
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS

-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import qualified Data.Vector.Fusion.Bundle as VFB
import qualified Data.Vector.Generic as VG

{- ORMOLU_ENABLE -}

getLineIntList :: IO [Int]
getLineIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getLineIntVec :: IO (VU.Vector Int)
getLineIntVec = VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

{-# INLINE vLength #-}
vLength :: (VG.Vector v e) => v e -> Int
vLength = VFB.length . VG.stream

-- }}}

getSums :: IO (AU.UArray (Int, Int) Int)
getSums = do
  [h, w] <- getLineIntList
  !list <- concat <$> replicateM h getLineIntList

  return $ STA.runSTUArray $ do
        -- NOTE: The right most element of the index tuple is used first.
        -- So we have to store the row-major matrix in an array with capacity `(h, w)` and
        -- use `(y, x)` index (i.e., `(row, column)` index).
        !sums <- MA.newListArray ((0, 0), (h - 1, w - 1)) list

        forM_ [0 .. (h - 1)] $ \y -> do
          forM_ [1 .. (w - 1)] $ \x -> do
            v0 <- MA.readArray sums (y, x - 1)
            v1 <- MA.readArray sums (y, x)
            MA.writeArray sums (y, x) (v0 + v1)

        forM_ [0 .. (w - 1)] $ \x -> do
          forM_ [1 .. (h - 1)] $ \y -> do
            v0 <- MA.readArray sums ((y - 1), x)
            v1 <- MA.readArray sums (y, x)
            MA.writeArray sums (y, x) (v0 + v1)

        return sums

getQuestions :: IO [[Int]]
getQuestions = do
  [!q] <- getLineIntList
  !rects <- replicateM q getLineIntList
  return $ flip map rects $ \[y1, x1, y2, x2] -> [y1 - 2, x1 - 2, y2 - 1, x2 - 1]

main :: IO ()
main = do
  sums <- getSums
  rects <- getQuestions

  -- Be warned that the rect is composed of (row, column, row, column), i.e., `(y1, x2, y2, x2)`.
  forM_ rects $ \[y1, x1, y2, x2] -> do
    let !bx = x1 >= 0
    let !by = y1 >= 0

    let !y2x2 = sums IA.! (y2, x2)
    let !y1x1 = if by && bx then sums IA.! (y1, x1) else 0
    let !y1x2 = if by then sums IA.! (y1, x2) else 0
    let !y2x1 = if bx then sums IA.! (y2, x1) else 0

    print $ y2x2 + y1x1 - y1x2 - y2x1
