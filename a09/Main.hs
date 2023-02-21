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

getEvents :: IO ((Int, Int, AU.UArray (Int, Int) Int))
getEvents = do
  [h, w, n] <- getLineIntList
  rects <- replicateM n getLineIntList

  -- Example: create four events from (2, 2, 3, 3):
  -- . . . .
  -- . + . -
  -- . . . .
  -- . - . +
  -- NOTE: The array has size of `(h, w) + (1, 1)`
  let source = flip concatMap rects $ \[y1, x1, y2, x2] ->
        [ ((y1 - 1, x1 - 1), 1),
          ((y1 - 1, x2), -1),
          ((y2, x1 - 1), -1),
          ((y2, x2), 1)
        ]

  let events = IA.accumArray (+) 0 ((0, 0), (h, w)) source
  return (h, w, events)

toCumulativeSums :: AU.UArray (Int, Int) Int -> AU.UArray (Int, Int) Int
toCumulativeSums events = do
  let (_, (h, w)) = IA.bounds events

  STA.runSTUArray $ do
    !arr <- MA.thaw events

    -- scan rows
    forM_ [0 .. h] $ \y -> do
      forM_ [1 .. w] $ \x -> do
        v0 <- MA.readArray arr (y, x - 1)
        v1 <- MA.readArray arr (y, x)
        MA.writeArray arr (y, x) (v0 + v1)

    -- scan columns
    forM_ [0 .. w] $ \x -> do
      forM_ [1 .. h] $ \y -> do
        v0 <- MA.readArray arr (y - 1, x)
        v1 <- MA.readArray arr (y, x)
        MA.writeArray arr (y, x) (v0 + v1)

    return arr

main :: IO ()
main = do
  (h, w, events) <- getEvents
  let csums = toCumulativeSums events

  forM_ [0 .. (h - 1)] $ \y -> do
    putStrLn $ unwords . map show . map (\x -> csums IA.! (y, x)) $ [0 .. (w - 1)]
