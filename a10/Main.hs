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

main :: IO ()
main = do
  -- NOTE: using 1-based index
  [n] <- getLineIntList
  rooms <- VU.fromList . (0 :) <$> getLineIntList

  [d] <- getLineIntList
  questions <- replicateM d getLineIntList

  let maxL = VU.scanl1' max rooms
  let maxR = VU.scanr1' max rooms

  forM_ questions $ \[l, r] -> do
    let l' = maxL VU.! (l - 1)
    let r' = if r < n then maxR VU.! (r + 1) else 0
    print $ max l' r'
