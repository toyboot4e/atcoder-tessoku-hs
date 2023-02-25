#!/usr/bin/env stack
-- stack script --resolver lts-16.31 --package bytestring --package vector --package vector-algorithms --package containers --package array

{-# LANGUAGE ScopedTypeVariables #-}

import Data.Bifunctor
import Data.Char
import Data.List
import Data.Maybe

{- ORMOLU_DISABLE -}

-- bytestring: https://www.stackage.org/lts-16.11/package/bytestring-0.10.10.0
import qualified Data.ByteString.Char8 as BS

-- vector: https://www.stackage.org/lts-16.11/package/vector-0.12.1.2
import qualified Data.Vector.Unboxed as VU

{- ORMOLU_ENABLE -}

getLineIntList :: IO [Int]
getLineIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getLineIntVec :: IO (VU.Vector Int)
getLineIntVec = VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [n, k] <- getLineIntList
  xs <- getLineIntVec

  -- Linear search, but just once
  let count nDrop offset = do
        let x = xs VU.! offset
            xs' = VU.drop nDrop xs
            delta = VU.length . VU.takeWhile (\x' -> x' - x <= k) $ xs'
         in ((nDrop + delta) `max` (offset + 1), delta + nDrop - offset - 1)

  -- `mapAccumL` = rusty scan
  -- http://zvon.org/other/haskell/Outputlist/mapAccumL_f.html
  let sc = mapAccumL count 1 [0 .. (n - (1 + 1))]
  print . sum $ snd sc
