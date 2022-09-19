#!/usr/bin/env stack
-- stack script --resolver lts-16.11

import Control.Monad
import Data.Char
import Data.List

-- {{{ Template
{- ORMOLU_DISABLE -}

-- bytestring
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS

-- vector
import qualified Data.Vector.Fusion.Bundle as VFB
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU

{- ORMOLU_ENABLE -}

bsToIntVec :: BS.ByteString -> VU.Vector Int
bsToIntVec = VU.unfoldr (BS.readInt . BS.dropWhile isSpace)

getLineIntVec :: IO (VU.Vector Int)
getLineIntVec = VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

bsToIntList :: BS.ByteString -> [Int]
bsToIntList = unfoldr (BS.readInt . BS.dropWhile isSpace)

getLineIntList :: IO [Int]
getLineIntList = bsToIntList <$> BS.getLine

{-# INLINE vLength #-}
vLength :: (VG.Vector v e) => v e -> Int
vLength = VFB.length . VG.stream

-- }}}

main :: IO ()
main = do
  [_, q] <- getLineIntList

  -- number of comers for each day (0-based index)
  -- REMARK: list was too slow for the preceding calculation
  as <- getLineIntVec

  -- inclusive day ranges (1-based index)
  -- REMARK: we can't have a two-dimensional unboxing vector
  lrs <- replicateM q getLineIntList

  mapM_ print (solve as lrs)

solve :: VU.Vector Int -> [[Int]] -> [Int]
solve as lrs = map count lrs
  where
    count [l, r] = csums VU.! r - csums VU.! (l - 1)
    count _ = undefined
    -- cumulative sums (1-based index)
    csums = VU.scanl (+) 0 as
