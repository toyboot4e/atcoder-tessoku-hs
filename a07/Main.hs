#!/usr/bin/env stack
-- stack script --resolver lts-16.11 --package bytestring --package vector --package containers --package array

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
import qualified Data.Vector.Unboxed.Mutable as VUM

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
  [d] <- getLineIntList
  [n] <- getLineIntList
  -- REMARK: `Vector (Int, Int)` works while `Vector [Int]` does not work
  lrs <- VU.replicateM n (fmap (\[l, r] -> (l, r)) getLineIntList)
  VU.mapM_ print (solve d lrs)

solve :: Int -> VU.Vector (Int, Int) -> VU.Vector Int
solve d lrs =
  -- REMARK: How to use remarkable vectors
  let vec = VU.create $ do
        v <- VUM.replicate d (0 :: Int)
        VU.forM_ lrs $ \(l, r) -> do
          VUM.modify v (+ 1) (l -1)
          when (r < d) $ VUM.modify v (subtract 1) r
        return v
   in VU.scanl1 (+) vec
