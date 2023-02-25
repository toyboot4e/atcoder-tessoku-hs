#!/usr/bin/env stack
-- stack script --resolver lts-16.31 --package bytestring --package vector --package vector-algorithms --package containers --package array

{-# LANGUAGE NumericUnderscores #-}

import Data.Bifunctor
import Data.Char
import Data.List
import System.IO

{- ORMOLU_DISABLE -}

-- bytestring: https://www.stackage.org/lts-16.11/package/bytestring-0.10.10.0
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as BSB

-- vector: https://www.stackage.org/lts-16.11/package/vector-0.12.1.2
import qualified Data.Vector.Unboxed as VU

{- ORMOLU_ENABLE -}

getLineIntList :: IO [Int]
getLineIntList = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getLineIntVec :: IO (VU.Vector Int)
getLineIntVec = VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [n] <- getLineIntList
  as <- getLineIntVec

  -- sorted indices
  let is = VU.fromList . sortOn (as VU.!) $ [0 .. (n - 1)]

  -- compressed values zipped with `[0..]`
  let ivs =
        scanl'
          ( \(i0', v) i1' ->
              let a0 = as VU.! (is VU.! i0')
                  a1 = as VU.! (is VU.! i1')
                  v' = if a1 > a0 then v + 1 else v
               in (i1', v')
          )
          (0, 1)
          [1 .. (n - 1)]

  -- compressed values in the original order
  let result = map snd . sortOn (\(i', _) -> is VU.! i') $ ivs

  -- print result
  let bs = mconcat . intersperse (BSB.char7 ' ') $ map BSB.intDec result
  BSB.hPutBuilder stdout bs
  BSB.hPutBuilder stdout $ BSB.char7 '\n'

