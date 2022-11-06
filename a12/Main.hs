#!/usr/bin/env stack
-- stack script --resolver lts-16.11 --package bytestring --package vector --package vector-algorithms --package containers --package array

{-# LANGUAGE NumericUnderscores #-}

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

-- | Binary search for a sorted items which returns the `(ok, ng)` index pair at the boundary.
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
-- | In this case `bsearchMain` returns the `(ok, ng)` = `(5, 6)` pair:
-- |
-- | > > let xs = [0..9] in do
-- | > >   print $ bsearchMain (0, 9) (\i -> xs !! i <= 5)
-- | > (5, 6)
-- |
-- | Note that the closure CANNOT BE an exact match. Use a range-based predicate instead.
-- |
-- | Errors in edge cases
-- | --------------------
-- |
-- | While this function works for the above example, be warned that it's incomplete.
-- | It makes errors in certain edge cases:
-- |
-- | 1. The only `ok` item is at the end of the list.
-- | 2. The only `ng` item is at the beginning of the list.
-- | 3. There's no `ok` item or there's no `ng` item.
-- |
-- | So this function is wrapped by `bsearch`.
bsearchMain :: (Int, Int) -> (Int -> Bool) -> (Int, Int)
bsearchMain (ok, ng) isOk
  | abs (ok - ng) == 1 = (ok, ng)
  | isOk m = bsearchMain (m, ng) isOk
  | otherwise = bsearchMain (ok, m) isOk
  where
    m = (ok + ng) `div` 2

-- | Binary search for inclusive range (from left to right only)
bsearch :: (Int, Int) -> (Int -> Bool) -> (Maybe Int, Maybe Int)
bsearch (low, top) isOk = bimap wrap wrap result
  where
    result = bsearchMain (low - 1, top + 1) isOk'

    isOk' :: Int -> Bool
    isOk' x
      | x == low - 1 = True
      | x == top + 1 = False
      | otherwise = isOk x

    wrap :: Int -> Maybe Int
    wrap x
      | x == low - 1 || x == top + 1 = Nothing
      | otherwise = Just x

main :: IO ()
main = do
  [_n, k] <- getLineIntList
  cycles <- getLineIntVec

  let count t = VU.sum . VU.map (t `div`) $ cycles

  let (_ok, ng) = bsearch (0, 1_000_000_000) (\t -> count t < k)
  print $ fromJust ng
