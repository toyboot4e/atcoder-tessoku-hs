#!/usr/bin/env stack
-- stack script --resolver lts-16.11 --package bytestring --package vector --package vector-algorithms --package containers --package array

import Data.Bifunctor
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord

{- ORMOLU_DISABLE -}

-- bytestring: https://www.stackage.org/lts-16.11/package/bytestring-0.10.10.0
import qualified Data.ByteString.Char8 as BS

-- vector: https://www.stackage.org/lts-16.11/package/vector-0.12.1.2
import qualified Data.Vector.Unboxed as VU

{- ORMOLU_ENABLE -}

bsToIntList :: BS.ByteString -> [Int]
bsToIntList = unfoldr (BS.readInt . BS.dropWhile isSpace)

getLineIntList :: IO [Int]
getLineIntList = bsToIntList <$> BS.getLine

-- | Binary search for sorted items in an inclusive range (from left to right only)
-- |
-- | It returns the `(ok, ng)` index pair at the boundary.
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
bsearch :: (Int, Int) -> (Int -> Bool) -> (Maybe Int, Maybe Int)
bsearch (low, high) isOk = bimap wrap wrap (loop (low - 1, high + 1) isOk)
  where
    loop (ok, ng) isOk
      | abs (ok - ng) == 1 = (ok, ng)
      | isOk m = loop (m, ng) isOk
      | otherwise = loop (ok, m) isOk
      where
        m = (ok + ng) `div` 2
    wrap :: Int -> Maybe Int
    wrap x
      | x == low - 1 || x == high + 1 = Nothing
      | otherwise = Just x

main :: IO ()
main = do
  [n, k] <- getLineIntList
  -- [12, 10, 11] -> [(1, 10), (2, 11), (0, 12)]
  ias <- VU.fromList . sortBy (comparing snd) . zip [(0 :: Int) ..] <$> getLineIntList

  let (ok, ng) = bsearch (0, n - 1) (\i -> snd (ias VU.! i) <= k)
  print $ 1 + fromJust ok
