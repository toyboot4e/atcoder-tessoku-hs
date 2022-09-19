#!/usr/bin/env stack
-- stack script --resolver lts-16.11

getLineIntList :: IO [Int]
getLineIntList = map read . words <$> getLine

main :: IO ()
main = do
  [n, k] <- getLineIntList
  print $ solve n k

solve :: Int -> Int -> Int
solve n k = length xs
  where
    xs =
      [() | a <- [1 .. (k - 2) `min` n], b <- [1 .. (k - 1) `min` n], let c = k - (a + b), c >= 1 && c <= n]
