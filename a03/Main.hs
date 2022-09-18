#!/usr/bin/env stack
-- stack script --resolver lts-16.11

getLineIntList :: IO [Int]
getLineIntList = map read . words <$> getLine

main :: IO ()
main = do
  [n, k] <- getLineIntList
  ps <- getLineIntList
  qs <- getLineIntList
  let res = solve n k ps qs
  putStrLn $ if res then "Yes" else "No"

solve :: Int -> Int -> [Int] -> [Int] -> Bool
solve _ k ps qs = xs /= []
  where
    xs = [ () | p <- ps, q <- qs, p + q == k]
