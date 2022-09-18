#!/usr/bin/env stack
-- stack script --resolver lts-16.11

getLineIntList :: IO [Int]
getLineIntList = map read . words <$> getLine

main :: IO ()
main = do
  [_, x] <- getLineIntList
  as <- getLineIntList
  putStrLn $ if x `elem` as then "Yes" else "No"
