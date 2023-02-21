#!/usr/bin/env stack
-- stack script --resolver lts-16.31

-- https://hackage.haskell.org/package/base-4.13.0.0/docs/Data-Bits.html
import Data.Bits

main :: IO ()
main = do
  n <- read <$> getLine
  putStrLn $ solve 0 n []

solve :: Int -> Int -> [Char] -> [Char]
solve 10 _ cs = cs
solve i n cs = solve (i + 1) n (c : cs)
  where
    c = if b == 0 then '0' else '1'
    b = n .&. bit i
