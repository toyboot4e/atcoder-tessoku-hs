#!/usr/bin/env stack
-- stack script --resolver lts-16.11

main :: IO ()
main = do
  n <- read <$> getLine :: IO Int
  print $ n * n
