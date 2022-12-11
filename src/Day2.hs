{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day2() where

getScore :: String -> Int
getScore line = do
  case words line of
    ["A", "X"] -> 3 + value "X"
    ["A", "Y"] -> 6 + value "Y"
    ["A", "Z"] -> value "Z"
    ["B", "X"] -> value "X"
    ["B", "Y"] -> 3 + value "Y"
    ["B", "Z"] -> 6 + value "Z"
    ["C", "X"] -> 6 + value "X"
    ["C", "Y"] -> value "Y"
    ["C", "Z"] -> 3 + value "Z"
  where
    value "A" = 1
    value "B" = 2
    value "C" = 3
    value "X" = 1
    value "Y" = 2
    value "Z" = 3

predict :: String -> String
predict line = do
  case words line of
    ["A", "X"] -> "A Z"
    ["A", "Y"] -> "A X"
    ["A", "Z"] -> "A Y"
    ["B", "X"] -> "B X"
    ["B", "Y"] -> "B Y"
    ["B", "Z"] -> "B Z"
    ["C", "X"] -> "C Y"
    ["C", "Y"] -> "C Z"
    ["C", "Z"] -> "C X"

main :: IO ()
main = do
    -- Part 1
  input <- readFile "src/input/Day2.txt"
  let numbers = map getScore $ lines input
  print $ sum numbers
  -- Part 2
  input2 <- readFile "src/input/Day2.txt"
  let numbers2 = map (getScore . predict) $ lines input2
  print $ sum numbers2
