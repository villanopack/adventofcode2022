{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day3() where

import           Data.Char       (isUpper, ord)
import           Data.List       (intersect)
import           Data.List.Split (chunksOf)

findItems :: String -> Char
findItems line = do
  let (h, t) = splitAt (div (length line) 2) line
  head (h `intersect` t)

priorityValue :: Char -> Int
priorityValue c = if isUpper c then  ord c - 38 else ord c - 96

findItems2 :: [String] -> Char
findItems2 [a,b,c] = do
  head ((a `intersect` b) `intersect` c)

main :: IO ()
main = do
  -- Part 1
  input <- readFile "src/input/Day3.txt"
  let items = map (priorityValue . findItems) (lines input)
  print $ sum items
  -- Part 2
  input2 <- readFile "src/input/Day3.txt"
  let items2 = map (priorityValue . findItems2) ((chunksOf 3 .lines) input2)
  print $ sum items2
