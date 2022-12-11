{-# LANGUAGE OverloadedStrings #-}

module Day1(main) where

import           Data.List       (sort)
import           Data.List.Split (splitOn)

main :: IO ()
main = do
  input <- readFile "src/input/Day1.txt"
  let numbers = map (sum . map (\x -> read x :: Int)) (splitOn [""] $ lines input)
  -- Part 1
  print $ maximum numbers
  -- Part 2
  print $ sum $ take 3 $ reverse $ sort numbers
