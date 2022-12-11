{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day4() where

isOverlapped :: String -> Bool
isOverlapped line = do
  let h = takeWhile (/= ',') line
  let t = drop 1 $ dropWhile (/= ',') line
  -- first range
  let (a,b) = (read $ takeWhile (/= '-') h :: Int, read $ drop 1 $ dropWhile (/= '-') h :: Int)
  -- second range
  let (c,d) = (read $ takeWhile (/= '-') t :: Int, read $ drop 1 $ dropWhile (/= '-') t :: Int)
  --
  (a <= c && b >= d) || (c <= a && d >= b)

isOverlapped2 :: String -> Bool
isOverlapped2 line = do
  let h = takeWhile (/= ',') line
  let t = drop 1 $ dropWhile (/= ',') line
  -- first range
  let (a,b) = (read $ takeWhile (/= '-') h :: Int, read $ drop 1 $ dropWhile (/= '-') h :: Int)
  -- second range
  let (c,d) = (read $ takeWhile (/= '-') t :: Int, read $ drop 1 $ dropWhile (/= '-') t :: Int)
  --
  (a <= c && b >= c) || (c <= a && d >= a)

main :: IO ()
main = do
    -- Part 1
    input <- readFile "src/input/Day4.txt"
    let overlapped = filter (== True) $ map isOverlapped $ lines input
    print $ length overlapped
    -- Part 2
    let overlapped2 = filter (== True) $ map isOverlapped2 $ lines input
    print $ length overlapped2
