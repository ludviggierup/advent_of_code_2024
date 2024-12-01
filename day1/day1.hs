import Data.List

part1 :: IO ()
part1 = do
  input <- readFile "part1.txt"
  print . sum $ distances (sort $ leftList input) (sort $ rightList input)

part2 :: IO ()
part2 = do
  input <- readFile "part2.txt"
  print . sum $ [x * count x (rightList input) | x <- leftList input]

leftList :: String -> [Int]
leftList input = map readInt $ evens $ words input

rightList :: String -> [Int]
rightList input = map readInt $ odds $ words input

evens :: [a] -> [a]
evens (x : xs) = x : odds xs
evens _ = []

odds :: [a] -> [a]
odds (_ : xs) = evens xs
odds _ = []

readInt :: String -> Int
readInt = read

distances :: [Int] -> [Int] -> [Int]
distances (x : xs) (y : ys) = abs (x - y) : distances xs ys
distances _ _ = []

count :: Int -> [Int] -> Int
count n [] = 0
count n (x : xs)
  | n == x = 1 + count n xs
  | otherwise = count n xs