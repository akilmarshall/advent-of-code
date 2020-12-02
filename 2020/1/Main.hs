module Main where

getInput :: FilePath -> IO [Int]
getInput fName = do
    raw <- readFile fName
    let ls = lines raw
    let nums = map read ls :: [Int]
    return nums

-- choose all k-tuples from xs
combinations :: Int -> [a] -> [[a]]
combinations k xs = combinations' (length xs) k xs
  where combinations' n k' l@(y:ys)
          | k' == 0   = [[]]
          | k' >= n   = [l]
          | null l    = []
          | otherwise = map (y :) (combinations' (n - 1) (k' - 1) ys) ++ combinations' (n - 1) k' ys 

testInput :: [Int]
testInput = [1721, 979, 366, 299, 675, 1456]

target :: Int
target = 2020

-- return True if the condition a + b = target is met
check :: Int -> Int -> Bool
check a b
    | a + b == target = True
    | otherwise       = False

-- check if pair sums to target
checkPair :: [Int] -> Bool
checkPair (a:b:xs)
    | a + b == target = True
    | otherwise       = False

-- check if triple sums to target
checkTriple :: [Int] -> Bool
checkTriple (a:b:c:xs)
    | a + b + c == target = True
    | otherwise           = False

-- both solutions filter a list of all n-combinations for the required condition of the n-tuple's entries summing to 2020

-- solution for part 1
partOne :: [Int] -> Int
partOne xs = product answer
    where candidates = combinations 2 xs
          answer = head $ filter checkPair candidates 

-- solution for part 2
partTwo :: [Int] -> Int
partTwo xs = product answer 
    where candidates = combinations 3 xs
          answer = head $ filter checkTriple candidates 
          
main :: IO ()
main = do
    input <- getInput "input"
    let trimmedInput = filter (>= target) input
    print $ partOne input
    print $ partTwo input
