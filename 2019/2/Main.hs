module Main where

import Data.List.Split (splitOn)
import Data.Vector (Vector, fromList, (!), head, slice, concat, length)

replace :: Char -> Char
replace c
    | c == ','  = '\n'
    | otherwise = c

-- getInput :: FilePath -> IO [Int]
getInput fName = do
    raw <- readFile fName
    let ls = lines $ map replace raw
    let nums = map read ls :: [Int]
    let vectorOfNums = fromList nums
    return vectorOfNums

testInput :: Vector Int
testInput = fromList . map read $ splitOn "," "1,9,10,3,2,3,11,0,99,30,40,50"

partOne _ = ()

-- opcodes defined in the Intcode language
data Opcode = Add | Mult | Halt | Null deriving (Show, Eq)

-- helper function to turn Ints into Opcodes
intToOpcode :: Int -> Opcode
intToOpcode 1  = Add
intToOpcode 2  = Mult
intToOpcode 99 = Halt
intToOpcode _  = Null

-- get the ith word from a program
getWord :: Int -> Vector Int -> (Int, Int, Int, Int)
getWord i v = (w, x, y, z)
    where w = v ! ((4 * i) + 0)
          x = v ! ((4 * i) + 1) 
          y = v ! ((4 * i) + 2)
          z = v ! ((4 * i) + 3)

-- run the integer code program and return the value computed
simulate :: Vector Int -> Int
simulate program = Data.Vector.head program

vectorModify i x vec = Data.Vector.concat [pre, fromList [x], post]
    where l = Data.Vector.length
          -- pre = slice 0 i vec
          -- post = slice (i + 1) (max 0 $ l - i - 1) vec
          pre = fromList [1..3]
          post = fromList [4..8]
-- compute step n of the program
-- step :: Int -> Vector Int -> Vector Int
step n program
    | 
    | op == Add  = fromList []
    | op == Mult = fromList [] 
    | op == Halt = program
    | op == Null = program
    where m = div $ Data.Vector.length program 4
          (a, b, c, d) = getWord n program
          op = intToOpcode a
          x = program ! a
          y = program ! b


main :: IO ()
main = do
    input <- getInput "input"
    print "day 2"
    print $ partOne input
