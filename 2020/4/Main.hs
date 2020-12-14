module Main where

import Data.List (groupBy)
import Parsing

getInput :: FilePath -> IO [String]
getInput fName = do
    raw <- readFile fName
    let ls = lines raw
    return ls

-- minimum keys required to be considered a valid passport
minimumKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

test = 
    [ "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
    , "byr:1937 iyr:2017 cid:147 hgt:183cm"
    , ""
    , "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
    , "hcl:#cfa07d byr:1929"
    , ""
    , "hcl:#ae17e1 iyr:2013"
    , "eyr:2024"
    , "ecl:brn pid:760753108 byr:1931"
    , "hgt:179cm"
    , ""
    , "hcl:#cfa07d eyr:2025 pid:166559648"
    , "iyr:2011 ecl:brn hgt:59in"
    ]

-- given a string of space seperated key:value pairs return a list of keys
keys :: String -> [String]
keys "" = []
keys s  = map (takeWhile (/= ':') . filter (/= ' ')) $ groupBy (\ _ b -> b /= ' ') s

-- given a list x and y of equatble values compute if x is a subset of y
-- x `subset` y
subset :: (Eq a) => [a] -> [a] -> Bool
subset [] _ = True
subset _ [] = False
subset (x:xs) y
    | x `elem` y = subset xs y
    | otherwise = False



solve' :: Int -> [String] -> [String] -> Int
solve' validPassports _ []      = validPassports
solve' validPassports k [x]
    | minimumKeys `subset` k' = validPassports + 1
    | otherwise               = validPassports
    where k' = k ++ keys x
solve' validPassports k ("":xs)
    | minimumKeys `subset` k = solve' (validPassports + 1) [] xs
    | otherwise              = solve' validPassports [] xs
solve' validPassports k (x:xs)  = solve' validPassports k' xs
    where k' = k ++ keys x

solve :: [String] -> Int
solve = solve' 0 []

-- part 2

-- expr ::= key:value
inputParser = do
    key <- many letter  
    char ':'
    value <- many alphanum
    (space <|> (char '\n'))  -- how to parse x or y
    return (key, value)

keyValues :: String -> [(String, String)]
keyValues s = [("", "")]

-- f validPassports kv [x]     = 
-- f validPassports kv ("":xs) = 
-- f validPassports kv (x:xs)  = 

main :: IO ()
main = do
    input <- getInput "input"
    let answer = solve input
    print answer
