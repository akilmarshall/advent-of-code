module Main where

import Parsing

data Policy = Policy Int Int Char deriving (Show)
type Password = String
data Row = Row Policy Password deriving (Show)
type Database = [Row]

inputParser :: Parser Row
inputParser = do
    a <- nat
    char '-'
    b <- nat
    space
    c <- letter
    char ':'
    space
    pass <- some letter
    return (Row (Policy a b c) pass)

-- return the input as a list of Strings
getInput :: FilePath -> IO [String]
getInput fileName = do
    raw <- readFile fileName
    return $ lines raw

-- count the number of times a is contained in a String
contains :: Char -> String -> Int
contains a = length . filter (== a)

-- implement the policy from part one
policy :: Row -> Bool
policy (Row (Policy a b c) pass)
    | (n >= a) && (n <= b) = True
    | otherwise            = False
    where n = contains c pass

-- count the number of rows that pass a policy
policyCheck :: Database -> (Row -> Bool) -> Int
policyCheck d p = length $ filter p d

-- implement the policy from part two
newPolicy :: Row -> Bool
newPolicy (Row (Policy a b c) pass) = x /= y
    where x = (pass !! (a - 1)) == c
          y = (pass !! (b - 1)) == c
    
main :: IO ()
main = do
    input <- getInput "input"
    -- parse the input
    let parsedInput = map (parse inputParser) input
    -- create a database from the parsed input
    let database = map (fst . head) parsedInput
    let answerOne = policyCheck database policy
    let answerTwo = policyCheck database newPolicy
    print answerOne
    print answerTwo
