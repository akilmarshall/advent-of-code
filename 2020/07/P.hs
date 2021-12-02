module P where

-- import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

main = putStrLn "Hello"

ruleSet = undefined
rule = undefined
color = undefined 




number :: GenParser Char st String
number = many1 digit
