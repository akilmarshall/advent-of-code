import Text.ParserCombinators.ReadP
import Data.Char (isAlpha, isSpace, isDigit)
import Data.List as List
import Data.Map as Map


getInput :: FilePath -> IO [String]
getInput fName = do
    raw <- readFile fName
    let ls = lines raw
    return ls

sample = "Faerun to Norrath = 129"

town = many1 (satisfy isAlpha)

parser :: ReadP ((String, String), Integer)
parser = do
    source <- town
    string " to "
    destination <- town
    string " = "
    distance <- many1 (satisfy isDigit)
    return ((source, destination), read distance)

getPoint =  fst . last <$> readP_to_S parser

-- points = [ getPoint s | s <- getInput "input"]
-- points = fmap getPoint (getInput "input")

-- main = do
--     data_  <- getInput "input"
--     points <- mapM getInput data_
--     return points

-- points = fmap (fmap getPoint) (getInput "input")

-- adjList = fmap (fmap Map.fromList) points 

-- m = Map.fromList [getPoint sample]

contains (a, b) m
    | Map.member (a, b) m = True
    | Map.member (b, a) m = True
    | otherwise           = False

r = ("Faerun", "Norrath")
s = ("Norrath", "Faerun")

main = do
    points <- fmap (fmap getPoint) (getInput "input")
    -- m <- Map.fromList points
    return points


h = [(r, 1), (s, 2)]
f = Map.fromList h
