module Main where

-- import Data.Vector (Vector, (!))

getInput :: FilePath -> IO [String]
getInput fName = do
    raw <- readFile fName
    let ls = lines raw
    return ls

testInput :: [String]
testInput = [
    "..##.......",
    "#...#...#..",
    ".#....#..#.",
    "..#.#...#.#",
    ".#...##..#.",
    "..#.##.....",
    ".#.#.#....#",
    ".#........#",
    "#.##...#...",
    "#...##....#",
    ".#..#...#.#"]

type Slope = (Int, Int)
data Tile = Snow | Tree deriving (Show, Eq)
type Row = [Tile]
data Map = Map { tiles :: [Row]  
               , width :: Int  
               , height :: Int  
               } deriving (Show)


toTile :: Char -> Tile
toTile '.' = Snow 
toTile '#' = Tree

f :: String -> Row
f = map toTile

createMap :: [String] -> Map
createMap rows = Map tiles width height 
    where tiles = map (map toTile) rows
          width = length $ head rows
          height = length rows

at :: (Int, Int) -> [[a]] -> a
at (x, y) = (!! x) . (!! y)

coordinate :: Map -> (Int, Int) -> Maybe Tile
coordinate m (x, y)
    | x >= width m || x < 0  = Nothing
    | y >= height m || y < 0 = Nothing
    | otherwise              = Just $ at (x, y) ms
    where ms = tiles m
          w = width m
          h = height m

m = createMap testInput
s :: Slope
s = (3, 1)

coordAtTime :: Map -> Slope -> Int -> (Int, Int)
coordAtTime _ _ 0 = (0, 0)
coordAtTime m (a, b) n = ((a * n) `mod` w, b * n)
    where w = width m

countTrees :: [Maybe Tile] -> Int
countTrees [] = 0
countTrees (x:xs)
    | x == Just Tree = 1 + countTrees xs
    | otherwise = countTrees xs

-- partOne :: Map -> Slope -> Int
partOne m s = treesSeen
    where w = width m
          h = height m
          t = tiles m
          coordPath = map (coordAtTime m s) [0..h]
          tilePath = map (coordinate m) coordPath 
          treesSeen = countTrees tilePath         

slopeList :: [Slope]
slopeList = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

partTwo :: Map -> [Slope] -> Int
partTwo _ []     = 1
partTwo m (s:xs) = partOne m s * partTwo m xs

main :: IO ()
main = do
    input <- getInput "input"
    let m = createMap input
    print $ partOne m s
    print $ partTwo m slopeList
