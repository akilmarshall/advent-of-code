module Main where

getInput :: FilePath -> IO [Int]
getInput fName = do
    raw <- readFile fName
    let ls = lines raw
    let nums = map read ls :: [Int]
    return nums

type Mass = Int
type Fuel = Int

testInput :: [Int]
testInput = [12, 14, 1969, 100756]

fuelRequired :: Mass -> Fuel
fuelRequired mass = div mass 3 - 2

part1 :: [Mass] -> Fuel
part1 = sum . map fuelRequired

fuelRequired' :: Mass -> Fuel
fuelRequired' mass
    | fuelMass <= 0 = 0 
    | otherwise = fuelMass + fuelRequired' fuelMass 
    where fuelMass = fuelRequired mass

part2 :: [Mass] -> Fuel
part2 = sum . map fuelRequired'

main :: IO ()
main = do
    input <- getInput "input"
    print $ part1 input
    print $ part2 input
