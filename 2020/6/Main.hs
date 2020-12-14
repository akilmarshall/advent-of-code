import Data.Set (empty, union, fromList, intersection)

getInput :: FilePath -> IO [String]
getInput fName = do
    raw <- readFile fName
    let ls = lines raw
    return ls

solve [] answers n      = n + length answers
solve ("":xs) answers n = solve xs empty (n + length answers)
solve (x:xs) answers n  = solve xs (answers `union` fromList x) n   

partOne answers = solve answers empty 0

identity = fromList ['a'..'z']

solve' [] answers n      = n + length answers
solve' ("":xs) answers n = solve' xs identity (n + length answers)
solve' (x:xs) answers n  = solve' xs (answers `intersection` fromList x) n

partTwo answers = solve' answers identity 0

main :: IO ()
main = do
    input <- getInput "input"
    print $ partTwo input
