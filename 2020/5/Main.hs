import Data.List

getInput :: FilePath -> IO [String]
getInput fName = do
    raw <- readFile fName
    let ls = lines raw
    return ls

front a b = (a, y)
    where midpoint = (b - a) / 2
          delta = fromIntegral $ ceiling midpoint
          y = b - delta

back a b = (y, b)
    where midpoint = (b - a) / 2
          delta = fromIntegral $ ceiling midpoint
          y = a + delta


calculatePos "" (a, b, c, d) = (a * 8) + c
calculatePos ('F':xs) (a, b, c, d) = calculatePos xs (a', b', c, d)
    where (a', b') = front a b
calculatePos ('B':xs) (a, b, c, d) = calculatePos xs (a', b', c, d)
    where (a', b') = back a b
calculatePos ('L':xs) (a, b, c, d) = calculatePos xs (a, b, c', d')
    where (c', d') = front c d
calculatePos ('R':xs) (a, b, c, d) = calculatePos xs (a, b, c', d')
    where (c', d') = back c d

solve coord = calculatePos coord (0, 127, 0, 7)

findSeat (x:y:xs)
    | x + 1 == y = findSeat (y:xs) 
    | otherwise  = x + 1

main :: IO ()
main = do
    input <- getInput "input"
    let ids = map solve input
    let maxId = foldr max 0 ids
    let sortedIds = sort ids
    let missingId = findSeat sortedIds
    print missingId
