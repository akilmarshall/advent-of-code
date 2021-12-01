input = [3,1,1,3,3,2,2,1,1,3]

-- group, takes a value and a list of values and computes how
-- many copies of a sit contiguously at the beginning of the
-- list
rollcall a []   = 0
rollcall a [x]
    | a == x    = 1
    | otherwise = 0
rollcall a (x:xs)
    | a == x    = 1 + rollcall a xs
    | otherwise = 0

-- rest, given a list remove the head and any adjacent duplicates
rest [x] = []
rest (x:y:xs)
    | x == y    = rest (x:xs)
    | otherwise = (y:xs)

-- process, a recursive method to run the look-and-say routine
-- Input  |  Output
-- 1        11
-- 11       21
-- 21       1211
-- 1211     111221
-- 111221   312211
-- process :: [Int] -> [Int]
process []      = []
process [x]     = [1, x]
process (x:xs)  = [count, x] ++ process left 
    where count = rollcall x (x:xs)
          left  = rest (x:xs)

-- compose f n times over x
-- callN f x 2 = (f.f) x
-- callN f x 5 = (f.f.f.f.f) x
callN f x 0 = x
callN f x 1 = f x
callN f x n = callN f (f x) (n - 1)

partOne = length $ callN process input 40
partTwo = length $ callN process input 50
