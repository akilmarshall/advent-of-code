import Data.Char (digitToInt)

-- MISGUIDED


-- Helper functions

alphabet = "abcdefghijlkmnopqrstuvwxyz"
indices = [0..26]

findI a []      = Nothing
findI a (x:xs)
    | a == x    = Just 0
    | otherwise = fmap (+ 1) (findI a xs)


-- try to compute the indice of a character
encode :: Char -> Maybe Integer
encode c = (!!) indices <$> findI c alphabet

-- try to return the char at the indice
decode :: Integer -> Maybe Char
decode i = (!!) alphabet <$> findI i indices

-- Given a number xyz return the list [x, y, z]
strToList :: Functor f => f Char -> f Int
strToList n = fmap digitToInt n

-- Given [x, y, z] return xyz

listToNum xs = _listToNum xs n
    where n = length xs - 1

_listToNum [] _     = 0
_listToNum [x] 0    = x
_listToNum (x:xs) n = x * (10^n) + _listToNum xs (n-1)


-- Problem specific functions and definitions


santaPassword = "cqjxjnds"

encodePass :: [Char] -> Maybe Integer
encodePass p = fmap listToNum $ sequence $ fmap encode p 

decodePass p = undefined
    where s = show p

incrementPassword p = fmap (+1) (encodePass p)
