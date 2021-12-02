{- stack script
 --resolver lts-18.1
 --package "vector lens megaparsec containers"
 -}
{-# LANGUAGE TemplateHaskell #-}
import qualified Data.Vector as V
import qualified Data.Set as S
import Control.Lens
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void


getInput :: FilePath -> IO [String]
getInput fName = do
    raw <- readFile fName
    let ls = lines raw
    return ls

data Instruction = NOP | ACC Int | JMP Int
                   deriving (Show, Eq)

-- type Parser = M.Parsec Void Text

-- pInstruction :: Parser String
-- pInstruction = M.choice $ map (\x -> string x) ["nop", "acc", "jmp"]

-- pArg :: Parser Int
-- pArg = do
--     sign <- M.choice [char '+', char '-']
--     value <- M.many (digitChar)
--     return value

-- pAll :: Parser Instruction
-- pAll = do
--     instr <- pInstruction
--     L.space1
--     val <- pArg
--     M.eof
--     return $ f instr val

f "acc" n = ACC n
f "jmp" n = JMP n
f _ _     = NOP

type Program = V.Vector Instruction
data Console = Console { _program :: Program
                       , _counter :: Int
                       , _seen    :: S.Set Int
                       , _acc     :: Int
                       } deriving (Show, Eq)
makeLenses ''Console  -- using TemplateHaskell to create lenses for the fields in Console

makeConsole p = Console { _program = p
                        , _counter = 0
                        , _seen    = S.empty
                        , _acc     = 0
                        }

-- Useful functions
setCounter :: Int -> Console -> Console
setCounter n = counter .~ n

incCounter :: Console -> Console
incCounter = counter +~ 1

getCurInstr :: Console -> Instruction
getCurInstr console = (console ^. program) V.! (console ^. counter)

modifyAcc :: Int -> Console -> Console
modifyAcc n = acc +~ n

-- add the current instruction to the seen set
addToSeen :: Console -> Console
addToSeen c = (seen .~ seen') c  -- learning moment, composition with lenses is of the FUNCTIONS not the data \
    where i     = c ^. counter   -- \ ultimately the lenses should be curriable into endomorphisms on the data
          seen' = (c ^. seen) `S.union` (S.fromList [i])
            
seenBefore :: Console -> Int -> Bool
seenBefore console i = i `S.member` (console ^. seen)

-- ACC
-- 1. add the current instr to the seen set
-- 2. increment the program counter
-- 3. modify the accumulator
-- JMP
-- 1. add the current instr to the seen set
-- 2. modify (jump) the program counter
-- NOP
-- 1. add the current instr to the seen set
-- 2. increment the program counter
step (ACC n) c = ((modifyAcc n) . incCounter . addToSeen) c
step (JMP n) c = (setCounter (n + i) . addToSeen) c
    where i = c ^. counter
step NOP c     = (incCounter . addToSeen) c

solution c
    | seenBefore c (c ^. counter) = return (c ^. acc)  -- check for termination condition
    | otherwise = print cur_instr >> solution (step cur_instr c) -- step the dynamics and make the recursive call
    where cur_instr = getCurInstr c
          
-- Testing data
testProgram = V.fromList [NOP, ACC 1, JMP 4, ACC 3, JMP (-3), ACC (-99), ACC 1, JMP (-4), ACC 6]

console = Console { _program = testProgram
                      , _counter = 0
                      , _seen    = S.empty
                      , _acc     = 0
                      }

test = print "running example" >> solution console

main :: IO ()
main = do
            print "Day 8"
            program <- getInput "input"
            print (take 5 program)
