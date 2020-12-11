import System.IO
import System.IO.Unsafe

import Data.Maybe

import Data.List
import Data.List.Split

getLines :: IO [String]
getLines = do
  contents <- getContents
  return $ getLines' [] "" contents

getLines' :: [String] -> String -> String -> [String]
getLines' l s "" = l ++ [s]
getLines' l s ('\n':t) = getLines' (l ++ [s]) "" t
getLines' l s (t:u) = getLines' l (s ++ [t]) u

data Opcode = Jump | Acc | Nop deriving (Show)
data Instruction = Instruction Opcode Int deriving (Show)

parseOpcode s = opcode where
  (opcode, _) = fromJust $ find (\(_, name) -> name == s) [
    (Jump, "jmp"),
    (Acc, "acc"),
    (Nop, "nop")]

parseInstruction s = Instruction (parseOpcode op) (sign*val) where
  parts = splitOn " " s
  op = parts !! 0
  sign = case (parts !! 1) !! 0 of
    '+' -> 1
    '-' -> -1
    _ -> error "invalid sign"
  val = read $ drop 1 $ (parts !! 1)


runUntilRepeat :: [Int] -> [Instruction] -> Int -> Int -> Int
runUntilRepeat prev instrs counter acc = case elem counter prev of
  True -> acc
  False -> case instrs !! counter of
        Instruction Jump v -> runUntilRepeat (counter:prev) instrs (counter + v) acc
        Instruction Acc v -> runUntilRepeat (counter:prev) instrs (counter + 1) (acc + v)
        Instruction Nop _ -> runUntilRepeat (counter:prev) instrs (counter + 1) acc


dropLast :: Int -> [a] -> [a]
dropLast n s = take (length s - n) s

main = do
  lines <- getLines
  putStrLn $ show lines
  let instrs = map parseInstruction $ dropLast 1 lines
  putStrLn $ show instrs
  putStrLn $ show $ runUntilRepeat [] instrs 0 0
