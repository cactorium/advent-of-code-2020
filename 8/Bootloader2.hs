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


runUntilTerminate :: [Int] -> Int -> Int -> [Instruction] -> Maybe Int
runUntilTerminate prev counter acc instrs = case elem counter prev of
  True -> Nothing
  False -> case length instrs == counter of
    True -> Just acc
    False -> case instrs !! counter of
        Instruction Jump v -> runUntilTerminate (counter:prev) (counter + v) acc instrs
        Instruction Acc v -> runUntilTerminate (counter:prev) (counter + 1) (acc + v) instrs
        Instruction Nop _ -> runUntilTerminate (counter:prev) (counter + 1) acc instrs

mutateInstrs instrs idx = start ++ [modified] ++ end where
  start = take idx instrs
  modified = case instrs !! idx of
    Instruction Jump v -> Instruction Nop v
    Instruction Nop v -> Instruction Jump v
    Instruction Acc v -> Instruction Acc v
  end = drop (idx + 1) instrs

dropLast :: Int -> [a] -> [a]
dropLast n s = take (length s - n) s

main = do
  lines <- getLines
  putStrLn $ show lines
  let instrs = map parseInstruction $ dropLast 1 lines
  putStrLn $ show instrs
  putStrLn $ show $ filter isJust $ map (runUntilTerminate [] 0 0 . mutateInstrs instrs) [0..length instrs]
