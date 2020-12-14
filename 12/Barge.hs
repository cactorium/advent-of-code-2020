import Control.Monad.ST

import System.IO

getLines :: IO [String]
getLines = do
  contents <- getContents
  return $ getLines' [] "" contents

getLines' :: [String] -> String -> String -> [String]
getLines' l s "" = l ++ [s]
getLines' l s ('\n':t) = getLines' (l ++ [s]) "" t
getLines' l s (t:u) = getLines' l (s ++ [t]) u

dropLast :: Int -> [a] -> [a]
dropLast n xs = take (length xs - n) xs

data Move = Move Direction Int deriving (Show)
data Direction = North | South | East | West | Forward | MLeft | MRight deriving (Show)

data State = State { stateDir :: Direction, stateX :: Int, stateY :: Int } deriving (Show)

parseDirection 'N' = North
parseDirection 'S' = South
parseDirection 'E' = East
parseDirection 'W' = West
parseDirection 'F' = Forward
parseDirection 'L' = MLeft
parseDirection 'R' = MRight
parseDirection _ = error "invalid direction to parse"

parseMove :: String -> Move
parseMove (s:ss) = Move dir moves where
  dir = parseDirection s
  moves = read ss

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North
turnLeft _ = error "invalid direction to turn from"

turnRight :: Direction -> Direction
turnRight North = East
turnRight West = North
turnRight South = West
turnRight East = South
turnRight _ = error "invalid direction to turn from"

executeMove :: State -> Move -> State
executeMove (State sdir x y) (Move dir spaces) = case dir of
  North -> State sdir x (y+spaces)
  South -> State sdir x (y-spaces)
  East -> State sdir (x+spaces) y
  West -> State sdir (x-spaces) y
  Forward -> executeMove (State sdir x y) (Move sdir spaces)
  MLeft -> case spaces of
    v | v >= 90 -> executeMove (State (turnLeft sdir) x y) (Move MLeft (spaces-90))
    0 -> (State sdir x y)
  MRight -> case spaces of
    v | v >= 90 -> executeMove (State (turnRight sdir) x y) (Move MRight (spaces-90))
    0 -> (State sdir x y)

main = do
  liness <- getLines
  let lines = dropLast 1 liness
  let moves = map parseMove lines
  putStrLn $ show moves
  let final = foldr (flip executeMove) (State East 0 0) moves
  putStrLn $ show final
  putStrLn $ show $ (abs $ stateX final) + (abs $ stateY final)

