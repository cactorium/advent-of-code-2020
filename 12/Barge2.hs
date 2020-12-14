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

data State = State (Int, Int) (Int, Int) deriving (Show)

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

executeMove :: State -> Move -> State
executeMove (State p@(x, y) w@(wx, wy)) (Move dir spaces) = case dir of
  North -> State p (wx, (wy+spaces))
  South -> State p (wx, (wy-spaces))
  East -> State p ((wx+spaces), wy)
  West -> State p ((wx-spaces), wy)
  Forward -> State (x + spaces*wx, y+spaces*wy) w
  MLeft -> case spaces of
    v | v >= 90 -> executeMove (State p (-wy, wx)) (Move MLeft (spaces-90))
    0 -> State p w
  MRight -> case spaces of
    v | v >= 90 -> executeMove (State p (wy, -wx)) (Move MRight (spaces-90))
    0 -> State p w

main = do
  liness <- getLines
  let lines = dropLast 1 liness
  let moves = map parseMove lines
  putStrLn $ show moves
  let final@(State (px, py) (wx, wy)) = foldl executeMove (State (0, 0) (10, 1)) moves
  putStrLn $ show final
  putStrLn $ show $ (abs px) + (abs py)

