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

data Tile = Floor | Empty | Full deriving (Eq)

instance Show Tile where
  show Floor = "."
  show Empty = "L"
  show Full = "#"

nextBoard :: [[Tile]] -> [[Tile]]
nextBoard b = map (\j -> map (\i -> nextState b i j) [0..length (b!!j) - 1]) [0..length b - 1]

nextState :: [[Tile]] -> Int -> Int -> Tile
nextState b x y = case b !! y !! x of
  Floor -> Floor 
  Empty -> if countNeighbors b x y == 0 then
    Full else Empty
  Full -> if countNeighbors b x y >= 5 then
    Empty else Full

isFull Full = True
isFull _ = False

countNeighbors :: [[Tile]] -> Int -> Int -> Int
countNeighbors b x y = length $ filter isFull spaces where
  spaces = map raycast [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
  raycast (i, j) = raycast' (x+i) (y+j) (i, j)
  raycast' nx ny (i, j) | not $ inbounds (nx, ny) = Empty
  raycast' nx ny (i, j) | indexB (nx, ny) == Floor = raycast' (nx+i) (ny+j) (i, j)
  raycast' nx ny (i, j) | otherwise = indexB (nx, ny)

  indexB (i, j) = b !! j !! i
  inbounds (i, j) = (j >= 0) && (length b > j) && (i >= 0) && (length (b !! j) > i)

parseTile '.' = Floor
parseTile 'L' = Empty
parseTile '#' = Full
parseTile _ = error "invalid tile"

dropLast x l = take (length l - x) l

findFixed b = if nb == b then b else findFixed nb where
  nb = nextBoard b

main = do
  liness <- getLines
  let lines = dropLast 1 liness
  putStrLn $ show lines

  let tiles = map (map parseTile) lines :: [[Tile]] 
  putStrLn $ show tiles
  let fixed = findFixed tiles
  putStrLn "fixed: "
  putStrLn $ show fixed
  putStrLn $ show $ length $ concat $ map (filter isFull) fixed
