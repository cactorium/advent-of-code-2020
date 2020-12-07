import System.IO

data Tile = Tree | Empty deriving (Show)

istree Tree = True
istree _ = False

getLines :: IO [String]
getLines = do
  contents <- getContents
  return $ getLines' [] "" contents

getLines' :: [String] -> String -> String -> [String]
getLines' l s "" = l ++ [s]
getLines' l s ('\n':t) = getLines' (l ++ [s]) "" t
getLines' l s (t:u) = getLines' l (s ++ [t]) u

char2tile :: Char -> Tile
char2tile '#' = Tree
char2tile _ = Empty

path :: [[Tile]] -> (Int, Int) -> [(Int, Int)]
path ts delta = takeWhile (inbounds ts) $ traverse' delta 0 0

traverse' :: (Int, Int) -> Int -> Int -> [(Int, Int)]
traverse' (dx, dy) x y = (x, y) : traverse' (dx, dy) (x + dx) (y + dy)

inbounds ts (x, y) = ((length ts) > y)

index ts (x, y) = row !! (x `mod` (length row)) where
  row = (ts !! y)

main = do
  lines <- getLines
  let tiles = map (map char2tile) $ filter (\l -> (length l) > 0) lines
  putStrLn $ show tiles
  let traversed = map (\s -> length $ filter istree $ map (index tiles) $ path tiles s) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  putStrLn $ show traversed
  putStrLn $ show $ product traversed
