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

path :: [[Tile]] -> [(Int, Int)]
path ts = takeWhile (inbounds ts) $ traverse' 0 0

traverse' :: Int -> Int -> [(Int, Int)]
traverse' x y = (x, y) : traverse' (x + 3) (y + 1)

inbounds ts (x, y) = ((length ts) > y)

index ts (x, y) = row !! (x `mod` (length row)) where
  row = (ts !! y)

main = do
  lines <- getLines
  let tiles = map (map char2tile) $ filter (\l -> (length l) > 0) lines
  putStrLn $ show tiles
  let traversed = map (index tiles) $ path tiles
  putStrLn $ show traversed
  putStrLn $ show $ length $ filter istree $ traversed
