import Control.Monad
import Control.Monad.ST

import Data.Array
import Data.Array.ST

import System.IO
import System.IO.Unsafe

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

parseLine :: (STArray s (Int, Int) Tile) -> (Int, String) -> ST s (STArray s (Int, Int) Tile)
parseLine ary (j, l) = do
  let el = zip [0..(length l)-1] l
  _ <- forM el (\(i, c) -> writeArray ary (j, i) $ parseTile c)
  return $ ary


parseTiles :: [String] -> ST s (STArray s (Int, Int) Tile)
parseTiles s = do
  arr <- newArray ((0, 0), ((length s)-1, (length $ s !! 0)-1)) Empty
  _ <- forM (zip [0..(length s)-1] s) (parseLine arr)
  return $ arr

nextBoardST :: (STArray s (Int, Int) Tile) -> (STArray s (Int, Int) Tile) -> ST s ()
nextBoardST a b = do
  (_, (rows, cols)) <- getBounds a
  forM [0..rows] (\j -> forM [0..cols] (\i -> do
    new <- nextStateST a (j, i)
    writeArray b (j, i) new
    ))
  return ()

nextStateST :: (STArray s (Int, Int) Tile) -> (Int, Int) -> ST s Tile
nextStateST ary (j, i) = do
  ns <- getNeighborsST ary (j, i)
  let numFull = length $ filter isFull ns
  v <- readArray ary (j, i)
  return $ case v of
    Floor -> Floor
    Empty -> if numFull == 0 then Full else Empty
    Full -> if numFull >= 4 then Empty else Full

getNeighborsST ary (j, i) = do
  (_, (rows, cols)) <- getBounds ary
  let inbounds (j, i) = (j >= 0) && (i >= 0) && (j <= rows) && (i <= cols)
  let nidx = filter inbounds $ map (\(oj, oi) -> (j+oj, i+oi)) $
        [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
  forM nidx (\(y, x) -> readArray ary (y, x))

nextBoard :: [[Tile]] -> [[Tile]]
nextBoard b = map (\j -> map (\i -> nextState b i j) [0..length (b!!j) - 1]) [0..length b - 1]

nextState :: [[Tile]] -> Int -> Int -> Tile
nextState b x y = case b !! y !! x of
  Floor -> Floor 
  Empty -> if countNeighbors b x y == 0 then
    Full else Empty
  Full -> if countNeighbors b x y >= 4 then
    Empty else Full

isFull Full = True
isFull _ = False

countNeighbors :: [[Tile]] -> Int -> Int -> Int
countNeighbors b x y = length $ filter isFull $ map indexB $
    filter inbounds $ map offset $ filter isNotMe $ 
    concat spaces where
  spaces = map (\x -> map (\y -> (x, y)) [-1, 0, 1]) [-1, 0, 1]
  indexB (i, j) = b !! j !! i
  inbounds (i, j) = (j >= 0) && (length b > j) && (i >= 0) && (length (b !! j) > i)
  offset (i, j) = (x + i, y + j)
  isNotMe (0, 0) = False
  isNotMe _ = True

parseTile '.' = Floor
parseTile 'L' = Empty
parseTile '#' = Full
parseTile _ = error "invalid tile"

dropLast x l = take (length l - x) l

findFixed b = if nb == b then b else findFixed nb where
  nb = nextBoard b

isEqualST :: (STArray s (Int, Int) Tile) -> (STArray s (Int, Int) Tile) -> ST s Bool
isEqualST a b = do
  ea <- getElems a
  eb <- getElems b
  return $ ea == eb

findFixedST :: (STArray s (Int, Int) Tile) -> (STArray s (Int, Int) Tile) -> ST s (STArray s (Int, Int) Tile)
findFixedST a b = do
  eq <- isEqualST a b
  if eq
    then return a
    else do
      nextBoardST a b
      findFixedST b a

main = do
  liness <- getLines
  let lines = dropLast 1 liness
  putStrLn $ show lines

  let tiles = runSTArray $ parseTiles lines
  putStrLn $ show tiles
  let result = runSTArray $ do
      tiles <- parseTiles lines
      bounds <- getBounds tiles
      tiles2 <- newArray bounds Empty
      findFixedST tiles tiles2

  putStrLn $ show $ length $ filter isFull $ elems result
