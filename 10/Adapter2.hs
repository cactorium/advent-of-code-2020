import Data.List

import Control.Monad.ST
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

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort l | length l == 1 = l
quickSort l | otherwise = quickSort left ++ [l !! 0] ++ quickSort right where
  pivot = l !! 0
  rest = drop 1 l
  left = [x | x <- rest, x <= pivot]
  right = [x | x <- rest, x > pivot]

dropLast :: Int -> [a] -> [a]
dropLast n s = take (length s - n) s

calcDiff s = map (\(a, b) -> a - b) $ zip (s ++ [last s + 3]) (0:s)

-- find all the runs of x < 3, then calculate the number of ways of each
-- of those runs, then find the product of all those runs
calcWays :: [Int] -> Integer
calcWays x | length x <= 1 = 1
calcWays x = unsafePerformIO $ do
  putStrLn $ show x
  return $ case findIndex (\v -> v == 3) x of 
    Just i -> (calcWays (take i x)) * (calcWays (drop (i + 1) x))
    Nothing -> case findCombinable x of
      -- in one case combine them, in the other don't
      Just (i, v) -> (calcWays combinedX) + (calcWays $ drop (i+1) x) where
        combinedX = (take i x) ++ (v:drop (i+2) x)
      -- return one if there are no combinable sets
      Nothing -> 1

first [] = Nothing
first (x:_) = Just x

findCombinable x = first $ filter (\(_, v) -> v <= 3) $ zip [0..length x - 1] $
  map (\(a, b) -> a + b) $ zip x $ drop 1 x
  

main = do
  lins <- getLines
  let lines = map read $ dropLast 1 lins
  let adapters = quickSort lines
  putStrLn $ show adapters
  putStrLn $ show $ calcWays [1,1]
  putStrLn $ show $ calcWays [1,1,1]
  putStrLn $ show $ calcWays [1,1,1,1]
  putStrLn $ show $ calcDiff adapters
  putStrLn $ show $ calcWays $ calcDiff adapters
