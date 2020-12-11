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

main = do
  lins <- getLines
  let lines = map read $ dropLast 1 lins
  let adapters = quickSort lines
  putStrLn $ show adapters
  putStrLn $ show $ calcDiff adapters
  let nums = map (\m -> length $ filter (\x -> x == m) $ calcDiff adapters) [1, 3]
  putStrLn $ show nums
  putStrLn $ show $ product nums
