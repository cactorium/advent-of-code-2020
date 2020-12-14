import Data.Ord
import Data.List
import Data.List.Split

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

parseBuses :: String -> [Int]
parseBuses s = map read $ filter (\s -> s /= "x") $ splitOn "," s

calcTime :: Int -> Int -> Int
calcTime time bus = bus - (time `mod` bus)

main = do
  lines <- getLines
  let time = read (lines !! 0) :: Int
  let buses = parseBuses (lines !! 1)
  putStrLn $ show buses
  let bus = minimumBy (comparing $ calcTime time) buses
  putStrLn $ show (bus * (calcTime time bus))
