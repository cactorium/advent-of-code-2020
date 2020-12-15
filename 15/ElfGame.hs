import System.IO

import Data.List
import Data.List.Split

-- need to build two lists, forward and reverse
next last = case elemIndex (head last) (tail last) of
  Nothing -> 0
  Just v -> v + 1

spoken start = start ++ nextSpoken (reverse start)

nextSpoken l = nl:nextSpoken (nl:l) where
  nl = next l

main = do
  input <- getContents
  let inp = map read $ splitOn "," input
  let s = spoken inp
  putStrLn $ show $ take 10 $ spoken s
  putStrLn $ show $ s !! 2019
