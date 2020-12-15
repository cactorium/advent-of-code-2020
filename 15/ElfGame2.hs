import System.IO
import System.IO.Unsafe

import qualified Data.HashMap.Strict as HM

import Data.List
import Data.List.Split

-- need to build two lists, forward and reverse
next :: Int -> Int -> HM.HashMap Int Int -> Int
next count last h = case HM.lookup last h of
  Nothing -> 0
  Just v -> count - v - 1

dropLast n = reverse . drop n . reverse

spoken start = start ++ nextSpoken (length start) (head $ reverse start) h where
  h = foldl (\h (i, v) -> HM.insert i v h) HM.empty $ zip (dropLast 1 start) [0..]

nextSpoken c last h = nl:nextSpoken (c+1) nl nh where
  nl = next c last h
  nh = HM.insert last (c-1) h

main = do
  input <- getContents
  let inp = map read $ splitOn "," input
  let s = spoken inp
  putStrLn $ show $ take 10 $ spoken s
  putStrLn $ show $ s !! (30000000 - 1)
