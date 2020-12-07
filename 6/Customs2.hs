import System.IO

getLines :: IO [String]
getLines = do
  contents <- getContents
  return $ getLines' [] "" contents

getLines' :: [String] -> String -> String -> [String]
getLines' l s "" = l ++ [s]
getLines' l s ('\n':t) = getLines' (l ++ [s]) "" t
getLines' l s (t:u) = getLines' l (s ++ [t]) u

sepGroups :: [String] -> [[String]]
sepGroups = sepGroups' []

sepGroups' :: [String] -> [String] -> [[String]]
sepGroups' l [] = [l]
sepGroups' l (s:ss) = case (length s == 0) of
  True -> l : (sepGroups' [] ss)
  False -> sepGroups' (l ++ [s]) ss

countGroup :: [String] -> Int
countGroup gs = length $ filter (\x -> all (elem x) gs) ['a'..'z']

unique as [] = as
unique as (x:xs) = case (any (\a -> a == x) as) of
  True -> unique as xs
  False -> unique (x:as) xs

main = do
  lines <- getLines
  let groups = filter (\g -> length g > 0) $ sepGroups lines
  print $ show groups
  print $ map countGroup groups
  print $ sum $ map countGroup groups

