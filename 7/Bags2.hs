import System.IO
import Data.List
import Data.Maybe

import System.IO.Unsafe

getLines :: IO [String]
getLines = do
  contents <- getContents
  return $ getLines' [] "" contents

getLines' :: [String] -> String -> String -> [String]
getLines' l s "" = l ++ [s]
getLines' l s ('\n':t) = getLines' (l ++ [s]) "" t
getLines' l s (t:u) = getLines' l (s ++ [t]) u

data Rule = Rule String [(Int, String)] deriving (Show)

splitAtContain :: [String] -> ([String], [String])
splitAtContain s = (l, r) where
  isNotContain = \x -> x /= "contain"
  l = takeWhile isNotContain s
  r = drop 1 $ dropWhile isNotContain s

dropLast :: Int -> [a] -> [a]
dropLast n s = take (length s - n) s


parseLine :: String -> Rule
parseLine s = Rule b c where
  (l, r) = splitAtContain $ words $ dropLast 1 s -- remove the period
  b = unwords $ dropLast 1 l
  c = case (r !! 0) == "no" of
    True -> []
    False -> map parseEntry $ splitCommas r
  parseEntry s = (read n :: Int, unwords $ dropLast 1 rest) where
    {-
    n = unsafePerformIO $ do
          putStrLn $ show s
          return $ s !! 0
    -}
    n = s !! 0
    rest = drop 1 $ s
  splitCommas :: [String] -> [[String]]
  splitCommas s = splitCommas' [] s
  splitCommas' :: [String] -> [String] -> [[String]]
  splitCommas' x [] = [x]
  splitCommas' x (s:ss) = case endsWithComma s of
    True -> (x ++ [dropLast 1 s]):(splitCommas' [] ss)
    False -> splitCommas' (x ++ [s]) ss
  endsWithComma s = (last s) == ','
  
-- let's assume there's no cycles!
data Bag = Bag String [(Int, Bag)] deriving (Show)

lookupRule rules name = find checkRule rules where
  checkRule (Rule n _) = n == name

determineBag :: [Rule] -> String -> Maybe Bag
determineBag rules typ = lookupRule rules typ >>=
  \r -> case r of
    Rule _ innerbagrules -> Just (Bag typ innerbags) where
      innerbags = map (\(i, b) -> (i, fromJust $ determineBag rules b)) innerbagrules

ruleName (Rule n _) = n

hasBag :: String -> Maybe Bag -> Bool
hasBag name (Just (Bag n innerbags)) = (n == name) || (
    any (hasBag name) $ map (\(_, b) -> Just b) innerbags)
hasBag _ Nothing = False

countBags (Bag _ []) = 1
countBags (Bag _ entries) = 1 + (sum $ map (\(i, b) -> i * countBags b) entries)

findBag s (Just (Bag n _)) = s == n
findBag s Nothing = False

main = do
  lines <- getLines
  let rules = map parseLine $ dropLast 1 lines
  --putStrLn $ show rules
  let bags = map (determineBag rules) $ map ruleName rules
  --putStrLn $ show bags
  let goldshiny = fromJust $ fromJust $ find (findBag "shiny gold") bags
  let numbags = countBags goldshiny
  putStrLn $ show (numbags - 1)
