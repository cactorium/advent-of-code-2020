import System.IO

data Password = Password Int Int Char String deriving (Show)

wrapPassword :: [String] -> Password
wrapPassword (ls:c:p:[]) = Password mn mx (c!!0) p where
  mn = read $ takeWhile (\x -> x /= '-') ls
  mx = read $ drop 1 $ dropWhile (\x -> x /= '-') ls

parsePassword :: String -> Password
parsePassword = wrapPassword . words

getLines :: IO [String]
getLines = do
  contents <- getContents
  return $ getLinesH [] "" contents

getLinesH :: [String] -> String -> String -> [String]
getLinesH l s "" = l ++ [s]
getLinesH l s ('\n':t) = getLinesH (l ++ [s]) "" t
getLinesH l s (t:u) = getLinesH l (s ++ [t]) u

isValid :: Password -> Bool
isValid (Password mn mx c s) = (lowerHas || upperHas) && (lowerHas /= upperHas) where
  lowerHas = (s !! (mn - 1)) == c
  upperHas = (s !! (mx - 1)) == c

main = do
  contents <- getLines
  let passwords = map parsePassword $ filter (\s -> (length s) > 0) contents
  putStrLn $ show passwords
  let c = length $ filter isValid passwords
  putStrLn $ show c
