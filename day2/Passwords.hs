import System.IO

data Password = Password Int Int Char String deriving (Show)

wrapPassword :: [String] -> Password
wrapPassword (ls:c:p:[]) = Password mn mx (c!!0) p where
  mn = read $ takeWhile (\x -> x /= '-') ls
  mx = read $ drop 1 $ dropWhile (\x -> x /= '-') ls
wrapPassword other = Password 1 (-1) ' ' ""

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
isValid (Password mn mx c "") = (mn <= 0) && (mx >= 0)
isValid (Password mn mx c (d:s)) = if c == d then
  isValid $ Password (mn-1) (mx-1) c s else
  isValid $ Password mn mx c s

main = do
  contents <- getLines
  let passwords = map parsePassword contents
  putStrLn $ show passwords
  let c = length $ filter isValid passwords
  putStrLn $ show c
