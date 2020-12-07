import System.IO

data Passport = Passport [(String, String)] deriving (Show)

parseLines :: [String] -> [Passport]
parseLines = parseLines' $ Passport []

parseLines' :: Passport -> [String] -> [Passport]
parseLines' (Passport l) [] = [Passport l]
parseLines' (Passport l) (s:ss) = if (length s == 0) then
  (Passport l) : parseLines' (Passport []) ss else
  parseLines' (Passport (l ++ nf)) ss where
    nf = parseFields s

parseFields :: String -> [(String, String)]
parseFields "" = []
parseFields s = (parseEntry entry) : (parseFields rest) where
  entry = takeWhile (\c -> (c /= ' ')) s
  rest = drop 1 $ dropWhile (\c -> (c /= ' ')) s

parseEntry :: String -> (String, String)
parseEntry s = (takeWhile (\c -> (c /= ':')) s,
    drop 1 $ dropWhile (\c -> (c /= ':')) s)

getLines :: IO [String]
getLines = do
  contents <- getContents
  return $ getLines' [] "" contents

getLines' :: [String] -> String -> String -> [String]
getLines' l s "" = l ++ [s]
getLines' l s ('\n':t) = getLines' (l ++ [s]) "" t
getLines' l s (t:u) = getLines' l (s ++ [t]) u

validatePassport :: Passport -> Bool
validatePassport p = all (checkField p) [
    "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

checkField :: Passport -> String -> Bool
checkField (Passport l) f = any (\(x, y) -> x == f) l

main = do
  lines <- getLines
  putStrLn $ show lines
  let passports = parseLines lines
  putStrLn $ show passports
  putStrLn $ show $ length $ filter validatePassport passports
