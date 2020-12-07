import System.IO

import qualified Data.List as List

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

checkYear mn mx y = (length y == 4) && (yr >= mn) && (yr <= mx) where
  yr :: Int
  yr = read y

dropLast i s = take (length s - i) s

checkHeight s = (((List.isSuffixOf "cm" s) && (meas >= 150) && (meas <= 193)) ||
    (((List.isSuffixOf "in" s) && (meas >= 59) && (meas <= 76)))) where
    meas = (read $ dropLast 2 s) :: Int

checkHair (s:ss) = (length ss == 6) && (s == '#') && all
    (\c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')) ss

checkEye s = any (\t -> t == s) 
    ["amb", "blu", "brn", "gry", "grn", "hzl" , "oth"]

validatePassport :: Passport -> Bool
validatePassport p = all (checkField p) [
    ("byr", checkYear 1920 2002), 
    ("iyr", checkYear 2010 2020), 
    ("eyr", checkYear 2020 2030),
    ("hgt", checkHeight),
    ("hcl", checkHair),
    ("ecl", checkEye),
    ("pid", \s -> (all (\c -> c >= '0' && c <= '9') s) && (length s == 9))]

checkField :: Passport -> (String, String -> Bool) -> Bool
checkField (Passport l) (f, g) = any (\(x, y) -> (x == f) && g y) l

main = do
  lines <- getLines
  putStrLn $ show lines
  let passports = parseLines lines
  putStrLn $ show passports
  putStrLn $ show $ length $ filter validatePassport passports
