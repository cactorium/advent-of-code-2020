import System.IO
import System.IO.Unsafe

getLines :: IO [String]
getLines = do
  contents <- getContents
  return $ getLines' [] "" contents

getLines' :: [String] -> String -> String -> [String]
getLines' l s "" = l ++ [s]
getLines' l s ('\n':t) = getLines' (l ++ [s]) "" t
getLines' l s (t:u) = getLines' l (s ++ [t]) u

firstInvalid :: Int -> [Int] -> Maybe Int
firstInvalid len vals = firstInvalid' len (reverse $ take len vals) $ drop len vals

firstInvalid' :: Int -> [Int] -> [Int] -> Maybe Int
firstInvalid' _ _ [] = Nothing
firstInvalid' len vs (w:ws) = case invalidVal vs w of
  True -> Just w
  False -> firstInvalid' len nv ws where
    nv = case length vs of
      x | x < len -> w:vs
      otherwise -> w:(take (len - 1) vs)

invalidVal :: [Int] -> Int -> Bool
--invalidVal vs w = not $ checkVal vs [] w
invalidVal vs w = unsafePerformIO $ do
  putStrLn $ show vs ++ " " ++ show w
  return $ not $ checkVal vs [] w

checkVal :: [Int] -> [Int] -> Int -> Bool
checkVal [] _ _ = False
checkVal (u:us) vs w = case ((w-u) `elem` us) || ((w-u) `elem` vs) of
  True -> True
  False -> checkVal us (u:vs) w

dropLast :: Int -> [a] -> [a]
dropLast n s = take (length s - n) s

main = do
  lins <- getLines
  let lines = map read $ dropLast 1 lins
  putStrLn $ show lines
  putStrLn $ show $ firstInvalid 25 lines
