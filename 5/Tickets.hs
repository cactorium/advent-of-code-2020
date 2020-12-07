import System.IO

data Number = L | R | F | B deriving (Show)

getLines :: IO [String]
getLines = do
  contents <- getContents
  return $ getLines' [] "" contents

getLines' :: [String] -> String -> String -> [String]
getLines' l s "" = l ++ [s]
getLines' l s ('\n':t) = getLines' (l ++ [s]) "" t
getLines' l s (t:u) = getLines' l (s ++ [t]) u

processChar 'L' = L
processChar 'R' = R
processChar 'F' = F
processChar 'B' = B
processChar _ = error "invalid input"

seatId :: [Number] -> Int
seatId = seatId' 0

seatId' n [] = n
seatId' n (F:ls) = seatId' (2*n) ls
seatId' n (B:ls) = seatId' (2*n + 1) ls
seatId' n (L:ls) = seatId' (2*n) ls
seatId' n (R:ls) = seatId' (2*n + 1) ls

main = do
  lins <- getLines
  let lines = filter (\l -> length l > 0) lins
  let tickets = map (map processChar) lines
  let ids = map seatId tickets
  putStrLn $ show $ maximum ids

