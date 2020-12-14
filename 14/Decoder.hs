import System.IO

import qualified Data.HashMap.Strict as HM

import Data.Bits

import Data.List
import Data.List.Split

dropLast x ls = take (length ls - x) ls

data Command = Bitmask Integer Integer | SetMem Integer Integer deriving (Show)

parseCommand :: String -> Command
parseCommand s | isPrefixOf "mask" s = Bitmask mask val where
  (mask, val) = calcMask (0, 0) $ drop 7 s
  calcMask (m, v) [] = (m, v)
  calcMask (m, v) ('X':ss) = calcMask (2*m + 1, 2*v) ss
  calcMask (m, v) ('1':ss) = calcMask (2*m, 2*v + 1) ss
  calcMask (m, v) ('0':ss) = calcMask (2*m, 2*v) ss
  calcMask (m, v) (_:ss) = error "invalid mask"
parseCommand s | isPrefixOf "mem" s = SetMem addr val where
  splitS = splitOn " = " s
  addr = read $ dropLast 1 $ drop 4 $ head splitS
  val = read $ splitS !! 1
parseCommand s | otherwise = error "invalid command"

executeCommand :: (HM.HashMap Integer Integer, Integer, Integer) -> Command -> (HM.HashMap Integer Integer, Integer, Integer)
executeCommand (h, mask, mv) cmd = case cmd of
  Bitmask m v -> (h, m, v)
  SetMem a nv -> (HM.insert a mnv h, mask, mv) where
    mnv = (mask .&. nv) .|. mv

main = do
  input <- getContents
  let lines = filter (\l -> length l > 0) $ splitOn "\n" input
  putStrLn $ show lines
  let cmds = map parseCommand lines
  putStrLn $ show cmds
  let (results, _, _) = foldl executeCommand (HM.empty, 0, 0) cmds
  let s = sum $ map snd $ HM.toList results
  putStrLn $ show s
