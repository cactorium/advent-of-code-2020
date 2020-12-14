import Data.Bits
import Data.Ord
import Data.List
import Data.List.Split

import Control.Monad.ST

import System.IO

getLines :: IO [String]
getLines = do
  contents <- getContents
  return $ getLines' [] "" contents

getLines' :: [String] -> String -> String -> [String]
getLines' l s "" = l ++ [s]
getLines' l s ('\n':t) = getLines' (l ++ [s]) "" t
getLines' l s (t:u) = getLines' l (s ++ [t]) u

dropLast :: Int -> [a] -> [a]
dropLast n xs = take (length xs - n) xs

parseBuses :: String -> [Maybe Integer]
parseBuses s = map maybeRead $ splitOn "," s where
  maybeRead s = if s !! 0 == 'x' then
    Nothing else
    Just $ read s

-- assuming all the bus numbers are coprime
-- sum up the solutions for each
-- bus congruency
-- basically the solution that solves for the each entry individually but does not contribute to the other conguences, ie. vi | vi `mod` bj = delta ij*ej
-- this means vi is the multiple of the product of all the other bus numbers
-- multiplied by some constant, k*r where r is the product of all the other numbers
-- this solves all the congruences specified by vi | vi `mod` bj = delta ij*ej except with i=j, vi `mod` bi = ei
-- which implies k*r `mod` bi = ei, which is easily solvable with
-- r = (ei*k^-1) `mod` bi, using k's multiplicative root
calcTime x = solve $ toConstraints x where
  toConstraints :: [Maybe Integer] -> [(Integer, Integer)]
  toConstraints lst = fst $ foldl (\(l, i) e -> case e of
    Just v -> ((v, i):l, i + 1)
    Nothing -> (l, i + 1)) ([], 0) lst
  solve nums = findSmallest (map solveConstraint nums) 0 where
    findSmallest [] v = v
    findSmallest ((l, r):rest) v = let  left = findSmallest rest (v+l)
                                        right = findSmallest rest (v+r) in
      case (left > 0, right > 0) of
        (True, True) -> minimum (left, right)
        (True, False) -> left
        (False, True) -> right
        (False, False) -> -1
    allnums = map fst nums
    solveConstraint (v, i) = (
        ((-i * base_mod_inv) `mod` v) * base,
        -((i*base_mod_inv) `mod` v) * base)  where
      base_mod_inv = base_mod `mul_inv` v
      base_mod = base `mod` v
      base = product $ filter ((/=) v) allnums

mul_inv val base = exp_mod val (base - 2) base

exp_mod :: Integer -> Integer -> Integer -> Integer
exp_mod v exp base = foldr (\a b -> (a*b) `mod` base) 1 $ masked where
  masked = map fst $ filter (\(_, i) -> (exp .&. i) /= 0) factors
  factors = takeWhile (\(_, i) -> i <= exp) $ zip exps $ map (shift 1) [0..]
  exps = iterate (\x -> (x*x) `mod` base) v

exp_mod_factors v exp base = masked where
  masked = map fst $ filter (\(_, i) -> (exp .&. i) /= 0) factors
  factors = takeWhile (\(_, i) -> i <= exp) $ zip exps $ map (shift 1) [0..]
  exps = iterate (\x -> (x*x) `mod` base) v

main = do
  lines <- getLines
  let time = read (lines !! 0) :: Int
  let buses = parseBuses (lines !! 1)
  putStrLn $ show buses
  putStrLn $ show $ calcTime buses
