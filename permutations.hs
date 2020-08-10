-- module Likes where
-- likes :: [String] -> String
-- likes [] = "no one likes this"
-- likes [x] = x ++ " likes this"
-- likes [x, y] = x ++ " and " ++ y ++ " like this"
-- likes [x, y, z] = x ++ ", " ++ y ++ " and " ++ z ++ " like this"
-- likes (x : y : lst) = x ++ ", " ++ y ++ " and " ++ (show (length lst)) ++ " others like this"
insert :: Char -> String -> [String]
insert a str = [(take n str) ++ (a:(drop n str)) | n <- [0 .. length str]]

unique :: [String] -> [String]
unique [] = []
unique (x:xs) = x:[d | d <- unique xs, d /= x]

append :: [[String]] -> [String]
append [] = []
append (x:xs) = x ++ append xs

permutations :: String -> [String]
permutations [] = [""]
permutations [x] = [[x]]
permutations (x:xs) = unique (append [(insert x t) | t <- (permutations xs)])

-- import Data.List (sort)
-- descendingOrder :: Integer -> Integer
-- descendingOrder = read . reverse . sort . show
iter :: Integral n => n -> n -> Bool
iter n m
  | n * n < m = iter (n + 1) m
  | n * n > m = False
  | otherwise = True

isSquare :: Integral n => n -> Bool
isSquare = iter 0

-- narcissistic :: Integral n => n -> Bool
-- narcissistic n =
--   let arr = [read [x] :: Int | x <- show $ fromIntegral n]
--    in sum [x ^ length arr | x <- arr] == fromIntegral n
findMissing :: Integral n => [n] -> n
findMissing xs = let step = div (fromIntegral $ last xs - head xs) $ length xs
                     a1 = fromIntegral $ head xs
                     arr = [div ((xs !! (n - 1)) + (xs !! n)) 2
                           | n <- [0 ..]
                           , a1 + n * step /= fromIntegral (xs !! n)]
                 in head arr
-- data Base = A | T | G | C
-- type DNA = [Base]
-- f :: Base -> Base
-- f A = T
-- f T = A
-- f G = C
-- f C = G
-- dnaStrand :: DNA -> DNA
-- dnaStrand xs = map f xs
