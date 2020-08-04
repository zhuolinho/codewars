-- module Likes where

-- likes :: [String] -> String
-- likes [] = "no one likes this"
-- likes [x] = x ++ " likes this"
-- likes [x, y] = x ++ " and " ++ y ++ " like this"
-- likes [x, y, z] = x ++ ", " ++ y ++ " and " ++ z ++ " like this"
-- likes (x : y : lst) = x ++ ", " ++ y ++ " and " ++ (show (length lst)) ++ " others like this"

insert :: Char -> String -> [String]
insert a str = [(take n str) ++ (a : (drop n str)) | n <- [0 .. length str]]

unique :: [String] -> [String]
unique [] = []
unique (x : xs) = x : [d | d <- unique xs, d /= x]

append :: [[String]] -> [String]
append [] = []
append (x : xs) = x ++ append xs

permutations :: String -> [String]
permutations [] = [""]
permutations [x] = [[x]]
permutations (x : xs) = unique (append [(insert x t) | t <- (permutations xs)])

-- data Base = A | T | G | C
-- type DNA = [Base]

-- f :: Base -> Base
-- f A = T
-- f T = A
-- f G = C
-- f C = G

-- dnaStrand :: DNA -> DNA
-- dnaStrand xs = map f xs
