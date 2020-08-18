safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit [x] = Just []
safeInit (x:xs) = do
  ms <- safeInit xs
  return (x:ms)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith func lst =
  let (m, n) = span func lst
      x = if null n
          then []
          else splitWith func (tail n)
  in if null m
     then x
     else m:x

zipWithN func = foldl step []
  where
    step [] ys = ys
    step (x:xs) (y:ys) = func x y:step xs ys

rever xs = helper [] xs
  where
    helper acc [] = acc
    helper acc (x:xs) = helper (x:acc) xs

myFoldrMap f xs = foldr step [] xs
  where
    step x xs = f x:xs

myFoldr step zero (x:xs) = step x (myFoldr step zero xs)
myFoldr _ zero [] = zero

-- myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f = myFoldr step id
  where
    step x g = g . (flip f) x

type ErrorMessage = String

digitToInt :: Char -> Either ErrorMessage Int
digitToInt '0' = Right 0
digitToInt '1' = Right 1
digitToInt '2' = Right 2
digitToInt '3' = Right 3
digitToInt '4' = Right 4
digitToInt '5' = Right 5
digitToInt '6' = Right 6
digitToInt '7' = Right 7
digitToInt '8' = Right 8
digitToInt '9' = Right 9
digitToInt x = Left ("non-digit '" ++ x:"'")

fold acc [] = Right acc
fold acc (x:xs) = case digitToInt x of
  Left msg -> Left msg
  Right q  -> fold (acc * 10 + q) xs

asInt_either :: String -> Either ErrorMessage Int
asInt_either ('-':xs) = case asInt_either xs of
  Left msg -> Left msg
  Right q  -> Right (-q)
asInt_either xs = fold 0 xs

concat' :: [[a]] -> [a]
concat' = foldr (++) []

takeWhile' f [] = []
takeWhile' f (x:xs)
  | f x = x:(takeWhile' f xs)
  | otherwise = []

takewhile f xs = foldr step [] xs
  where
    step x acc
      | f x = x:acc
      | otherwise = []

-- groupBy' f [] = []
-- groupBy' f [x] = [[x]]
-- groupBy' f (x:xs)
--   | f x (head (head next)) = (x:head next):tail next
--   | otherwise = [x]:next
--   where
--     next = (groupBy' f xs)
groupBy' f xs = foldr step [] xs
  where
    step x [] = [[x]]
    step x acc
      | f x (head (head acc)) = (x:head acc):tail acc
      | otherwise = [x]:acc

values =
  [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]

abc = groupBy' (\x y -> (x > 0) == (y > 0)) values
