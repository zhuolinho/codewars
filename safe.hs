import           Data.Char (digitToInt)

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

digitToInt' :: Char -> Either ErrorMessage Int
digitToInt' x
  | elem x ['0' .. '9'] = Right (digitToInt x)
  | otherwise = Left ("non-digit '" ++ x:"'")

fold acc [] = Right acc
fold acc (x:xs) = case digitToInt' x of
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
    step x acc@(h@(hh:_):t)
      | f x hh = (x:h):t
      | otherwise = [x]:acc

values =
  [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]

any' f xs = foldr step False xs
  where
    step x acc = (f x) || acc

cycle' xs = foldr (:) (cycle' xs) xs

words' xs = let result = foldr step [[]] xs
                  where
                    step ' ' acc@("":_) = acc
                    step ' ' acc = []:acc
                    step x (h:t) = (x:h):t
            in if head result == ""
               then tail result
               else result

unlines' xs = foldr step "" xs
  where
    step x acc = x ++ '\n':acc

abc = groupBy' (\x y -> x * y > 0) values