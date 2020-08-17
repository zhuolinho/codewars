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

abc = myFoldl step [1 .. 100]
  where
    step acc x = x:acc