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
    step xs [] = xs
    step [] ys = ys
    step (x:xs) (y:ys) = func x y:step xs ys

abc = zipWithN (++) $ map (map (:[])) ["abc", "defg", "hij"]