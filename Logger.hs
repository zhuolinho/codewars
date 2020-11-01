-- file: ch14/Logger.hs
module Logger (Logger, Log, runLogger, record) where

import           Control.Monad (liftM, liftM2)

type Log = [String]

newtype Logger a = Logger { execLogger :: (a, Log) }

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

record :: String -> Logger ()
record s = Logger ((), [s])

instance Functor Logger where
  fmap = undefined

instance Applicative Logger where
  pure = undefined

  (<*>) = undefined

instance Monad Logger where
  return a = Logger (a, [])

  m >>= k = let (a, w) = execLogger m
                n = k a
                (b, x) = execLogger n
            in Logger (b, w ++ x)

instance MonadFail Logger where
  fail = undefined

globToRegex cs = globToRegex' cs >>= \ds -> return ('^':ds)

globToRegex' :: String -> Logger String
globToRegex' "" = return "$"
globToRegex' ('?':cs) = record "any" >> globToRegex' cs
  >>= \ds -> return ('.':ds)
globToRegex' ('*':cs) = do
  record "kleene star"
  ds <- globToRegex' cs
  return (".*" ++ ds)
globToRegex' ('[':'!':c:cs) = record "character class, negative"
  >> charClass cs
  >>= \ds -> return ("[^" ++ c:ds)
globToRegex' ('[':c:cs) = record "character class" >> charClass cs
  >>= \ds -> return ("[" ++ c:ds)
globToRegex' ('[':_) = fail "unterminated character class"
globToRegex' (c:cs) = liftM2 (++) (escape c) (globToRegex' cs)

escape :: Char -> Logger String
escape c
  | c `elem` regexChars = record "escape" >> return ['\\', c]
  | otherwise = return [c]
  where
    regexChars = "\\+()^$.{}]|"

charClass (']':cs) = (']':) `liftM` globToRegex' cs
charClass (c:cs) = (c:) `liftM` charClass cs

