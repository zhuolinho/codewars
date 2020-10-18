-- file: ch08/GlobRegex.hs
module GlobRegex (globToRegex, matchesGlob) where

import           Text.Regex.Posix ((=~))

type GlobError = String

globToRegex :: String -> Either GlobError String
globToRegex cs = case globToRegex' cs of
  Left err     -> Left err
  Right result -> Right ('^':result ++ "$")

globToRegex' :: String -> Either GlobError String
globToRegex' "" = Right ""
globToRegex' ('*':cs) = case globToRegex' cs of
  Left err     -> Left err
  Right result -> Right (".*" ++ result)
globToRegex' ('?':cs) = case globToRegex' cs of
  Left err     -> Left err
  Right result -> Right ('.':result)
globToRegex' ('[':'!':c:cs) = case charClass cs of
  Left err     -> Left err
  Right result -> Right ("[^" ++ c:result)
globToRegex' ('[':c:cs) = case charClass cs of
  Left err     -> Left err
  Right result -> Right ('[':c:result)
globToRegex' ('[':_) = Left "unterminated character class"
globToRegex' (c:cs) = case globToRegex' cs of
  Left err     -> Left err
  Right result -> Right (escape c ++ result)

charClass :: String -> Either GlobError String
charClass (']':cs) = case globToRegex' cs of
  Left err     -> Left err
  Right result -> Right (']':result)
charClass (c:cs) = case charClass cs of
  Left err     -> Left err
  Right result -> Right (c:result)
charClass [] = Left "unterminated character class"

escape :: Char -> String
escape c
  | c `elem` regexChars = '\\':[c]
  | otherwise = [c]
  where
    regexChars = "\\+()^$.{}]|"

matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat = case globToRegex pat of
  Left _       -> False
  Right result -> name =~ result

abc = globToRegex "[]"
