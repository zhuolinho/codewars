-- file: ch11/Run.hs
module Main where

import           Arbitrary (prop_empty_id, prop_char, prop_text, prop_line
                          , prop_double, prop_hcat, prop_punctuate')
import           Test.QuickCheck (quickCheckWithResult
                                , Args(Args, replay, maxSuccess, maxDiscardRatio, maxSize, chatty))

anal :: Args
anal = Args { replay = Nothing
            , maxSuccess = 1000
            , maxDiscardRatio = 1
            , maxSize = 1000
            , chatty = True
            }

minimal :: Args
minimal = Args { replay = Nothing
               , maxSuccess = 200
               , maxDiscardRatio = 1
               , maxSize = 200
               , chatty = True
               }

runTests :: Args -> IO ()
runTests args = do
  f prop_empty_id "empty_id ok?"
  f prop_char "char ok?"
  f prop_text "text ok?"
  f prop_line "line ok?"
  f prop_double "double ok?"
  f prop_hcat "hcat ok?"
  f prop_punctuate' "punctuate ok?"
  where
    f prop str = do
      putStrLn str
      quickCheckWithResult args prop
      return ()

main :: IO ()
main = do
  putStrLn "Choose test depth"
  putStrLn "1. Anal"
  putStrLn "2. Minimal"
  depth <- readLn
  if depth == 1
    then runTests anal
    else runTests minimal