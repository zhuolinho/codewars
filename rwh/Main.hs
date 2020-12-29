module Main (main) where

import           SimpleJSON

main = print
  (pretty
     20
     (renderJValue (JObject [("foo", JNumber 1), ("bar", JBool False)])))