module Arbitrary
    ( prop_empty_id
    , prop_char
    , prop_text
    , prop_line
    , prop_double
    , prop_hcat
    , prop_punctuate') where

import           Test.QuickCheck (elements, oneof, sample', Arbitrary(arbitrary)
                                , Gen)
import           Control.Monad (liftM, liftM2)
import           Data.List (intersperse)
import           Prelude hiding ((<>))

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
  deriving (Show, Eq)

data Ternary = Yes
             | No
             | Unknown
  deriving (Eq, Show)

instance Arbitrary Ternary where
  arbitrary = elements [Yes, No, Unknown]

abc = sample' (arbitrary :: Gen Ternary)

instance Arbitrary Doc where
  arbitrary = oneof
    [ return Empty
    , liftM Char arbitrary
    , liftM Text arbitrary
    , return Line
    , liftM2 Concat arbitrary arbitrary
    , liftM2 Union arbitrary arbitrary]

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

hcat :: [Doc] -> Doc
hcat = fold (<>)

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d Arbitrary.<> p):punctuate p ds

prop_empty_id x = empty Arbitrary.<> x == x && x Arbitrary.<> empty == x

prop_char c = char c == Char c

prop_text s = text s
  == if null s
     then Empty
     else Text s

prop_line = line == Line

prop_double d = double d == text (show d)

prop_hcat xs = hcat xs == glue xs
  where
    glue [] = empty
    glue (d:ds) = d Arbitrary.<> glue ds

prop_punctuate' s xs = punctuate s xs == combine (intersperse s xs)
  where
    combine [] = []
    combine [x] = [x]
    combine (x:Empty:ys) = x:combine ys
    combine (Empty:y:ys) = y:combine ys
    combine (x:y:ys) = x `Concat` y:combine ys