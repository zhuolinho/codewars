import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck
import           Control.Monad

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

prop_empty_id x = empty Main.<> x == x && x Main.<> empty == x

prop_char c = char c == Char c

prop_text s = text s
  == if null s
     then Empty
     else Text s

prop_line = line == Line

prop_double d = double d == text (show d)