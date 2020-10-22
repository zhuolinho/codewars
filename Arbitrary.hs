-- import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

class Arbitrary a where
  arbitrary :: Gen a

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

instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (x, y)