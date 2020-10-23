import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
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