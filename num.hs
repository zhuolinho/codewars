-- file: ch13/num.hs
import           Data.List

--------------------------------------------------
-- Symbolic/units manipulation
--------------------------------------------------
-- The "operators" that we're going to support
data Op = Plus
        | Minus
        | Mul
        | Div
        | Pow
  deriving (Eq, Show)

{- The core symbolic manipulation type.  It can be a simple number,
a symbol, a binary arithmetic operation (such as +), or a unary
arithmetic operation (such as cos)

Notice the types of BinaryArith and UnaryArith: it's a recursive
type.  So, we could represent a (+) over two SymbolicManips. -}
data SymbolicManip a = Number a           -- Simple number, such as 5
                     | Symbol String      -- A symbol, such as x
                     | BinaryArith Op (SymbolicManip a) (SymbolicManip a)
                     | UnaryArith String (SymbolicManip a)
  deriving (Eq)

{- SymbolicManip will be an instance of Num.  Define how the Num
operations are handled over a SymbolicManip.  This will implement things
like (+) for SymbolicManip. -}
instance Num a => Num (SymbolicManip a) where
  a + b = BinaryArith Plus a b

  a - b = BinaryArith Minus a b

  a * b = BinaryArith Mul a b

  negate a = BinaryArith Mul (Number (-1)) a

  abs a = UnaryArith "abs" a

  signum _ = error "signum is unimplemented"

  fromInteger i = Number (fromInteger i)