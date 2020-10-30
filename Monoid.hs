{-# LANGUAGE GeneralizedNewtypeDeriving #-}

class Monoid a where
  mempty :: a                -- the identity

  mappend :: a -> a -> a      -- associative binary operator

instance Main.Monoid [a] where
  mempty = []

  mappend = (++)

newtype AInt = A { unA :: Int }
  deriving (Show, Eq, Num)

-- monoid under addition
instance Main.Monoid AInt where
  mempty = 0

  mappend = (+)

newtype MInt = M { unM :: Int }
  deriving (Show, Eq, Num)

-- monoid under multiplication
instance Main.Monoid MInt where
  mempty = 1

  mappend = (*)

abc = 2 `Main.mappend` 5 :: MInt

xyz = 2 `Main.mappend` 5 :: AInt