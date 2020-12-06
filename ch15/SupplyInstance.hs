newtype Reader e a = R { runReader :: e -> a }

instance Monad (Reader e) where
  return a = R $ \_ -> a

  m >>= k = R $ \r -> runReader (k (runReader m r)) r

instance Functor (Reader e) where
  fmap f m = R $ \r -> f (runReader m r)

instance Applicative (Reader e) where
  pure = R . const

  f <*> x = R $ \r -> runReader f r (runReader x r)

ask :: Reader e e
ask = R id

abc = runReader (ask >>= \x -> return (x * 3)) 2