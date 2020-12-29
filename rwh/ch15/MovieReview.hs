import           Control.Monad (MonadPlus(..), liftM, liftM2, liftM3)

data MovieReview =
  MovieReview { revTitle :: String, revUser :: String, revReview :: String }

simpleReview :: [(String, Maybe String)] -> Maybe MovieReview
simpleReview alist = case lookup "title" alist of
  Just (Just title@(_:_)) -> case lookup "user" alist of
    Just (Just user@(_:_)) -> case lookup "review" alist of
      Just (Just review@(_:_)) -> Just (MovieReview title user review)
      _ -> Nothing -- no review
    _ -> Nothing -- no user
  _ -> Nothing -- no title

maybeReview alist = do
  title <- lookup1 "title" alist
  user <- lookup1 "user" alist
  review <- lookup1 "review" alist
  return (MovieReview title user review)

lookup1 key alist = case lookup key alist of
  Just (Just s@(_:_)) -> Just s
  _ -> Nothing

liftedReview alist = liftM3
  MovieReview
  (lookup1 "title" alist)
  (lookup1 "user" alist)
  (lookup1 "review" alist)

apReview alist = MovieReview `liftM` lookup1 "title" alist
  `app` lookup1 "user" alist
  `apa` lookup1 "review" alist

apa = liftM2 id

app = liftM2 ($)

data Context = Home
             | Mobile
             | Business
  deriving (Eq, Show)

type Phone = String

albulena = [(Home, "+355-652-55512")]

nils = [ (Mobile, "+47-922-55-512")
       , (Business, "+47-922-12-121")
       , (Home, "+47-925-55-121")
       , (Business, "+47-922-25-551")]

twalumba = [(Business, "+260-02-55-5121")]

onePersonalPhone :: [(Context, Phone)] -> Maybe Phone
onePersonalPhone ps = case lookup Home ps of
  Nothing -> lookup Mobile ps
  Just n  -> Just n

allBusinessPhones :: [(Context, Phone)] -> [Phone]
allBusinessPhones ps = map snd numbers
  where
    numbers = case filter (contextIs Business) ps of
      [] -> filter (contextIs Mobile) ps
      ns -> ns

contextIs a (b, _) = a == b

oneBusinessPhone :: [(Context, Phone)] -> Maybe Phone
oneBusinessPhone ps = lookup Business ps `mplus` lookup Mobile ps

allPersonalPhones :: [(Context, Phone)] -> [Phone]
allPersonalPhones ps = map snd
  $ filter (contextIs Home) ps `mplus` filter (contextIs Mobile) ps

lookup2 :: (Eq a) => a -> [(a, b)] -> Maybe b
lookup2 _ [] = Nothing
lookup2 k ((x, y):xys)
  | x == k = Just y
  | otherwise = lookup k xys

lookupM :: (MonadPlus m, Eq a) => a -> [(a, b)] -> m b
lookupM _ [] = mzero
lookupM k ((x, y):xys)
  | x == k = return y `mplus` lookupM k xys
  | otherwise = lookupM k xys

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

x `zeroMod` n = guard ((x `mod` n) == 0) >> return x