-- file: ch14/Carrier.hs
import qualified Data.Map as M
import           System.Random

type PersonName = String

type PhoneNumber = String

type BillingAddress = String

data MobileCarrier = Honest_Bobs_Phone_Network
                   | Morrisas_Marvelous_Mobiles
                   | Petes_Plutocratic_Phones
  deriving (Eq, Ord)

findCarrierBillingAddress
  :: PersonName
  -> M.Map PersonName PhoneNumber
  -> M.Map PhoneNumber MobileCarrier
  -> M.Map MobileCarrier BillingAddress
  -> Maybe BillingAddress
findCarrierBillingAddress = undefined

variation1 person phoneMap carrierMap addressMap =
  case M.lookup person phoneMap of
    Nothing     -> Nothing
    Just number -> case M.lookup number carrierMap of
      Nothing      -> Nothing
      Just carrier -> M.lookup carrier addressMap

variation2 person phoneMap carrierMap addressMap = do
  number <- M.lookup person phoneMap
  carrier <- M.lookup number carrierMap
  address <- M.lookup carrier addressMap
  return address

variation2a person phoneMap carrierMap addressMap = do
  number <- M.lookup person phoneMap
  carrier <- M.lookup number carrierMap
  M.lookup carrier addressMap

monadic xs ys = do
  x <- xs
  y <- ys
  return (x, y)

comprehensive xs ys = [(x, y) | x <- xs, y <- ys]

blockyPlain xs ys = xs >>= \x -> ys >>= \y -> return (x, y)

blockyPlain_reloaded xs ys = concat
  (map (\x -> concat (map (\y -> return (x, y)) ys)) xs)

guarded True xs = xs
guarded False _ = []

multiplyTo :: Int -> [(Int, Int)]
multiplyTo n = do
  x <- [1 .. n]
  y <- [x .. n]
  guarded (x * y == n) $ return (x, y)

robust xs = do
  (_:x:_) <- Just xs
  return x

wordCount = print . length . words =<< getContents

type SimpleState s a = s -> (a, s)

returnSt :: a -> SimpleState s a
returnSt a = \s -> (a, s)

returnAlt :: a -> SimpleState s a
returnAlt a s = (a, s)

bindSt :: (SimpleState s a) -> (a -> SimpleState s b) -> SimpleState s b
bindSt m k = \s -> let (a, s') = m s
                   in (k a) s'

bindAlt step makeStep oldState = let (result, newState) = step oldState
                                 in (makeStep result) newState

getSt :: SimpleState s s
getSt = \s -> (s, s)

newtype State s a = State { runState :: s -> (a, s) }

returnState :: a -> State s a
returnState a = State $ \s -> (a, s)

bindState :: State s a -> (a -> State s b) -> State s b
bindState m k = State
  $ \s -> let (a, s') = runState m s
          in runState (k a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

twoBadRandoms :: RandomGen g => g -> (Int, Int)
twoBadRandoms gen = (fst $ random gen, fst $ random gen)
