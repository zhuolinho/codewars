import           Supply
import           System.Random hiding (next)
import           Control.Arrow (first)

randomsIO_golfed :: Random a => IO [a]
randomsIO_golfed = getStdRandom (first randoms . split)

randomsIO :: Random a => IO [a]
randomsIO = getStdRandom
  $ \g -> let (a, b) = split g
          in (randoms a, b)
-- (fst . runSupply next) `fmap` randomsIO