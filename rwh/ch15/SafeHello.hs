module SafeHello where

import           MonadHandle (MonadHandle(hClose, openFile, hPutStrLn))
import           System.IO (IOMode(..))

safeHello :: MonadHandle h m => FilePath -> m ()
safeHello path = do
  h <- openFile path WriteMode
  hPutStrLn h "hello world"
  hClose h