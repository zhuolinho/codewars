{-# LANGUAGE ScopedTypeVariables #-}

import           Prelude hiding (traverse)
import           Control.Monad (liftM, forM)
import           System.Directory (Permissions(..), getModificationTime
                                 , getPermissions, getDirectoryContents)
import           Data.Time.Clock (UTCTime(..))
import           System.FilePath ((</>))
import           Control.Exception (bracket, handle, SomeException)
import           System.IO (IOMode(..), hClose, hFileSize, openFile)
import           Data.List (sortBy)

maybeIO act = handle (\(_ :: SomeException) -> return Nothing) (liftM Just act)

data Info = Info { infoPath :: FilePath
                 , infoPerms :: Maybe Permissions
                 , infoSize :: Maybe Integer
                 , infoModTime :: Maybe UTCTime
                 }
  deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
  names <- getUsefulContents path
  contents <- mapM getInfo (path:map (path </>) names)
  liftM concat
    $ forM (order contents)
    $ \info -> do
      if isDirectory info && infoPath info /= path
        then traverse order (infoPath info)
        else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

type InfoP a = Info -> a

liftP :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP q f g = \i -> f i `q` g i

traverseWrapper
  :: (Info -> Bool) -> ([Info] -> [Info]) -> FilePath -> IO [Info]
traverseWrapper fil order path = traverse (order . filter fil) path

main = do
  infos <- traverse (sortBy (flip compare)) ".."
  -- infos <- traverse id ".."
  return (map infoPath infos)
