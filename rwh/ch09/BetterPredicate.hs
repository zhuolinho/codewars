{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad (filterM)
import           System.Directory (Permissions(..), getModificationTime
                                 , getPermissions)
import           Data.Time.Clock (UTCTime)
import           System.FilePath (takeExtension)
import           Control.Exception (bracket, handle, SomeException)
import           System.IO (IOMode(..), hClose, hFileSize, openFile)
import           RecursiveContents (getRecursiveContents)

type Predicate = FilePath      -- path to directory entry
  -> Permissions   -- permissions
  -> Maybe Integer -- file size (Nothing if not file)
  -> UTCTime     -- last modified
  -> Bool

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle (\(_ :: SomeException) -> return Nothing)
  $ bracket (openFile path ReadMode) hClose
  $ \h -> do
    size <- hFileSize h
    return (Just size)

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where
    check name = do
      perms <- getPermissions name
      size <- getFileSize name
      modified <- getModificationTime name
      return (p name perms size modified)

saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path = handle (\(_ :: SomeException) -> return Nothing)
  $ do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return (Just size)

type InfoP a = FilePath        -- path to directory entry
  -> Permissions     -- permissions
  -> Maybe Integer   -- file size (Nothing if not file)
  -> UTCTime       -- last modified
  -> a

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

constP :: a -> InfoP a
constP k _ _ _ _ = k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k = liftP2 q f $ constP k

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

andP = liftP2 (&&)

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)

lesserP = liftP (<)

(==?) = equalP

(&&?) = andP

(>?) = greaterP

infix 4 ==?

infixr 3 &&?

infix 4 >?

myTest4 = liftPath takeExtension ==? ".cpp" &&? sizeP >? 131072