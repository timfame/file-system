module Task6
  ( FS (..)
  , getDirectory
  , _File
  , _Dir
  , name
  , contents
  ) where

import System.Directory      ( doesFileExist
                             , doesDirectoryExist
                             , listDirectory
                             )
import System.FilePath.Posix ( splitPath
                             , takeFileName
                             , (</>)
                             )
import Lens.Micro            ( Lens'
                             , lens
                             , Traversal'
                             )

data FS
  = Dir
  { _name     :: FilePath
  , _contents :: [FS]
  }
  | File
  { _name     :: FilePath
  }
  deriving (Show)

getDirectory :: FilePath -> IO FS
getDirectory fp = do
  isFile <- doesFileExist fp
  isDir  <- doesDirectoryExist fp
  if isFile
    then return $ File $ takeFileName fp
    else if isDir
      then do
        dirContentNames <- listDirectory fp
        dirContent <- mapM (getDirectory . ((</>) fp)) dirContentNames
        return $ Dir (last $ splitPath fp) dirContent
      else fail (show fp ++ " does not exist")

_File :: Traversal' FS FS
_File t f@(File _)  = t f
_File _ d@(Dir _ _) = pure d

_Dir :: Traversal' FS FS
_Dir _ f@(File _)  = pure f
_Dir t d@(Dir _ _) = t d

name :: Lens' FS FilePath
name = lens _name (\f n -> f {_name = n})

fakecontents :: Lens' FS [FS]
fakecontents = lens _contents (\f c -> f {_contents = c})

contents :: Traversal' FS [FS]
contents = _Dir.fakecontents
