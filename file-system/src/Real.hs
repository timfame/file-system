{-# LANGUAGE FlexibleInstances #-}

module Real
  ( FSOutput (..)
  , FSActions (..)
  , RealFS (..)
  ) where

import Commands                   ( Command (..)
                                  , getCommand)
import Control.Monad              ( when)
import Control.Monad.Except       ( ExceptT (..)
                                  , catchError
                                  , throwError)
import Control.Monad.IO.Class     ( liftIO)
import Control.Monad.State.Lazy   ( State
                                  , get
                                  , modify)
import Control.Monad.Trans.Class  ( lift)
import Control.Monad.Trans.Reader ( ReaderT (..)
                                  , ask)
import Data.Char                  ( isSpace)
import Data.IORef                 ( IORef
                                  , modifyIORef
                                  , newIORef
                                  , readIORef)
import Data.List                  ( intercalate)
import Data.List.Split            ( splitOn)
import Data.Time.Clock            ( UTCTime)
import System.Directory           ( Permissions
                                  , canonicalizePath
                                  , createDirectory
                                  , doesFileExist
                                  , findFiles
                                  , getCurrentDirectory
                                  , getFileSize
                                  , getModificationTime
                                  , getPermissions
                                  , listDirectory
                                  , removeDirectoryRecursive
                                  , removeFile)
import System.FilePath            ( (</>))
import System.FilePath.Posix      ( takeExtension)
import System.IO                  ( hFlush
                                  , stdout)

-- | The `FSOutput` is a common data for possible outputs of all commands in FileSystem
data FSOutput
  -- | The `Empty` constructor destined for commands with no output such as: `CD`, `MkDir`, `Touch`, `Rm`, `Write`
  = Empty
  -- | The `FilesList` constructor destined for commands with output with list of file pathes such as: `Dir`, `LS`, `Find`
  | FilesList [FilePath]
  -- | The `StringData` constructor destined for command `Cat`
  | StringData String
  -- | The `FileInfo` constuctor destined for command `Info` for some file
  | FileInfo FilePath Permissions String UTCTime Integer
  -- | The `DirInfo` constructor destined for command `Info` for some directory
  | DirInfo FilePath Permissions Integer Integer
  deriving (Eq)

instance Show FSOutput where
  show Empty = ""
  show (FilesList ps) = show ps
  show (StringData s) = s
  show (FileInfo path perms ext time sz) = "\nPath: " ++ show path ++ "\nPermissions: " ++ show perms ++ "\nType: " ++ show ext ++ "\nLast modification time: " ++ show time ++ "\nSize: " ++ show sz
  show (DirInfo path perms sz filesCnt) = "\nPath: " ++ show path ++ "\nPermissions: " ++ show perms ++ "\nSize: " ++ show sz ++ "\nFiles Count: " ++ show filesCnt

-- |The `FSActions` is a type-class for Real and Test(Mock) FileSystems
class Monad m => FSActions m where
  -- | The `exit` method exists FileSystem  
  exit :: m FSOutput
  -- | The `dir` method shows all content of current directory
  dir :: m FSOutput
  -- | The `cd` method change current directory
  cd :: FilePath    -> m FSOutput
  -- | The `ls` method shows all content of specified directory 
  ls :: FilePath    -> m FSOutput
  -- | The `mkdir` method creates a new directory with the specified path
  mkdir :: String   -> m FSOutput
  -- | The `cat` method shows content of specified file
  cat :: FilePath   -> m FSOutput
  -- | The `touch` method creates a new empty file with specified path
  touch :: FilePath -> m FSOutput
  -- | The `rm` method removes specified directory or file
  rm :: FilePath    -> m FSOutput
  -- | The `write` method write some text to the file
  write :: FilePath -> String -> m FSOutput
  -- | The `find` method finds all files with corresponding name in current directory and all subdirectories
  find :: String    -> m FSOutput
  -- | The `info` method shows information about specified directory or file
  info :: FilePath  -> m FSOutput

  getSubDirectories :: FilePath  -> m [FilePath]
  getFullPathContent :: FilePath -> m [FilePath]

  fileInfo :: FilePath -> m FSOutput
  dirInfo :: FilePath  -> m FSOutput

  sizeOf :: FilePath  -> m Integer
  countOf :: FilePath -> m Integer

  getCurrentPath :: m FilePath
  resolvePath :: FilePath -> m FilePath

type RealFS = ReaderT (IORef FilePath) IO

instance FSActions RealFS where
  getCurrentPath = do
    state <- ask
    cur <- liftIO $ readIORef state
    return cur

  resolvePath path = do
    state <- ask
    cur <- liftIO $ readIORef state
    lift $ canonicalizePath $ cur </> path

  exit = do
    return Empty

  dir = do
    absPath <- resolvePath ""
    content <- liftIO $ listDirectory absPath
    return $ FilesList content

  cd path = do
    absPath <- resolvePath path
    state <- ask
    liftIO $ modifyIORef state (\_ -> absPath)
    return Empty

  ls path = do
    absPath <- resolvePath path
    content <- liftIO $ listDirectory absPath
    return $ FilesList content

  mkdir path = do
    absPath <- resolvePath path
    liftIO $ createDirectory absPath
    return Empty

  cat path = do
    absPath <- resolvePath path
    content <- liftIO $ readFile absPath
    return $ StringData content

  touch path = do
    absPath <- resolvePath path
    liftIO $ writeFile absPath ""
    return Empty

  rm path = do
    absPath <- resolvePath path
    isF <- liftIO $ doesFileExist absPath
    if isF
      then liftIO $ removeFile absPath
      else liftIO $ removeDirectoryRecursive absPath
    return Empty

  write path text = do
    absPath <- resolvePath path
    liftIO $ writeFile absPath text
    return Empty

  find name = do
    absPath <- resolvePath ""
    subDirs <- getSubDirectories absPath
    content <- liftIO $ findFiles subDirs name
    return $ FilesList content

  getSubDirectories absPath = do
    isF <- liftIO $ doesFileExist absPath
    if isF
      then return []
      else do
        lstDir <- getFullPathContent absPath
        dirs <- concat <$> (mapM getSubDirectories lstDir)
        return (absPath : dirs)

  info path = do
    absPath <- resolvePath path
    isF <- liftIO $ doesFileExist absPath
    if isF
      then fileInfo absPath
      else dirInfo absPath

  fileInfo absPath = do
    perms <- liftIO $ getPermissions absPath
    let ext = takeExtension absPath
    time <- liftIO $ getModificationTime absPath
    sz <- liftIO $ getFileSize absPath
    return $ FileInfo absPath perms ext time sz

  dirInfo absPath = do
    perms <- liftIO $ getPermissions absPath
    sz <- sizeOf absPath
    cnt <- countOf absPath
    return $ DirInfo absPath perms sz cnt

  sizeOf absPath = do
    isF <- liftIO $ doesFileExist absPath
    if isF
      then do
        sz <- liftIO $ getFileSize absPath
        return sz
      else do
        lstDir <- getFullPathContent absPath
        sz <- sum <$> (mapM sizeOf lstDir)
        return sz

  countOf absPath = do
    isF <- liftIO $ doesFileExist absPath
    if isF
      then return 1
      else do
        lstDir <- getFullPathContent absPath
        cnt <- sum <$> (mapM countOf lstDir)
        return cnt

  getFullPathContent absPath = do
    lstDir <- liftIO $ listDirectory absPath
    absDirPaths <- mapM (\path -> return $ absPath </> path) lstDir
    return absDirPaths



--  FileSystem for tests

tree :: MockFSTree
tree =
  MockDir
    "/"
    [ ( MockDir
          "tim"
          [ (MockFile "tim1" "hhh"),
            (MockFile "tim2.txt" "www")
          ]
      ),
      (MockFile "test1" "hello"),
      (MockFile "test2.txt" "world")
    ]

handler :: MockFSTreeHandler
handler = MockFSTreeHandler "/" tree

data MockFSTree
  = MockDir
      { dirName :: String,
        dirEntities :: [MockFSTree]
      }
  | MockFile
      { fileName :: String,
        fileContent :: String
      }
  deriving (Show, Eq)

data MockFSTreeHandler = MockFSTreeHandler
  { currentPath :: String,
    fsTree :: MockFSTree
  }
  deriving (Show)

data MockException
  = FileOrDirNotExist String
  | NoSuchDirectory String
  | SomeExc String
  deriving (Show)

type MockFS = ExceptT MockException (State MockFSTreeHandler)

class Monad m => MockHelpers m where
  findEntity :: [MockFSTree] -> String -> m (Maybe MockFSTree)
  goto :: String -> [String] -> MockFSTree -> m MockFSTree
  entityNames :: [MockFSTree] -> m [String]
  splitPath :: FilePath -> m [String]

instance MockHelpers MockFS where
  findEntity [] _ = return $ Nothing
  findEntity (d@(MockDir n _) : rest) s
    | n == s = return $ Just d
    | otherwise = findEntity rest s
  findEntity ((MockFile _ _) : rest) s = findEntity rest s
  goto _ [] t = do
    return t
  goto allPath (p : ps) t = do
    if p == ""
      then goto allPath ps t
      else do
        case t of
          (MockDir _ es) -> do
            foundEntity <- findEntity es p
            case foundEntity of
              Nothing -> throwError $ FileOrDirNotExist allPath
              (Just entity) -> goto allPath ps entity
          _ -> return t

  entityNames [] = return []
  entityNames ((MockFile n _) : ts) = do
    rest <- entityNames ts
    return (n : rest)
  entityNames ((MockDir n _) : ts) = do
    rest <- entityNames ts
    return (n : rest)

  splitPath newPath
    | newPath == "" = return []
    | newPath == "/" = return []
    | otherwise = return $ splitOn "/" newPath

instance FSActions MockFS where
  resolvePath newPath = do
    treeHandler <- lift get
    splitted <- splitPath ((currentPath treeHandler) </> newPath)
    return $ intercalate "/" $ reverse $ canonicalize [] splitted
    where
      canonicalize :: [String] -> [String] -> [String]
      canonicalize [] (next : rest) = canonicalize [next] rest
      canonicalize stack [] = stack
      canonicalize (st : sts) (next : rest)
        | next == ".." = canonicalize sts rest
        | otherwise = canonicalize (next : st : sts) rest

  getCurrentPath = do
    treeHandler <- lift get
    return $ currentPath treeHandler

  exit = do
    return Empty

  dir = do
    ls ""

  cd newPath = do
    th <- lift get
    p <- resolvePath newPath
    splitted <- splitPath p
    newCD <- goto p splitted $ fsTree th
    case newCD of
      (MockDir _ _) -> lift $ modify (\x -> x {currentPath = p})
      (MockFile _ _) -> throwError $ NoSuchDirectory p
    return Empty

  ls newPath = do
    treeHandler <- lift get
    p <- resolvePath newPath
    splitted <- splitPath p
    curDir <- goto p splitted $ fsTree treeHandler
    case curDir of
      (MockFile _ _) -> do throwError $ FileOrDirNotExist p
      (MockDir _ es) -> do
        content <- entityNames es
        return $ FilesList content

  mkdir _ = do
    return Empty

  cat _ = do
    return Empty

  touch _ = do
    return Empty

  rm _ = do
    return Empty

  write _ _ = do
    return Empty

  find _ = do
    return Empty

  getSubDirectories _ = do
    return [""]

  info _ = do
    return Empty

  fileInfo _ = do
    return Empty

  dirInfo _ = do
    return Empty

  sizeOf _ = do
    return 0

  countOf _ = do
    return 0

  getFullPathContent _ = do
    return [""]