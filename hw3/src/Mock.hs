-- {-# LANGUAGE FlexibleInstances #-}

-- module Mock (
--     -- MockFSTree (..),
--     -- MockFSTreeHandler (..),
--     -- MockException (..),
--     -- MockFS,
--     -- FSOutput (..),
--     -- FSActions (..)
--     ) where

-- import System.FilePath            ( (</>))
-- import Control.Monad              ( when)
-- import Control.Monad.Except       ( ExceptT
--                                   , runExceptT
--                                   , throwError)
-- import Control.Monad.State.Lazy   ( State
--                                   , runState
--                                   , get
--                                   , modify)
-- import Data.List                  ( intercalate)
-- import Data.List.Split            ( splitOn)
-- import Control.Monad.Trans.Class  ( lift)
-- import System.Directory           ( Permissions)
-- import Data.Time.Clock            ( UTCTime)

-- data FSOutput 
--     = Empty
--     | FilesList  [FilePath]
--     | StringData String
--     | FileInfo   FilePath Permissions String UTCTime Integer
--     | DirInfo    FilePath Permissions Integer Integer
--     deriving (Eq)

-- instance Show FSOutput where
--     show Empty = ""
--     show (FilesList ps)  = show ps
--     show (StringData s)  = s
--     show (FileInfo path perms ext time sz) = "\nPath: " ++ show path ++ "\nPermissions: " ++ show perms ++ "\nType: " ++ show ext ++ "\nLast modification time: " ++ show time ++ "\nSize: " ++ show sz
--     show (DirInfo  path perms sz filesCnt) = "\nPath: " ++ show path ++ "\nPermissions: " ++ show perms ++ "\nSize: " ++ show sz ++ "\nFiles Count: " ++ show filesCnt

-- class Monad m => FSActions m where
--     exit  :: m FSOutput
--     dir   :: m FSOutput
--     cd    :: FilePath -> m FSOutput
--     ls    :: FilePath -> m FSOutput
--     mkdir :: String   -> m FSOutput
--     cat   :: FilePath -> m FSOutput
--     touch :: FilePath -> m FSOutput
--     rm    :: FilePath -> m FSOutput
--     write :: FilePath -> String -> m FSOutput
--     find  :: String   -> m FSOutput
--     info  :: FilePath -> m FSOutput

--     getSubDirectories  :: FilePath -> m [FilePath]
--     getFullPathContent :: FilePath -> m [FilePath]

--     fileInfo :: FilePath -> m FSOutput
--     dirInfo  :: FilePath -> m FSOutput

--     sizeOf :: FilePath -> m Integer
--     countOf :: FilePath -> m Integer

--     getCurrentPath :: m FilePath
--     resolvePath    :: FilePath -> m FilePath


