{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Commands                   ( Command (..)
                                  , getCommand)
import Control.Monad              ( when)
import Control.Monad.Except       ( catchError)
import Control.Monad.IO.Class     ( liftIO)
import Control.Monad.Trans.Reader ( ReaderT (..))
import Data.Char                  ( isSpace)
import Data.IORef                 ( newIORef)
import System.Directory           ( getCurrentDirectory)
import System.IO                  ( hFlush
                                  , stdout)
import Real                       ( FSOutput (..)
                                  , FSActions (..)
                                  , RealFS)

-- |Running FileSystem CLI
main :: IO ()
main = do
  curDir <- getCurrentDirectory
  ior <- newIORef curDir
  runReaderT cliHandler ior

-- |The `blank` function checks if string consists only of whitespaces 
blank :: String -> Bool
blank s = all isSpace s

-- |The `cliHandler` function reads input, with help of `getCommand` parses it and execute corresponding FileSystem command
-- It does it infinitely until the `Exit` command will not be entered
cliHandler :: RealFS ()
cliHandler = do
  currentDir <- getCurrentPath
  liftIO $ putStr $ currentDir ++ " > "
  liftIO $ hFlush stdout
  input <- liftIO getLine
  if (blank input)
    then cliHandler
    else do
      let maybeCmd = getCommand input
      case maybeCmd of
        Nothing -> do
          liftIO $ print "Cannot parse input"
          cliHandler
        (Just Exit) -> do
          liftIO $ print "Good bye :("
          return ()
        (Just cmd) -> do
          output <- (commandExecutor cmd) `catchError` (\e -> return $ StringData $ "Error: " ++ show e)
          when (output /= Empty) $ liftIO $ print output
          cliHandler
 
commandExecutor :: Command -> RealFS FSOutput
commandExecutor cmd = do
  output <- case cmd of
    Exit              -> exit
    Dir               -> dir
    (Cd path)         -> cd path
    (Ls path)         -> ls path
    (MkDir path)      -> mkdir path
    (Cat path)        -> cat path
    (Touch path)      -> touch path
    (Rm path)         -> rm path
    (Write path text) -> write path $ unwords text
    (Find name)       -> find name
    (Info path)       -> info path
  return output