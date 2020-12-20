module Commands
  ( Command (..)
  ,  getCommand
  )
where

import Options.Applicative ( Parser
                           , ParserInfo
                           , argument
                           , command
                           , execParserPure
                           , getParseResult
                           , helper
                           , info
                           , many
                           , metavar
                           , prefs
                           , progDesc
                           , showHelpOnEmpty
                           , str
                           , subparser)

type Text = [String]

-- |Data type for all possible commands in filesystem
data Command
  = Dir
  | Cd FilePath
  | Ls FilePath
  | MkDir FilePath
  | Cat FilePath
  | Touch FilePath
  | Rm FilePath
  | Write FilePath Text
  | Find String
  | Info FilePath
  | Exit
  deriving (Eq)

-- |Add --help option for every option with corresponding description
withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseDir :: Parser Command
parseDir = pure Dir

parseCd :: Parser Command
parseCd = Cd <$> argument str (metavar "DIRECTORY")

parseLs :: Parser Command
parseLs = Ls <$> argument str (metavar "DIRECTORY")

parseMkDir :: Parser Command
parseMkDir = MkDir <$> argument str (metavar "DIRECTORY")

parseCat :: Parser Command
parseCat = Cat <$> argument str (metavar "FILE")

parseTouch :: Parser Command
parseTouch = Touch <$> argument str (metavar "FILE")

parseRm :: Parser Command
parseRm = Rm <$> argument str (metavar "FILE OR DIRECTORY")

parseWrite :: Parser Command
parseWrite =
  Write
    <$> argument str (metavar "FILE")
    <*> (many $ argument str (metavar "WRITE CONTENT"))

parseFind :: Parser Command
parseFind = Find <$> argument str (metavar "FILE NAME")

parseInfo :: Parser Command
parseInfo = Info <$> argument str (metavar "FILE OR DIRECTORY")

parseExit :: Parser Command
parseExit = pure Exit

parseCommand :: Parser Command
parseCommand =
  subparser $
    command "dir" (parseDir `withInfo` "List all content of current directory")
      <> command "cd" (parseCd `withInfo` "Change directory")
      <> command "ls" (parseLs `withInfo` "List all content of directory")
      <> command "mkdir" (parseMkDir `withInfo` "Create new directory")
      <> command "cat" (parseCat `withInfo` "Show file content")
      <> command "touch" (parseTouch `withInfo` "Create new empty file")
      <> command "rm" (parseRm `withInfo` "Delete file or directory")
      <> command "write" (parseWrite `withInfo` "Write text to file")
      <> command "find" (parseFind `withInfo` "Find file in current directory and all subdirectories")
      <> command "info" (parseInfo `withInfo` "Show info about file or directory")
      <> command "exit" (parseExit `withInfo` "Exit FileSystem :(")

-- |The `getCommand` function parses line into special `Commands` for filesystem with its arguments 
-- It takes one argument, of type `String`
getCommand :: String -> Maybe Command
getCommand input = getParseResult $ execParserPure (prefs showHelpOnEmpty) (parseCommand `withInfo` "Interact with FileSystem") $ words input