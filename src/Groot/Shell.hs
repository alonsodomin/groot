{-# LANGUAGE OverloadedStrings #-}

module Groot.Shell (grootShell) where

import           Control.Monad.IO.Class
import           Data.String
import qualified Data.Text                as T
import           Data.Version             (showVersion)
import           Options.Applicative      hiding (defaultPrefs)
import qualified Options.Applicative      as Opts
import           Options.Applicative.Help (parserHelp)
import           Paths_groot              (version)
import           System.Console.Haskeline hiding (defaultPrefs, defaultSettings,
                                           display)
import qualified System.Console.Haskeline as Line
import           System.Directory         (createDirectoryIfMissing,
                                           getHomeDirectory)
import           System.Exit              (exitSuccess)

import           Groot.CLI
import           Groot.Console            (display)
import           Groot.Core
import           Groot.Internal.Data.Text

data GrootShellCmd =
    DefaultCmd GrootCmd
  | HelpCmd
  | ExitCmd
  | VersionCmd
  deriving (Eq, Show)

grootShellCommand :: Parser GrootShellCmd
grootShellCommand = (DefaultCmd <$> grootCommand) <|> hsubparser
   ( command "help" (info (pure HelpCmd) (progDesc "Displays this help"))
  <> command "version" (info (pure VersionCmd) (progDesc "Shows current version"))
  <> command "exit" (info (pure ExitCmd) (progDesc "Exits the shell"))
  )

shellParserPrefs :: ParserPrefs
shellParserPrefs = Opts.defaultPrefs

shellParserInfo :: ParserInfo GrootShellCmd
shellParserInfo = info grootShellCommand mempty

loadShellSettings :: IO (Settings IO)
loadShellSettings = do
  userHome <- getHomeDirectory
  let homeFolder = userHome ++ "/.groot"
  createDirectoryIfMissing True homeFolder
  return $ Line.defaultSettings { historyFile = Just $ homeFolder ++ "/history" }

execShellCmd :: GrootShellCmd -> GrootIO ()
execShellCmd (DefaultCmd cmd) = execGrootCmd cmd
execShellCmd HelpCmd =
  let helpDesc = parserHelp shellParserPrefs grootShellCommand
  in liftIO $ print helpDesc
execShellCmd ExitCmd = do
  liftIO $ putStrLn "Bye!"
  liftIO exitSuccess
execShellCmd VersionCmd =
  liftIO . putStrLn $ showVersion version

welcomeMsg :: StyledText
welcomeMsg = (styleless "Welcome to the Groot Shell" <+> (styleless . T.pack $ showVersion version) <> (styleless "."))
  </> ("Type '" <> (styled yellowStyle "help") <> "' for a list of available commands.")

grootShell :: Env -> IO ()
grootShell env = do
  display welcomeMsg
  settings <- loadShellSettings
  runInputT settings loop

  where
    loop :: InputT IO ()
    loop = withInterrupt $ do
      mline <- getInputLine "groot> "
      case mline of
        Just ""  -> return ()
        Just cmd -> handle cancelledHandler (runCommand $ words cmd)
        Nothing  -> return ()
      loop

    runCommand :: [String] -> InputT IO ()
    runCommand args = liftIO $ do
      result <- pure $ execParserPure shellParserPrefs shellParserInfo args
      case result of
        Success cmd -> runGrootT (execShellCmd cmd) env
        Failure err -> do
          (helpDesc, _, _) <- pure $ execFailure err ""
          liftIO $ print helpDesc
        CompletionInvoked res -> do
          str <- liftIO $ execCompletion res ""
          liftIO $ putStrLn str

    cancelledHandler Interrupt = outputStrLn "Cancelled."
