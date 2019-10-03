{-# LANGUAGE OverloadedStrings #-}

module Groot.Shell (grootShell) where

import           Control.Monad.IO.Class
import           Data.String
import qualified Data.Text                as T
import           Data.Version             (showVersion)
import           Options.Applicative
import           Options.Applicative.Help
import           Paths_groot              (version)
import           System.Exit              (exitSuccess)
import           System.Posix.Signals

import           Groot.CLI
import           Groot.Console            (askUser)
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

shellPrefs = defaultPrefs

execShellCmd :: GrootShellCmd -> GrootIO ()
execShellCmd (DefaultCmd cmd) = execGrootCmd cmd
execShellCmd HelpCmd =
  let help = parserHelp shellPrefs grootShellCommand
  in liftIO $ print help
execShellCmd ExitCmd = do
  liftIO $ putStrLn "Bye!"
  liftIO exitSuccess
execShellCmd VersionCmd =
  liftIO . putStrLn $ showVersion version

shellInfo :: ParserInfo GrootShellCmd
shellInfo = info grootShellCommand mempty

prompt :: StyledText
prompt = styled blueStyle "groot>"

grootShell :: Env -> IO ()
grootShell env = do
  initShell
  loop

  where
    initShell :: IO ()
    initShell = do
      installHandler keyboardSignal interruptHandler Nothing
      putStrLn ("Welcome to the Groot Shell " ++ (showVersion version) ++ ".")
      putStrLn "Type 'help' for a list of available commands."

    loop :: IO ()
    loop = do
      line <- askUser (prompt <> styleless " ")
      case (T.unpack <$> line) of
        Just cmd -> runCommand $ words cmd
        Nothing  -> pure ()
      loop

    runCommand :: [String] -> IO ()
    runCommand args = do
      result <- pure $ execParserPure shellPrefs shellInfo args
      case result of
        Success cmd -> runGrootT (execShellCmd cmd) env
        Failure err -> do
          (help, _, _) <- pure $ execFailure err ""
          liftIO $ print help
        CompletionInvoked result -> do
          str <- liftIO $ execCompletion result ""
          liftIO $ putStrLn str

    interruptHandler = CatchOnce $ do
      putStrLn ""
      loop
