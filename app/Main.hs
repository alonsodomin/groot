{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Lens
import           Data.Version        (showVersion)
import           Options.Applicative
import           Paths_groot         (version)

import           Groot.CLI
import           Groot.Core          (GrootIO, runGrootT)
import           Groot.Shell

versionInfo :: String
versionInfo = "groot " ++ (showVersion version)

versionOpt :: Parser (a -> a)
versionOpt = infoOption versionInfo $ mconcat [
    long "version"
  , short 'v'
  , help "Show version number"
  ]

data GrootMainCmd =
    DefaultCmd GrootCmd
  | ShellCmd
  deriving (Eq, Show)

mainCommand :: Parser GrootMainCmd
mainCommand = (DefaultCmd <$> grootCommand) <|> hsubparser
  ( command "shell" (info (pure ShellCmd) (progDesc "Starts a shell session"))
  )

data GrootMainOpts = GrootMainOpts
  { _gmOpts :: GrootOpts
  , _gmCmd  :: GrootMainCmd
  } deriving Eq

makeLenses ''GrootMainOpts

mainOpts :: Parser GrootMainOpts
mainOpts = (GrootMainOpts <$> grootOpts <*> mainCommand) <**> versionOpt

execMainCmd :: GrootMainCmd -> GrootIO ()
execMainCmd (DefaultCmd cmd) = execGrootCmd cmd
execMainCmd ShellCmd         = grootShell

mainInfo :: ParserInfo GrootMainOpts
mainInfo = info (mainOpts <**> helper)
  ( fullDesc
  <> progDesc "Utility to manage ECS Clusters"
  <> header "groot" )

main :: IO ()
main = mainProg =<< (execParser mainInfo)
  where
    mainProg opts = do
      env <- loadEnv $ opts ^. gmOpts
      runGrootT (execMainCmd $ opts ^. gmCmd) env
