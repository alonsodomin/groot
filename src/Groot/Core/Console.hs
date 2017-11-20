{-# LANGUAGE FlexibleContexts #-}

module Groot.Core.Console where

import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Char
import System.IO
import System.Console.ANSI

blueText :: [SGR]
blueText = [SetColor Foreground Dull Blue]

redText :: [SGR]
redText = [SetColor Foreground Vivid Red]

yellowText :: [SGR]
yellowText = [SetColor Foreground Dull Yellow]

withSGR :: (MonadResourceBase m) => [SGR] -> IO a -> ResourceT m a
withSGR sgr action = do
  (releaseKey, _) <- allocate (setSGR sgr) (\_ -> setSGR [Reset])
  result <- liftIO action
  release releaseKey
  return result

promptUser :: MonadIO m => String -> m (Maybe String)
promptUser msg = do
  answer <- liftIO $ do
    putStr msg
    hFlush stdout
    getLine
  return $ if answer == "" then Nothing else Just answer

promptUserYN :: MonadIO m => Bool -> String -> m Bool
promptUserYN def msg = do
  answer <- promptUser $ msg ++ defStr
  return $ handleAnswer answer
  where defStr = " [" ++ (if def then "Yn" else "yN") ++ "] "
        
        parseAnswer s =
          let s' = map toLower s
          in (s' == "y") || (s' == "yes")
    
        handleAnswer Nothing  = def
        handleAnswer (Just s) = parseAnswer s

promptUserToContinue :: MonadIO m => String -> m () -> m ()
promptUserToContinue msg cont = do
  answer <- promptUserYN False msg
  if answer then cont
  else return ()

putWarn :: MonadIO m => m ()
putWarn = liftIO . runResourceT $ withSGR yellowText $ putStr " WARN"

printWarn :: MonadIO m => String -> m ()
printWarn msg = liftIO $ do
  putWarn
  putStrLn $ ' ' : msg

putError :: MonadIO m => m ()
putError = liftIO . runResourceT $ withSGR redText $ putStr "ERROR"

printError :: MonadIO m => String -> m ()
printError msg = liftIO $ do
  putError
  putStrLn $ ' ' : msg