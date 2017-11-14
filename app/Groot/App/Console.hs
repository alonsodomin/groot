module Groot.App.Console where

import Control.Monad.IO.Class
import Data.Char
import System.IO
import System.Console.ANSI

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
putWarn = liftIO $ do
  setSGR [SetColor Foreground Dull Yellow]
  putStr " WARN"
  setSGR [Reset]

printWarn :: MonadIO m => String -> m ()
printWarn msg = liftIO $ do
  putWarn
  putStrLn $ ' ' : msg

putError :: MonadIO m => m ()
putError = liftIO $ do
  setSGR [SetColor Foreground Vivid Red]
  putStr "ERROR"
  setSGR [Reset]

printError :: MonadIO m => String -> m ()
printError msg = liftIO $ do
  putError
  putStrLn $ ' ' : msg