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
        

printWarn :: String -> IO ()
printWarn msg = do
  setSGR [SetColor Foreground Vivid Yellow]
  putStr "WARNING"
  setSGR [Reset]
  putStrLn $ " " ++ msg

printError :: String -> IO ()
printError msg = do
  setSGR [SetColor Foreground Vivid Red]
  putStr "ERROR"
  setSGR [Reset]
  putStrLn $ " " ++ msg