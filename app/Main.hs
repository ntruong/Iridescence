module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..), exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr)
import Iridescence

main :: IO ()
main = do
  args <- getArgs
  case args of
    []       -> usage >> failure
    "-h":_ -> usage >> exitSuccess
    "-l":(file:_) -> app file True  >> exitSuccess
    "-d":(file:_) -> app file False >> exitSuccess
    (file:_) -> app file False >> exitSuccess
    where
      usage = do
        name <- getProgName
        putStrLn $ "usage: " ++ name ++ " [image]"
      failure = exitWith (ExitFailure 1)
