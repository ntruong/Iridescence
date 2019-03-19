module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)
import Iridescence

main :: IO ()
main = do
  args <- getArgs
  case args of
    []       -> usage >> failure
    ("-h"):_ -> usage >> success
    ("-l"):(file:_) -> app file True  >> success
    ("-d"):(file:_) -> app file False >> success
    (file:_) -> app file False >> success
    where
      usage = do
        name <- getProgName
        putStrLn $ "usage: " ++ name ++ " [image]"
      success = exitWith ExitSuccess
      failure = exitWith (ExitFailure 1)
