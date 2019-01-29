module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Iridescence

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      name <- getProgName
      hPutStrLn stderr $ "usage: " ++ name ++ " [image]"
      exitFailure
    (file:_) -> app file
