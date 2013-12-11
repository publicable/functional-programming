module Main where

import DataProvider
import System.Environment
import System.Exit
import Data.Maybe
import Prelude hiding (print, putStrLn)
import System.IO.UTF8

main :: IO()

main = getArgs >>= parseArg

parseArg [] = usage >> success
parseArg ["-h"] = usage >> success
parseArg ["-v"] = version >> success
parseArg ("-s":station) = display (return (unwords station)) >> success

usage = putStrLn "Usage: vasttrafik -s [station name]"
version = putStrLn "Haskell vasttrafik 0.2"
success = exitWith ExitSuccess
failure = exitWith (ExitFailure 1)

display :: IO String -> IO ()
display sname = do
  s <- searchForStation sname
  ds <- getDepartures $ fromJust s
  sequence_ ((print $ fromJust s):(fmap (print . fromJust) ds))
