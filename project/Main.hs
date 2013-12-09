module Main where

import DataProvider
import System.Environment
import System.Exit

main :: IO()

main = getArgs >>= parseArg

parseArg ["-h"] = usage >> success
parseArg ["-v"] = version >> success
parseArg ("-s":station) = putStrLn (unwords station) >> success

usage = putStrLn "Usage: vasttrafik -s [station name]"
version = putStrLn "Haskell vasttrafik 0.1"
success = exitWith ExitSuccess
failure = exitWith (ExitFailure 1)
