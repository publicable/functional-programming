module Main where

import DataProvider
import System.Environment (getArgs, withArgs)
import System.Console.CmdArgs
import Control.Monad (when)
import System.Exit
import Data.Maybe
import Data.List

data VSOptions = StationMode   { stationName :: String, time :: String }
  deriving (Data, Typeable, Show, Eq)
 
stationMode :: VSOptions
stationMode = StationMode
    { stationName = "STATION_NAME" &= help "partial or full station name", 
      time = "HOURS:MINUTES" &= help "the time you need your schedule for (24h format)" }
    &= details  [ "Examples:", "vasttrafik -s Brunnsparken -t 18:52" ]
 
vsModes :: Mode (CmdArgs VSOptions)
vsModes = cmdArgsMode $ modes [stationMode]
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME
 
_PROGRAM_NAME = "vasttrafik"
_PROGRAM_VERSION = "0.2"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "a simple tool to check Vasttrafik departure boards"

main :: IO ()
main = do
    args <- getArgs
    opts <- (if null args then withArgs ["--help"] else id) $ cmdArgsRun vsModes
    optionHandler opts
 
optionHandler :: VSOptions -> IO ()
optionHandler opts@StationMode{..}  = do
    when (null stationName) $ putStrLn "warning: -s is blank"
    display stationName time

display :: String -> String -> IO ()
display sname dtime = do
  s  <- searchForStation sname
  ds <- getDepartures (fromJust s) dtime
  sequence_ ((print $ fromJust s):(fmap (print . fromJust) (sort ds)))
