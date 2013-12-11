module DataProvider where

import RequestHandler
import Config
import DumbJsonParser
import Data.Maybe

data Station = Station {sid::String, sname::String}

statInit :: [(String,String)] -> Maybe Station
statInit s | isNothing i = Nothing
           | isNothing n = Nothing
           | otherwise   = Just (Station (fromJust i) (fromJust n))
  where i = lookup "id" s
        n = lookup "name" s

data Departure = Departure {dname::String, 
                  direction::String, time::String}

depInit :: [(String,String)] -> Maybe Departure
depInit d | isNothing n  = Nothing
          | isNothing t  = Nothing
          | isNothing dr = Nothing
          | otherwise    = Just (Departure (fromJust n) (fromJust dr)
              (fmt $ fromJust t))
  where n     = lookup "sname" d
        t     = lookup "time" d
        dr    = lookup "direction" d
        fmt t = (take 2 t) ++ ":" ++ (drop 2 t)

searchForStation :: IO String -> IO (Maybe Station)
searchForStation name = do
      station <- name 
      json <- getJson (searchUrl station)
      return (statInit ((getFields ["id","name"] json) !! 0))

getDepartures :: Station -> IO ([Maybe Departure])
getDepartures s = do
  json <- getJson $ departuresUrl (sid s)
  let allDeps = (getFields ["sname","time","direction"] json)
  return (fmap depInit allDeps)

instance Show Departure where
  show d = (dname d) ++ " | " ++ (direction d) 
    ++ "\t" ++  (time d)

instance Show Station where
  show = sname
