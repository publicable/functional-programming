module DataProvider where

import RequestHandler
import Config
import DumbJsonParser
import Data.Maybe

data Station = Station {sid::String, sName::String}

data Departure = Departure {dName::String, 
                  direction::String, dTime::String}

instance Show Departure where
  show d = dName d ++ " | " ++ direction d 
    ++ "\t" ++  dTime d

instance Show Station where
  show = sName

instance Eq Departure where
  (Departure n1 d1 _) == (Departure n2 d2 _) = n1 == n2 && d1 == d2

instance Ord Departure where
     (Departure n1 d1 _) `compare` (Departure n2 d2 _) = n1 `compare` n2



statInit :: [(String,String)] -> Maybe Station
statInit s | isNothing i = Nothing
           | isNothing n = Nothing
           | otherwise   = Just (Station (fromJust i) (fromJust n))
  where i = lookup "id" s
        n = lookup "name" s

depInit :: [(String,String)] -> Maybe Departure
depInit d | isNothing n  = Nothing
          | isNothing t  = Nothing
          | isNothing dr = Nothing
          | otherwise    = Just (Departure (fromJust n) (fromJust dr)
              (fmt $ fromJust t))
  where n     = lookup "sname" d
        t     = lookup "time" d
        dr    = lookup "direction" d
        fmt t = take 2 t ++ ":" ++ drop 2 t

searchForStation :: String -> IO (Maybe Station)
searchForStation name = do
      json <- getJson (searchUrl name)
      return (statInit (head (getFields ["id","name"] json)))

getDepartures :: Station -> Maybe String -> IO [Maybe Departure]
getDepartures s Nothing = do
  json <- getJson $ departuresUrl [("id", sid s)]
  extractDepartures json
getDepartures s qt = do
  json <- getJson $ departuresUrl [("id", sid s), ("time", fromJust qt)]
  extractDepartures json
  
extractDepartures :: String -> IO [Maybe Departure]
extractDepartures json = do
  let allDeps = getFields ["sname","time","direction"] json
  return (fmap depInit allDeps)

