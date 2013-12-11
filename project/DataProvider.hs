module DataProvider where

import RequestHandler
import Config
import DumbJsonParser
import Data.Maybe

data Station = Station {id::String, name::String}
  deriving Show

data Departure = Departure {line::String, 
                  direction::String, time::String}

searchForStation :: String -> Maybe Station
searchForStation name = do 
      json <- getJson $ searchUrl name
      statInit ((getFields ["id","name"] json) !! 0)

statInit :: [(String,String)] -> Maybe Station
statInit s | isNothing i = Nothing
           | isNothing n = Nothing
           | otherwise   = Just (Station (fromJust i) (fromJust n))
  where i = lookup "id" s
        n = lookup "name" s


getDepartures :: Station -> Maybe [Departure]
getDepartures = undefined

instance Show Departure where
  show = undefined
