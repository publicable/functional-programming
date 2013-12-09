module DataProvider where

import RequestHandler
import Config
import Data.Aeson

type ID =  String
type Name = String
data Station = Station ID Name
  deriving Show

type Line = String
type Direction = String
type Time = String
data Departure = Departure Line Direction Time

searchForStation :: String -> Maybe Station
-- searchForStation station = getJson $ searchUrl station 
searchForStation = undefined

getDepartures :: Station -> Maybe [Departure]
getDepartures = undefined

instance Show Departure where
  show = undefined
