module DataProvider where


type ID =  String
type Name = String
data Station = Station ID Name

type Line = String
type Direction = String
type Time = String
data Departure = Departure Line Direction Time

searchForStation :: IO String -> Maybe Station
searchForStation = undefined

getDepartures :: Station -> Maybe [Departure]
getDepartures = undefined

instance Show Departure where
  show = undefined
