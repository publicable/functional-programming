module DataProvider where

import Network.URI
import Network.HTTP
import Network.HTTP.Simple

getJson:: String -> Json
getJson uri = case parseURI uri of
  Nothing -> error "Invalid URI"
  Just uri -> do 
    makeRequest uri

makeRequest:: URI -> String
makeRequest = undefined
