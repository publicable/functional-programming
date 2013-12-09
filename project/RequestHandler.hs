module RequestHandler where

import Network.URI
import Network.HTTP

getJson:: String -> IO String
getJson uri = makeRequest uri

makeRequest:: String -> IO String
makeRequest uri = do
                  response <- simpleHTTP (getRequest uri)
                  body <- getResponseBody response
                  return body

