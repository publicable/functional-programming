module JsonParser where

import Data.Maybe
import Data.Char

-- Taken from http://book.realworldhaskell.org/read/writing-a-library-working-with-json-data.html
data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

type Parser a = String -> Maybe (a, String)

printJson :: JValue -> String
printJson = undefined

readJson :: String -> JValue
readJson = undefined

