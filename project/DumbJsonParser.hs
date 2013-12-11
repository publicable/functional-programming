module DumbJsonParser where

import Data.Maybe
import Data.Char
import Data.List.Split
import Data.List

strip' :: String -> String
strip' s = dropWhile isSpace [s' | s' <- s, conditions s']
  where conditions x | isAlphaNum x = True
                     | x == ' ' = True
                     | otherwise = False

removeUntil :: String -> String -> String
removeUntil pr (x:xs) | pr `isPrefixOf` (x:xs) = drop (length pr) (x:xs)
                      | otherwise = removeUntil pr xs

getField :: String -> String -> [(String, String)]
getField f json = [(f, strip' $ removeUntil fieldName x) | 
      x <- splitOn "," json, fieldName `isInfixOf` x]
    where fieldName = "\"" ++ f ++ "\""

getFields :: [String] -> String -> [[(String, String)]]
getFields fs json = transpose [getField f json | f <- fs]