module DumbJsonParser where

import Data.Maybe
import Data.Char
import Data.List.Split
import Data.List

import Test.QuickCheck

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

prop_getField :: Property
prop_getField = forAll genSafeString $ \field -> 
				 forAll genSafeString $ \value1 ->
				 forAll genSafeString $ \value2 ->
				 forAll genSafeString $ \cruft ->
				 getField field ("{"
				  ++ "\"" ++ cruft ++ "\"" ++ ":" ++ "\"" ++ cruft ++ "\"" ++ ","
				  ++ "\"" ++ field ++ "\"" ++ ":" ++ "\"" ++ value1 ++ "\"" ++ ","
				  ++ "\"" ++ cruft ++ "\"" ++ ":" ++ "\"" ++ cruft ++ "\"" ++ ","
				  ++ "\"" ++ field ++ "\"" ++ ":" ++ "\"" ++ value2 ++ "\"" ++ ","
				  ++ "\"" ++ cruft ++ "\"" ++ ":" ++ "\"" ++ cruft ++ "\"" ++
				  "}") == [(field, value1), (field, value2)]


-- module for generating safe strings from
-- http://stackoverflow.com/questions/20934506/haskell-quickcheck-how-to-generate-only-printable-strings

genSafeChar :: Gen Char
genSafeChar = elements ['a'..'z']

genSafeString :: Gen String
genSafeString = listOf1 genSafeChar