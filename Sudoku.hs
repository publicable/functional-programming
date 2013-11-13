module Sudoku where

import Test.QuickCheck
import Data.Maybe
import Data.Char

-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 row)
  where row = replicate 9 Nothing

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku sud | length (rows sud) /= 9 = False
             | (and lengthRows) == False = False
             | otherwise = True
  where lengthRows = [ (length row) == 9 | row <- rows sud]

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sud | and rowsSummary == True = False
             | otherwise = True
  where rowsSummary = [elem Nothing row | row <- rows sud]

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sud = sequence_
  [printRow row | row <- rows sud]

printRow :: [Maybe Int] -> IO()
printRow [] = do putStrLn ""
printRow (x:xs) = do
  putStr $ if (isNothing x) then "." else show $ fromJust x
  printRow xs

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku path = do
  fileContents <- readFile path
  let fileLines = lines fileContents
  -- implement check for invalid sudoku file with isSudoku
  return (Sudoku [readLine line | line <- fileLines])
  
readLine :: String -> [Maybe Int]
readLine line = 
  [ if (x == '.') then Nothing else (Just (ord x)) | x <- line ]

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = undefined

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-------------------------------------------------------------------------