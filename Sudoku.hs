module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List
import Data.Maybe

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
isSudoku sud = length (rows sud) == 9 && (and lengthRows)
  where lengthRows = [ length row == 9 | row <- rows sud]

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sud = not (and [Nothing `elem` row | row <- rows sud])

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku = mapM_ printRow . rows

printRow :: [Maybe Int] -> IO()
printRow [] = putStrLn ""
printRow (x:xs) = do
  putStr $ maybe "." show x
  printRow xs

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku path = do
  fileContents <- readFile path
  let fileLines = lines fileContents
  -- implement check for invalid sudoku file with isSudoku
  let sudoku = Sudoku [readLine line | line <- fileLines]
  if isSudoku sudoku then
    return sudoku
  else
    error "Invalid sudoku"
  
readLine :: String -> [Maybe Int]
readLine line = 
  [ if x == '.' then Nothing else Just $ digitToInt x | x <- line ]

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(9, return Nothing), (1, rNumCell)]
  where rNumCell = elements $ map Just [1..9]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows' <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows')

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

type Block = [Maybe Int]

isOkayBlock :: Block -> Bool
isOkayBlock block = (length $ onlyJust block) == (length (nub $ onlyJust block))
  where onlyJust b = [ x | x <- b, isJust x]

blocks :: Sudoku -> [Block]
blocks sud = do
  let rb = rows sud
  let cb = transpose rb
  let tb = threeByThreeBlocks rb
  rb ++ cb ++ tb

threeByThreeBlocks :: [[Maybe Int]] -> [Block]
threeByThreeBlocks rs = [take 9 $ drop n ungroupped | n <- [0,9..72]]
  where ungroupped = concat [take 3 $ drop n (rs !! i) | n <- [0,3,6], i <- [0..8]]

prop_nineBlocks :: Sudoku -> Bool
prop_nineBlocks sud = (length $ blocks sud) == 27 && (and [length b == 9 | b <- blocks sud])

isOkay :: Sudoku -> Bool
isOkay sud = and [isOkayBlock b | b <- blocks sud]

type Pos = (Int,Int)

blanks :: Sudoku -> [Pos]
blanks sud = [(x,y) | x <- [0..8], y <- [0..8], isNothing ((rows sud !! x) !! y)]

prop_blanks :: Sudoku -> Bool
prop_blanks s = and [isNothing $ ((rows s) !! y) !! x | (y,x) <- blanks s]

(!!=) :: [a] -> (Int,a) -> [a]
(!!=) a (idx, r) = map f (zip a [0..])
  where f (n, i) | i == idx = r
                 | otherwise = n

--prop_findReplace :: [a] -> (Int,a) -> Bool
--prop_findReplace a (idx,v) = 

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sud (y,x) v = Sudoku ((rows sud) !!= (y, ((rows sud) !! y) !!= (x,v)))

candidates :: Sudoku -> Pos -> [Int]
candidates sud (y,x) = intersect (intersect inRow inColumn) inThreeByThree
  where inRow = valid (rows sud !! y)
        inColumn = valid ((transpose $ rows sud) !! x)
        inThreeByThree = valid $ (threeByThreeBlocks $ rows sud) !! quadrant
        quadrant = rowVal + colVal
        rowVal = ((y+3) `quot` 3) - 1
        colVal = (x `quot` 3) * 3

--prop_checkCandidates :: Sudoku -> Pos -> Bool
--prop_checkCandidates sud (y,x) = 

valid :: Block -> [Int]
valid block = map fromJust ((map Just [1..9]) \\ block)


solve :: Sudoku -> Maybe Sudoku
solve sud = case isSudoku sud && isOkay sud of 
              False -> Nothing
              True -> solve' sud

solve' :: Sudoku -> Maybe Sudoku
solve' sud = case null (blanks sud) of 
              True -> Just sud
              False -> do
                let pos = head $ blanks sud
                let cnd = candidates sud pos
                case null cnd of 
                  True -> Just sud
                  False -> solve' (update sud pos $ Just $ head cnd)


-- Use the property with quickCheck
main :: IO()
main = do
  quickCheck prop_Sudoku
  quickCheck prop_nineBlocks
