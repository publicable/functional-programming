module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List
import Data.Maybe

import System.Process

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
isSudoku sud = length (rows sud) == 9 && and lengthRows
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
isOkayBlock block = length (onlyJust block) == length (nub $ onlyJust block)
  where onlyJust b = [ x | x <- b, isJust x]

blocks :: Sudoku -> [Block]
blocks sud = let
  rb = rows sud
  cb = transpose rb
  tb = threeByThreeBlocks rb
  in rb ++ cb ++ tb

threeByThreeBlocks :: [[Maybe Int]] -> [Block]
threeByThreeBlocks rs = [take 9 $ drop n ungroupped | n <- [0,9..72]]
  where ungroupped = concat [take 3 $ drop n (rs !! i) | n <- [0,3,6], i <- [0..8]]

prop_nineBlocks :: Sudoku -> Bool
prop_nineBlocks sud = length (blocks sud) == 27 && and [length b == 9 | b <- blocks sud]

isOkay :: Sudoku -> Bool
isOkay sud = and [isOkayBlock b | b <- blocks sud]

type Pos = (Int,Int)

blanks :: Sudoku -> [Pos]
blanks sud = [(x,y) | x <- [0..8], y <- [0..8], isNothing ((rows sud !! x) !! y)]

prop_blanks :: Sudoku -> Bool
prop_blanks s = and [isNothing $ (rows s !! y) !! x | (y,x) <- blanks s]

(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] _       = []
(!!=) a (idx, r) = zipWith (curry f) a [0..]
  where f (n, i) | i == idx = r
                 | otherwise = n

prop_findReplace :: [Int] -> (Int,Int) -> Property
prop_findReplace a (idx,v) = not (null a) ==> 
    a !!= (idx',v) !! idx' == v
  where
    idx' = idx `mod` (length a)

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sud (y,x) v =
  if x >= 0 && x < 9 && y >= 0 && y < 9 then
    Sudoku (rows sud !!= (y, (rows sud !! y) !!= (x,v)))
  else
    sud

prop_checkUpdate :: Sudoku -> Pos -> Maybe Int -> Bool
prop_checkUpdate sud (y,x) v = 
  let 
    s = update sud (y',x') v
    y' = abs $ y `mod` 9
    x' = abs $ x `mod` 9
  in
    (rows s !! y') !! x' == v
      

candidates :: Sudoku -> Pos -> [Int]
candidates sud (y,x) = inRow `intersect` inColumn `intersect` inThreeByThree
  where inRow = candidates' (rows sud !! y)
        inColumn = candidates' (transpose (rows sud) !! x)
        inThreeByThree = candidates' $ threeByThreeBlocks (rows sud) !! quadrant
        quadrant = rowVal + colVal
        rowVal = ((y+3) `quot` 3) - 1
        colVal = (x `quot` 3) * 3

candidates' :: Block -> [Int]
candidates' block = [1..9] \\ catMaybes block

prop_checkCandidates :: Sudoku -> Pos -> Property
prop_checkCandidates sud (y,x) = isOkay sud ==> and checkAll
  where
    y'            = y `mod` 9
    x'            = x `mod` 9
    newSud        = update sud (y',x')
    cnds          = candidates sud (y',x')
    allCandidates = [newSud $ Just v | v <- cnds]
    checkAll      = map isSudoku allCandidates ++ map isOkay allCandidates

solve :: Sudoku -> Maybe Sudoku
solve sud | not (isSudoku sud) || not (isOkay sud) = Nothing
          | otherwise                              = solve' sud cnds
          where
            pos       = head $ blanks sud
            cnds      = candidates sud pos

solve' :: Sudoku -> [Int] -> Maybe Sudoku
solve' sud cnds | null cnds = Nothing
                | null $ blanks new_sud = Just new_sud
                | isNothing solution = solve' sud $ tail cnds
                | otherwise = solution
  where
    new_sud = update sud pos (Just cnd) 
    pos = head $ blanks sud
    cnd = head cnds
    new_pos = head $ blanks new_sud 
    new_cnds = candidates new_sud new_pos
    solution = solve' new_sud new_cnds

readAndSolve :: FilePath -> IO ()
readAndSolve path = do
  sud <- readSudoku path
  let solution = solve sud
  maybe (putStrLn "(no solution)") printSudoku solution

isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sol sud | not $ isSudoku sol = False
                     | not $ isOkay sol   = False
                     | not $ isSolved sol = False
                     | otherwise = and [sol `atCoord` p == sud `atCoord` p | p <- filled sud]

atCoord :: Sudoku -> Pos -> Maybe Int
atCoord sud (y,x) = (rows sud !! y) !! x

filled :: Sudoku -> [Pos]
filled sud = [(x,y) | x <- [0..8], y <- [0..8], isJust ((rows sud !! x) !! y)]

prop_SolveSound :: Sudoku -> Property
prop_SolveSound sud = isOkay sud 
  ==> fromJust (solve sud) `isSolutionOf` sud

-- Use the property with quickCheck
main :: IO()
main = do
  quickCheck prop_Sudoku
  quickCheck prop_nineBlocks
  quickCheck prop_blanks
  quickCheck prop_checkCandidates
  quickCheck prop_nineBlocks
  quickCheck prop_checkUpdate
  quickCheck prop_SolveSound
