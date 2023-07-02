module SudokuGenerator where

import System.Random
import Data.Char (isSpace)
import System.Random (randomRIO)

-- Type definitions
type Row = [Int]
type Grid = [Row]

-- Generates a random number between min and max (inclusive)
getRandomNumber :: Int -> Int -> IO Int
getRandomNumber min max = randomRIO (min, max)

-- Shuffles a list randomly using the Fisher-Yates algorithm
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
  i <- getRandomNumber 0 (length xs - 1)
  let (left, (a:right)) = splitAt i xs
  fmap (a:) (shuffle (left ++ right))

-- Creates an empty 9x9 grid
emptyGrid :: Grid
emptyGrid = replicate 9 (replicate 9 0)

-- Checks if a given number is valid in the grid at the specified position
isValid :: Grid -> Int -> (Int, Int) -> Bool
isValid grid num (row, col) =
  notElem num (getRow row) &&
  notElem num (getColumn col) &&
  notElem num (getSquare row col)
  where
    getRow r = grid !! r
    getColumn c = map (!! c) grid
    getSquare row col =
      let r = 3 * (row `div` 3)
          c = 3 * (col `div` 3)
      in [grid !! i !! j | i <- [r..r+2], j <- [c..c+2]]

-- Solves the Sudoku game using backtracking
solveSudoku :: Grid -> Maybe Grid
solveSudoku grid = solve (0, 0) grid
  where
    solve :: (Int, Int) -> Grid -> Maybe Grid
    solve (row, 9) grid = solve (row + 1, 0) grid
    solve (9, _) grid = Just grid
    solve (row, col) grid
      | grid !! row !! col /= 0 = solve (row, col + 1) grid
      | otherwise = tryNumbers [1..9]
      where
        tryNumbers [] = Nothing
        tryNumbers (num:rest)
          | isValid grid num (row, col) =
            case solve (row, col + 1) (updateGrid num (row, col) grid) of
              Just solution -> Just solution
              Nothing -> tryNumbers rest
          | otherwise = tryNumbers rest

-- Updates the grid with a number at the specified position
updateGrid :: Int -> (Int, Int) -> Grid -> Grid
updateGrid num (row, col) grid =
  take row grid ++
  [take col (grid !! row) ++ [num] ++ drop (col + 1) (grid !! row)] ++
  drop (row + 1) grid

-- Generates a solvable 9x9 Sudoku game
generateSudoku :: IO (Maybe Grid)
generateSudoku = do
  shuffledNums <- shuffle [1..9]
  let grid = emptyGrid
  return (solveSudoku (updateGrid (head shuffledNums) (0, 0) grid))


-- Helper function to print the Sudoku grid
printGrid :: Grid -> IO ()
printGrid = mapM_ printRow
  where
    printRow :: Row -> IO ()
    printRow = putStrLn . unwords . map show

-- Converts the Sudoku grid to a string representation
gridToString :: Maybe Grid -> String
gridToString Nothing = "No solution found."
gridToString (Just grid) = unlines $ map (unwords . map show) grid

-- Converts a multi-line string to a single-line string
flattenString :: String -> String
flattenString = filter (/= '\n')

-- Removes all white spaces from a string
removeSpaces :: String -> String
removeSpaces = filter (not . isSpace)


-- Removes random characters from a string and replaces them with dots
removeRandomChars :: String -> IO String
removeRandomChars str = do
  indices <- getRandomIndices (length str) (length str `div` 3)  -- Adjust the division factor as per your requirement
  let replacedStr = foldr (\i s -> replaceChar i '.' s) str indices
  return replacedStr

-- Generates random distinct indices within a given range
getRandomIndices :: Int -> Int -> IO [Int]
getRandomIndices _ 0 = return []
getRandomIndices range n = do
  index <- randomRIO (0, range - 1)
  rest <- getRandomIndices range (n - 1)
  return (index : rest)

-- Replaces a character at the specified index with a new character
replaceChar :: Int -> Char -> String -> String
replaceChar index newChar str =
  take index str ++ [newChar] ++ drop (index + 1) str

replaceCharAtIndex :: Int -> Char -> String -> String
replaceCharAtIndex index newChar str
  | index < 0 || index >= length str = str -- Return the original string if the index is out of bounds
  | otherwise = take index str ++ [newChar] ++ drop (index + 1) str