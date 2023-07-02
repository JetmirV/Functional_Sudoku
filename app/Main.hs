import System.Random
import Data.Set (Set, unions, fromList, member)
import Data.Map (Map, singleton, elems, (!), insert)
import Debug.Trace (trace)
import Data.List.Split
import SudokuGenerator
import Solving
import Data.Text (pack, unpack, strip)
import Data.Char (isSpace)
import Control.Arrow (Arrow(first, second))

main :: IO ()
main = do
    maybeSudoku <- generateSudoku
    let sudokuString = gridToString maybeSudoku
    let flattened = flattenString sudokuString
    let whithoutSpaces = removeSpaces flattened
    modifiedString <- removeRandomChars whithoutSpaces   
    let gridRead = readGrid modifiedString
    putStrLn "Your sudoku:"
    let maybeGrid = gridRead
    case maybeGrid of
        Just g -> putStrLn (showGrid g)
        Nothing -> putStrLn "Failed to get the grid"

    playedStrign <- playLoop modifiedString
    let gridRead = readGrid playedStrign

    case maybeGrid of
        Just g -> print (isGridInvalid g)
        Nothing -> putStrLn "Failed to show the boolean value"

    let solved = case maybeGrid of
            Just a -> solve a
    case solved of
        Just g -> putStrLn (showGrid g)
        Nothing -> putStrLn "No solution found"


playLoop :: String -> IO String
playLoop acc = do
  putStrLn "In order to play please write (Column) (Row) (the yumber you want to insert), note that they must be seperated by spaces, to quit write \"quit\":"
  input <- getLine
  if input == "quit"
    then do 
        putStrLn "Exiting game, and displaying the solved sudoku."
        return acc
  else do
    -- Perform the insert logic here
    let (first, second, third) = splitAndConvert input
    let position = first * second
    let modifiedString = replaceCharAtIndex position third acc        
    let gridRead = readGrid modifiedString
    putStrLn "Your sudoku:"
    let maybeGrid = gridRead
    case maybeGrid of
        Just g -> putStrLn (showGrid g)
        Nothing -> putStrLn "Failed to get the grid"
    playLoop modifiedString -- Recursively call the insertLoop function

splitAndConvert :: String -> (Int, Int, Char)
splitAndConvert str =
  case words str of
    [first, second, third] ->
      (read first :: Int, read second :: Int, head third)
    _ ->
      error "Invalid input format"




    

    

