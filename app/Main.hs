import System.Random
import Data.Set (Set, unions, fromList, member)
import Data.Map (Map, singleton, elems, (!), insert)
import Debug.Trace (trace)
import Data.List.Split
import SudokuGenerator
import Solving
import Data.Text (pack, unpack, strip)
import Data.Char (isSpace)

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
    putStrLn "Is the current grid valid: "
    case maybeGrid of
        Just g -> print (isGridInvalid g)
        Nothing -> putStrLn "Failed to show the boolean value"

    let solved = case maybeGrid of
            Just a -> solve a
    case solved of
        Just g -> putStrLn (showGrid g)
        Nothing -> putStrLn "No solution found"

    

