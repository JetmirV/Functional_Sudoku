import System.Random
import Data.Set (Set, unions, fromList, member)
import Data.Map (Map, singleton, elems, (!), insert)
import Debug.Trace (trace)
import Data.List.Split
import Solving
import Generator
import Data.Text (pack, unpack, strip)
import Data.Char (isSpace)
--import Test
import Test1

main :: IO ()
main = do
    maybeSudoku <- generateSudoku
    case maybeSudoku of
        Just sudoku -> printGrid sudoku
        Nothing -> putStrLn "Failed to generate a Sudoku grid"
    -- sudoku <- generateSudoku
    -- let modifiedSudoku = map (map (\num -> if num `elem` [1, 3, 5, 7] then 0 else num)) sudoku
    -- putStrLn "another form"
    -- displaySudoku modifiedSudoku
    -- putStrLn "another form"
    -- modifiedMatrix <- removeRandomNumbers modifiedSudoku

    -- putStrLn "This is your sudoku:"
    -- rng <- newStdGen
    -- layout <- return (createRandomLayout rng 0.1)
    -- let sudokuGame = stringifyLayout layout
   
    -- let trimmedInput = unpack (strip (pack modifiedMatrix))
    -- let linesWithoutQuotes = map (tail . init) (lines trimmedInput)
    -- let output = concatMap  (\c -> if c == '0' then "." else [c]) (concat linesWithoutQuotes)
    -- let trimmedOutput = reverse (dropWhile isSpace (reverse (filter (not . isSpace) output)))
    
    -- hello <- return (readGrid trimmedOutput)
    -- let maybeGrid = hello
    -- case maybeGrid of
    --     Just g -> putStrLn (showGrid g)
    --     Nothing -> putStrLn "Failed to get the grid"
    -- putStrLn "Is the current grid valid: "
    -- case maybeGrid of
    --     Just g -> print (isGridInvalid g)
    --     Nothing -> putStrLn "Failed to show the boolean value"

    -- let solved = case maybeGrid of
    --         Just a -> solve a
    -- case solved of
    --     Just g -> putStrLn (showGrid g)
    --     Nothing -> putStrLn "No solution found"

    

