import System.Random
import Data.Set (Set, unions, fromList, member)
import Data.Map (Map, singleton, elems, (!), insert)
import Debug.Trace (trace)
import Data.List.Split
import Solving
import Generator

main :: IO ()
main = do
    let inputStr = ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."
    hello <- return (readGrid inputStr)
    let maybeGrid = hello
    case maybeGrid of
        Just g -> putStrLn (showGrid g)
        Nothing -> putStrLn "Failed to get the grid"
    putStrLn "Is the current grid valid: "
    case maybeGrid of
        Just g -> print (isGridInvalid g)
        Nothing -> putStrLn "Failed to get the grid"

    let solved = case maybeGrid of
            Just a -> solve a
    case solved of
        Just g -> putStrLn (showGrid g)
        Nothing -> putStrLn "Failed to get the grid"

    -- rng <- newStdGen
    -- layout <- return (createRandomLayout rng 0.1)
    -- putStr (stringifyLayout layout)

