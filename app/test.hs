module Test where

import System.Random

-- Split a list into chunks of a given size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Generate a random Sudoku grid
generateSudoku :: IO [[Int]]
generateSudoku = do
  gen <- getStdGen
  let randomGrid = take 9 $ chunksOf 9 $ randomRs (1, 9) gen
      initialGrid = map (map (\num -> if num `elem` [1, 3, 5, 7] then 0 else num)) randomGrid
  case solveSudoku initialGrid of
    Just solution -> return solution
    Nothing -> generateSudoku

-- Check if a value is safe to be placed at a given position in the Sudoku grid
isSafe :: [[Int]] -> Int -> (Int, Int) -> Bool
isSafe grid num (row, col) = notElem num (getRowValues grid row)
                            && notElem num (getColValues grid col)
                            && notElem num (getBoxValues grid (row, col))
  where
    getRowValues g r = g !! r
    getColValues g c = map (!! c) g
    getBoxValues g (r, c) = [g !! i !! j | i <- [3 * (r `div` 3) .. 3 * (r `div` 3) + 2]
                                         , j <- [3 * (c `div` 3) .. 3 * (c `div` 3) + 2]]

-- Solve the Sudoku puzzle using backtracking
solveSudoku :: [[Int]] -> Maybe [[Int]]
solveSudoku grid = solve (0, 0) grid
  where
    solve :: (Int, Int) -> [[Int]] -> Maybe [[Int]]
    solve (row, 9) grid = solve (row + 1, 0) grid
    solve (9, _) grid = Just grid
    solve (row, col) grid
      | grid !! row !! col /= 0 = solve (row, col + 1) grid
      | otherwise = tryValues (row, col) grid [1..9]
        where
          tryValues _ _ [] = Nothing
          tryValues (r, c) g (v:vs)
            | isSafe g v (r, c) = case solve (r, c + 1) (replace r c v g) of
                                    Just solution -> Just solution
                                    Nothing -> tryValues (r, c) g vs
            | otherwise = tryValues (r, c) g vs
          replace r c v g = take r g ++ [take c (g !! r) ++ [v] ++ drop (c + 1) (g !! r)] ++ drop (r + 1) g


-- Remove random numbers from the matrix
removeRandomNumbers :: [[Int]] -> IO String
removeRandomNumbers matrix = do
  gen <- getStdGen
  let randomIndexes = take (length matrix) $ chunksOf (length matrix) $ randomRs (0, length matrix - 1) gen
      modifiedMatrix = zipWith (\row indexes -> removeRowNumbers row indexes) matrix randomIndexes
      gridString = concatMap (concatMap formatCell) modifiedMatrix
  return $ "\"" ++ gridString ++ "\""

-- Remove random numbers from a row based on the provided indexes
removeRowNumbers :: [Int] -> [Int] -> [Int]
removeRowNumbers row indexes = zipWith (\num idx -> if idx `elem` indexes then 0 else num) row [0..]

-- Format a cell value for display
formatCell :: Int -> String
formatCell num
  | num == 0 = "."
  | otherwise = show num

-- Display the Sudoku grid with dots instead of random numbers
displaySudoku :: [[Int]] -> IO ()
displaySudoku grid = mapM_ putStrLn (map (unwords . map formatCell) grid)
  where
    formatCell :: Int -> String
    formatCell num
      | num == 0 = "."
      | otherwise = show num