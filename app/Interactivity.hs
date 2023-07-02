module Interactive where

import System.Random

replaceCharAtIndex :: Int -> Char -> String -> String
replaceCharAtIndex index newChar str
  | index < 0 || index >= length str = str -- Return the original string if the index is out of bounds
  | otherwise = take index str ++ [newChar] ++ drop (index + 1) str