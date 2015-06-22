module PuzzleIO where

import Data.Char
import Data.Matrix

type Puzzle = Matrix (Maybe Int)

showPuzzle = unlines . (map showRow) . toLists

showRow = unwords . map showNumber

showNumber :: Maybe Int -> String
showNumber (Just n) = show n
showNumber Nothing = "-"

readPuzzle :: IO Puzzle
readPuzzle = do
  lines <- sequence $ take 9 $ repeat getLine
  return $ puzzleFromLines lines

puzzleFromLines lines =
  let chars = filter (not . isSpace) $ unwords lines
  in
    fromList 9 9 $ map toCell chars

toCell :: Char -> Maybe Int
toCell n = if isHexDigit n then Just (digitToInt n) else Nothing

