import Data.Matrix
import Data.Maybe
import Data.List
import Data.Char
import Control.Monad
import qualified Data.Vector

type Puzzle = Matrix (Maybe Int)

puzzle = puzzleFromLines
          ["7 5 - 8 9 1 - - -",
           "- - 1 6 - - 9 8 -",
           "- 9 - - - - 7 - -",
           "1 4 - - 5 - 3 7 -",
           "- 7 - - 3 - 6 1 4",
           "- - 9 - - 4 5 - -",
           "- - - - - - - 4 -",
           "- 6 - 3 - 2 1 - -",
           "2 - 3 - - 9 - 5 -"]

showPuzzle = unlines . (map showRow) . toLists

showRow = unwords . map showNumber

showNumber :: Maybe Int -> String
showNumber (Just n) = show n
showNumber Nothing = "-"

solvePuzzle :: Puzzle -> IO (Maybe Puzzle)
solvePuzzle p = do
            putStrLn $ showPuzzle p
            if isFull p
              then return $ Just p
              else
                findSolution $ map solvePuzzle $ genSinglePermutations p

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

findSolution :: [IO (Maybe Puzzle)] -> IO (Maybe Puzzle)
findSolution [] = return Nothing
findSolution (x:xs) =
  do
    thisSol <- x
    if isJust thisSol
    then return thisSol
    else findSolution xs

genSinglePermutations p =
  filter (checkValidity editLocation) $ permutations
  where
    editLocation = findFirstEmpty p
    permutations = map (setCellTo p editLocation) [1..9]

isFull :: Puzzle -> Bool
isFull =
  all isJust . toList

checkValidity :: (Int, Int) -> Puzzle -> Bool
checkValidity (x, y) p =
  checkRow newNum (getRow x p) &&
  checkRow newNum (getCol y p) &&
  boxIsValid (getBox p x y) newNum
  where newNum = fromMaybe (error "wut") $ getElem x y p

getBox :: Puzzle -> Int -> Int -> Matrix (Maybe Int)
getBox p x y = submatrix startRow endRow startCol endCol p 
  where
    startRow = (((x-1) `div` 3)*3) + 1
    startCol = (((y-1) `div` 3)*3) + 1
    endRow = startRow + 2
    endCol = startCol + 2

boxIsValid box val = 
  1 == length (filter (== Just val) (toList box))

checkRow :: Int -> Data.Vector.Vector (Maybe Int) -> Bool
checkRow val row = length (Data.Vector.filter (== (Just val)) row) == 1

findFirstEmpty :: Puzzle -> (Int, Int)
findFirstEmpty p = fst $ head $ filter (isNothing . snd) $ map (getElemByPair p) listPairs

getElemByPair :: Puzzle -> (Int, Int) -> ((Int, Int), Maybe Int)
getElemByPair p (x, y) = ((x, y), getElem x y p)

listPairs :: [(Int, Int)]
listPairs = [(x, y) | x <- [1..9], y <- [1..9] ]

setCellTo :: Puzzle -> (Int, Int) -> Int -> Puzzle
setCellTo p loc value = setElem (Just value) loc p

-- main = do
--   p <- readPuzzle
--   putStrLn $ showPuzzle p
main = solvePuzzle puzzle
