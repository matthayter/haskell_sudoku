import Data.Matrix
import Data.Maybe
import Data.List
import Control.Monad
import qualified Data.Vector

type Puzzle = Matrix (Maybe Int)

puzzle = fromList 9 9 [Just 7, Just 5, Nothing, Just 8, Just 9, Just 1, Nothing, Nothing, Nothing,
                    Nothing, Nothing, Just 1, Just 6, Nothing, Nothing, Just 9, Just 8, Nothing,
                    Nothing, Just 9, Nothing, Nothing, Nothing, Nothing, Just 7, Nothing, Nothing,
                    Just 1, Just 4, Nothing, Nothing, Just 5, Nothing, Just 3, Just 7, Nothing,
                    Nothing, Just 7, Nothing, Nothing, Just 3, Nothing, Just 6, Just 1, Just 4,
                    Nothing, Nothing, Just 9, Nothing, Nothing, Just 4, Just 5, Nothing, Nothing,
                    Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just 4, Nothing,
                    Nothing, Just 6, Nothing, Just 3, Nothing, Just 2, Just 1, Nothing, Nothing,
                    Just 2, Nothing, Just 3, Nothing, Nothing, Just 9, Nothing, Just 5, Nothing]

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

main = solvePuzzle puzzle
