import Data.Matrix
import Data.Maybe
import Data.List
import Data.Char
import Control.Monad
import qualified Data.Vector as Vec

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

solvePuzzle :: Puzzle -> IO (Maybe Puzzle)
solvePuzzle p = do
            putStrLn $ showPuzzle p
            if isFull p
              then return $ Just p
              else
                sequenceUntilJust $ map solvePuzzle $ genSinglePermutations p

isFull :: Puzzle -> Bool
isFull =
  all isJust . toList

-- Special case of sequenceUntilJust to collapse the Maybe (Maybe a)
sequenceUntilJust :: Monad m => [m (Maybe b)] -> m (Maybe b)
sequenceUntilJust xs = join <$> (sequenceUntil isJust xs)

sequenceUntil :: Monad m => (a -> Bool) -> [m a] -> m (Maybe a)
sequenceUntil _ [] = return Nothing
sequenceUntil test (x:xs) = do
                            thisResult <- x
                            if test thisResult
                            then return (Just thisResult)
                            else sequenceUntil test xs

genSinglePermutations p =
  catMaybes attempts
  where
    editLocation = findFirstEmpty p
    attempts = map (tryToFillCell editLocation p) [1..9]

tryToFillCell :: (Int, Int) -> Puzzle -> Int -> Maybe Puzzle
tryToFillCell (x, y) p newVal =
  let
    isValid =
      checkRow newVal (getRow x p) &&
      checkRow newVal (getCol y p) &&
      boxIsValid (getBox p x y) newVal
  in
    if isValid
    then Just (setElem (Just newVal) (x, y) p)
    else Nothing

getBox :: Puzzle -> Int -> Int -> Matrix (Maybe Int)
getBox p x y = submatrix startRow endRow startCol endCol p 
  where
    startRow = (((x-1) `div` 3)*3) + 1
    startCol = (((y-1) `div` 3)*3) + 1
    endRow = startRow + 2
    endCol = startCol + 2

boxIsValid box val = 
  not $ any (== Just val) (toList box)

checkRow :: Int -> Vec.Vector (Maybe Int) -> Bool
checkRow val row = not $ any (== Just val) (Vec.toList row)

findFirstEmpty :: Puzzle -> (Int, Int)
findFirstEmpty p = fst $ head $ filter (isNothing . snd) $ map (getElemByPair p) listPairs

getElemByPair :: Puzzle -> (Int, Int) -> ((Int, Int), Maybe Int)
getElemByPair p (x, y) = ((x, y), getElem x y p)

listPairs :: [(Int, Int)]
listPairs = [(x, y) | x <- [1..9], y <- [1..9] ]

-- main = do
--   p <- readPuzzle
--   putStrLn $ showPuzzle p
main = solvePuzzle puzzle
