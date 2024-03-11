-- Sudoku solver using Haskell
-- Name: Nguyen Anh Le
-- StudentID: 15000370
-- BCs Informatica

import System.Environment
import Data.List

type Row = Int
type Column = Int
type Value = Int
type Grid = [[Value]]
type Sudoku = (Row, Column) -> Value
type Constraint = (Row, Column, [Value])
type Node = (Sudoku, [Constraint])

-- Define constants for Sudoku grid
positions :: [Int]
positions = [1..9]

values :: [Value]
values = [1..9]

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

centerOfBlocks :: [Int]
centerOfBlocks = [2, 5, 8]

-- Conversion functions between Sudoku and Grid
sud2grid :: Sudoku -> Grid
sud2grid s = [[s (r, c) | c <- positions] | r <- positions]

grid2sud :: Grid -> Sudoku
grid2sud gr (r, c) = gr !! (r - 1) !! (c - 1)

-- Function to extend Sudoku with a new value at a specific position
extend :: Sudoku -> (Row, Column, Value) -> Sudoku
extend sud (r, c, v) (i, j) = if (r, c) == (i, j) then v else sud (i, j)

-- Function to read Sudoku from a file
readSudoku :: String -> IO (Maybe Sudoku)
readSudoku filename = do
  content <- readFile filename
  return $ grid2sud . splitStringIntoGrid <$> Just content
  where
    splitStringIntoGrid = map (map readInt . words) . lines
    readInt x = read x :: Int

-- Function to print Sudoku grid
printSudoku :: Sudoku -> IO ()
printSudoku = putStr . unlines . map (unwords . map show) . sud2grid

-- Function to get the filename of Sudoku from command line arguments
getSudokuName :: [String] -> String
getSudokuName [] = error "Filename of sudoku as the first argument."
getSudokuName (x:_) = x

-- Functions to find free values in rows, columns, and subgrids
freeInRow :: Sudoku -> Row -> [Value]
freeInRow sud row = values \\ [sud (row, col) | col <- positions]

freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn sud col = values \\ [sud (row, col) | row <- positions]

freeInSubgrid :: Sudoku -> (Row, Column) -> [Value]
freeInSubgrid sud (row, col) = values \\ [sud (r, c) | r <- blockRows, c <- blockCols]
    where blockRows = getBlockIndices row
          blockCols = getBlockIndices col
          getBlockIndices x = let start = 3 * ((x - 1) `div` 3) + 1 in [start..start+2]

-- Function to find free values at a specific position
freeAtPos :: Sudoku -> (Row, Column) -> [Value]
freeAtPos sud (row, col) = intersect (freeInRow sud row) $ intersect (freeInColumn sud col) (freeInSubgrid sud (row, col))

-- Function to find open positions in Sudoku grid
openPositions :: Sudoku -> [(Row, Column)]
openPositions sud = [(r, c) | r <- positions, c <- positions, sud (r, c) == 0]

-- Functions to check the validity of rows, columns, subgrids, and overall consistency
rowValid :: Sudoku -> Row -> Bool
rowValid sud row = let rowValues = [sud (row, col) | col <- positions] in nub rowValues == rowValues

colValid :: Sudoku -> Column -> Bool
colValid sud col = let colValues = [sud (row, col) | row <- positions] in nub colValues == colValues

subgridValid :: Sudoku -> (Row, Column) -> Bool
subgridValid sud (row, col) =
  let subgridValues = [sud (r, c) | r <- blockRows, c <- blockCols]
  in nub subgridValues == subgridValues
  where
    blockRows = getBlockIndices row
    blockCols = getBlockIndices col
    getBlockIndices x = let start = 3 * (x - 1) `div` 3 + 1 in [start..start+2]

-- Function to check overall consistency of Sudoku grid
consistent :: Sudoku -> Bool
consistent sud =
  all (\(r, c) -> let blockRows' = blockRows r
                      blockCols' = blockCols c
                  in nub [sud (r', c') | r' <- blockRows', c' <- blockCols', sud (r', c') /= 0] ==
                     [sud (r', c') | r' <- blockRows', c' <- blockCols', sud (r', c') /= 0])
    [(r, c) | r <- centerOfBlocks, c <- centerOfBlocks]
  where
    blockRows x = let start = 3 * ((x - 1) `div` 3) + 1 in [start..start+2]
    blockCols = blockRows

-- Function to generate a list of constraints for open positions
constraints :: Sudoku -> [Constraint]
constraints sud = [(r, c, freeAtPos sud (r, c)) | (r, c) <- openPositions sud]

-- Function to solve Sudoku using depth-first search
solveSudoku :: Sudoku -> Sudoku
solveSudoku sud
    | null (openPositions sud) = sud
    | otherwise = solve $ head $ dfs [(sud, constraints sud)]
    where
        dfs :: [Node] -> [Node]
        dfs [] = []
        dfs ((s, cs):nodes)
            | null cs = (s, cs) : dfs nodes
            | null options = dfs nodes
            | otherwise = dfs $ newNodes ++ nodes
            where
                ((r, c, options):cs') = cs
                newNodes = [(extend s (r, c, v), sortBy (\(_, _, vs) (_, _, vs') -> compare (length vs) (length vs')) (constraints $ extend s (r, c, v))) | v <- options]
        
        solve :: Node -> Sudoku
        solve (s, _) = solveSudoku s

-- Main function to read and solve Sudoku from a file
main :: IO ()
main = do
    args <- getArgs
    maybeSudoku <- readSudoku (getSudokuName args)
    
    case maybeSudoku of
        Just sud | consistent sud -> do
            let finalSudoku = solveSudoku sud
            printSudoku finalSudoku
        _ -> error "Invalid or unsolvable sudoku"
