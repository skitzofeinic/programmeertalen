import System.Environment
import Data.List

type Row = Int
type Column = Int
type Value = Int
type Grid = [[Value]] -- Only used to read/write from/to a file.
type Sudoku = (Row, Column) -> Value
type Constraint = (Row, Column, [Value])
type Node = (Sudoku, [Constraint])

positions :: [Int]
positions = [1..9]

values :: [Value]
values = [1..9]

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

centerOfBlocks :: [Int]
centerOfBlocks = [2, 5, 8]

sud2grid :: Sudoku -> Grid
sud2grid s = [[s (r, c) | c <- positions] | r <- positions]

grid2sud :: Grid -> Sudoku
grid2sud gr (r, c) = gr !! (r - 1) !! (c - 1)

extend :: Sudoku -> (Row, Column, Value) -> Sudoku
extend sud (r, c, v) (i, j) = if (r, c) == (i, j) then v else sud (i, j)

readSudoku :: String -> IO Sudoku
readSudoku filename = grid2sud . splitStringIntoGrid <$> readFile filename
    where splitStringIntoGrid = map (map readint . words) . lines
          readint x = read x :: Int

printSudoku :: Sudoku -> IO ()
printSudoku = putStr . unlines . map (unwords . map show) . sud2grid

getSudokuName :: [String] -> String
getSudokuName [] = error "Filename of sudoku as the first argument."
getSudokuName (x:_) = x

freeInRow :: Sudoku -> Row -> [Value]
freeInRow sud row = values \\ [sud (row, col) | col <- positions]

freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn sud col = values \\ [sud (row, col) | row <- positions]

freeInSubgrid :: Sudoku -> (Row, Column) -> [Value]
freeInSubgrid sud (row, col) = values \\ [sud (r, c) | r <- blockRows, c <- blockCols]
    where blockRows = getBlockIndices row
          blockCols = getBlockIndices col
          getBlockIndices x = let start = 3 * ((x - 1) `div` 3) + 1 in [start..start+2]

freeAtPos :: Sudoku -> (Row, Column) -> [Value]
freeAtPos sud (row, col) = intersect (freeInRow sud row) $ intersect (freeInColumn sud col) (freeInSubgrid sud (row, col))

openPositions :: Sudoku -> [(Row, Column)]
openPositions sud = [(r, c) | r <- positions, c <- positions, sud (r, c) == 0]

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

constraints :: Sudoku -> [Constraint]
constraints sud = [(r, c, freeAtPos sud (r, c)) | (r, c) <- openPositions sud]

solveSudoku :: Sudoku -> IO Sudoku
solveSudoku sud = do
    let (result, steps) = solve (sud, constraints sud)
    mapM_ (\(step, s) -> putStrLn $ "Step " ++ show step ++ ": " ++ s) (zip [1..] steps)
    return result
  where
    solve :: Node -> (Sudoku, [String])
    solve (s, []) = (s, [])
    solve (s, cs) = tryConstraints (sortBy (\(_, _, vs) (_, _, vs') -> compare (length vs) (length vs')) cs)
      where
        tryConstraints [] = (s, [])
        tryConstraints ((r, c, vs):cs') =
            case vs of
                [] -> (s, [])
                _  -> tryValues vs
          where
            tryValues [] = (s, [])
            tryValues (v:vs') =
                case filter (consistent . fst) [(extend s (r, c, v), constraints (extend s (r, c, v))) | v <- vs] of
                    []             -> tryValues vs'
                    ((s', cs''):_) -> let (result, steps) = solve (s', cs'' ++ filter (\(r', c', _) -> (r', c') /= (r, c)) cs')
                                    in (result, ("Trying value " ++ show v ++ " at position " ++ show (r, c)) : steps)

main :: IO ()
main = do
    args <- getArgs
    sud <- readSudoku (getSudokuName args)
    
    if consistent sud
        then do
            finalSudoku <- solveSudoku sud
            printSudoku finalSudoku
        else do
            error "Unsolvable sudoku"

    -- If the Sudoku is consistent and solvable, the program will continue.
    -- If the Sudoku is inconsistent, the program will terminate with an error message.
