module PuzzlePrinter (printPuzzleE, printPuzzleSolution, main) where
import PuzzleSyntax
import PuzzleSolver (PuzzleSolution(PuzzleSolution))
import PuzzleEvaluator (PuzzleE (PE), ConstraintE (CE), ConstraintEType (Value))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

-- Function to print PuzzleE as an ASCII grid
printPuzzleE :: PuzzleE -> String
printPuzzleE (PE w h constraints) =
  unlines $ intercalateLines w [rowToAscii r | r <- [0..(h-1)]]
  where
    cellValues = Map.fromList
      [ (cell, processNumber val)
      | CE (Value val) cells <- Set.toList constraints
      , cell <- Set.toList cells
      ]

    -- Convert a single row to ASCII
    rowToAscii r = "|" ++ concatMap (\c -> cellToAscii (r, c)) [0..(w-1)]

    -- Convert a single cell to ASCII
    cellToAscii coord = " " ++ maybe " " show (cellValues Map.!? coord) ++ " |"

-- Function to print PuzzleSolution as an ASCII grid
printPuzzleSolution :: PuzzleSolution -> String
printPuzzleSolution (PuzzleSolution (PE w h _) cellValues) =
    unlines $ intercalateLines w [rowToAscii r | r <- [0..(h-1)]]
    where
        -- Convert a single row to ASCII
        rowToAscii r = "|" ++ concatMap (\c -> cellToAscii (r, c)) [0..(w-1)]

        -- Convert a single cell to ASCII
        cellVal cell = maybe " " (show . processNumber) (cellValues Map.!? cell)
        cellToAscii cell = " " ++ cellVal cell ++ " |"

processNumber :: Int -> Int
processNumber n = n `mod` 10

intercalateLines :: Int -> [String] -> [String]
intercalateLines w rows =
  let separator = concat (replicate w "+---") ++ "+"
  in concatMap (\row -> [separator, row]) rows ++ [separator]

-- Example puzzles for testing
examplePuzzleE :: PuzzleE
examplePuzzleE = PE 3 3 (Set.fromList [
  CE (Value 1) (Set.fromList [(0, 0), (1, 1)]),
  CE (Value 12) (Set.fromList [(2, 2)])])

examplePuzzleSolution :: PuzzleSolution
examplePuzzleSolution = PuzzleSolution
  (PE 3 3 Set.empty)
  (Map.fromList [((0, 0), 15), ((1, 1), 25), ((2, 2), 35)])


exampleLargePuzzleE :: PuzzleE
exampleLargePuzzleE = PE 5 5 (Set.fromList
  [ CE (Value 12) (Set.fromList [(0, 0), (1, 1)]),
    CE (Value 15) (Set.fromList [(2, 2), (3, 3)]),
    CE (Value 22) (Set.fromList [(4, 4)])
  ])

exampleLargePuzzleSolution :: PuzzleSolution
exampleLargePuzzleSolution = PuzzleSolution
  (PE 5 5 Set.empty)  -- Replace Grid with PE
  (Map.fromList [
    ((0, 0), 12),
    ((0, 1), 15),
    ((1, 1), 18),
    ((2, 2), 25),
    ((3, 3), 35),
    ((4, 4), 45)
  ])


main :: IO ()
main = do
  putStrLn "Printing PuzzleE:"
  putStrLn $ printPuzzleE examplePuzzleE
  putStrLn "Printing PuzzleSolution:"
  putStrLn $ printPuzzleSolution examplePuzzleSolution
  putStrLn "Printing Large PuzzleE:"
  putStrLn $ printPuzzleE exampleLargePuzzleE
  putStrLn "Printing Large PuzzleSolution:"
  putStrLn $ printPuzzleSolution exampleLargePuzzleSolution



