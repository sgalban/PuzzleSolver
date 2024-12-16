module PuzzlePrinter (printPuzzleE, printPuzzleSolution, main, prettyPrint) where
import PuzzleSyntax
import PuzzleSolver (PuzzleSolution(PuzzleSolution))
import PuzzleEvaluator (PuzzleE (PE), ConstraintE (CE), ConstraintEType (Value))
import Text.PrettyPrint qualified as PP
import Text.PrettyPrint ((<+>))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

-- ASCII Grid

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

-- Pretty Print

-- PrettyPrint for NumExp
prettyNumExp :: NumExp -> PP.Doc
prettyNumExp (Number n) = PP.int n
prettyNumExp (RepeatVar n) = PP.char '$' <> PP.int n
prettyNumExp (Op2 l op r) = PP.parens $ prettyNumExp l <+> prettyBop op <+> prettyNumExp r

-- PrettyPrint for Bop
prettyBop :: Bop -> PP.Doc
prettyBop Plus = PP.char '+'
prettyBop Minus = PP.char '-'
prettyBop Times = PP.char '*'
prettyBop Divide = PP.text "//"
prettyBop Modulo = PP.char '%'
prettyBop Power = PP.char '^'

-- PrettyPrint for Range
prettyRange :: Range -> PP.Doc
prettyRange (Range (start, end)) = prettyNumExp start <+> PP.text "to" <+> prettyNumExp end

prettyCellGroup :: CellGroup -> PP.Doc
prettyCellGroup (ACell r c) =
  PP.text "cell" <> PP.parens (prettyNumExp r <> (PP.comma <+> prettyNumExp c))
prettyCellGroup (CellList cells) =
    PP.text "cells" <> PP.brackets (PP.hcat $ PP.punctuate PP.comma (map prettyCellGroup cells))
prettyCellGroup (Subgrid r c w h) =
    PP.text "subgrid" <> PP.parens (PP.hsep $ PP.punctuate PP.comma [prettyNumExp r, prettyNumExp c, prettyNumExp w, prettyNumExp h])
prettyCellGroup (Row idx range) =
    PP.text "row" <+> prettyNumExp idx <+> maybe PP.empty (\r -> PP.text "from" <+> prettyRange r) range
prettyCellGroup (Col idx range) =
    PP.text "col" <+> prettyNumExp idx <+> maybe PP.empty (\r -> PP.text "from" <+> prettyRange r) range
prettyCellGroup (Inverse cells) =
    PP.char '~' <> prettyCellGroup cells
prettyCellGroup All =
    PP.text "all"
prettyCellGroup Unconstrained =
    PP.text "unconstrained"

-- PrettyPrint for PrimitiveConstraint
prettyPrimitiveConstraint :: PrimitiveConstraint -> PP.Doc
prettyPrimitiveConstraint (Unique range) =
  PP.text "unique" <> PP.parens (prettyRange range)
prettyPrimitiveConstraint (AddsTo n) =
  PP.text "addsTo" <> PP.parens (prettyNumExp n)
prettyPrimitiveConstraint (MultsTo n) =
  PP.text "multTo" <> PP.parens (prettyNumExp n)
prettyPrimitiveConstraint (GreaterThan r1 r2) =
  PP.text "greaterThan" <> PP.parens (prettyNumExp r1 <> (PP.comma <+> prettyNumExp r2))
prettyPrimitiveConstraint (LessThan r1 r2) =
  PP.text "lessThan" <> PP.parens (prettyNumExp r1 <> (PP.comma <+> prettyNumExp r2))
prettyPrimitiveConstraint Always =
  PP.text "always"
prettyPrimitiveConstraint Never =
  PP.text "never"

-- PrettyPrint for Constraint
prettyConstraint :: Constraint -> PP.Doc
prettyConstraint (PC p) = PP.text "constraint" <+> prettyPrimitiveConstraint p
prettyConstraint (ConstraintList ps) =
  PP.text "constraints" <> PP.brackets (PP.hcat $ PP.punctuate PP.comma (map prettyPrimitiveConstraint ps))

-- PrettyPrint for ConstrainedCells
prettyConstrainedCells :: ConstrainedCells -> PP.Doc
prettyConstrainedCells (ConstrainedCells c group) =
  prettyConstraint c <+> PP.text "in" <+> prettyCellGroup group

-- PrettyPrint for ConstraintRule
prettyConstraintRule :: ConstraintRule -> PP.Doc
prettyConstraintRule (CellInit cells n) =
    PP.text "init" <+> prettyCellGroup cells <+> PP.text "with" <+> prettyNumExp n
prettyConstraintRule (Repeat range rule) =
    PP.text "repeat" <+> PP.parens (prettyRange range) <+> PP.braces (prettyConstraintRule rule)
prettyConstraintRule (CC cc) =
    prettyConstrainedCells cc

prettyPuzzle :: PuzzleSyntax -> PP.Doc
prettyPuzzle (Grid w h rules) =
  PP.text "grid" <+>
  PP.parens ((PP.int w <> PP.comma) <+> PP.int h) <+>
  PP.braces (PP.vcat (map prettyConstraintRule rules))

-- Helper Function
prettyPrint :: PuzzleSyntax -> String
prettyPrint = PP.render . prettyPuzzle

-- Test Inputs

-- Test 1: Basic grid with no constraints
testPuzzle1 :: PuzzleSyntax
testPuzzle1 = Grid 3 3 []

-- Test 2: Grid with a single cell initialization
testPuzzle2 :: PuzzleSyntax
testPuzzle2 = Grid 4 4 [CellInit (ACell (Number 0) (Number 1)) (Number (-5))]

-- >>> P.parse parsePuzzle (prettyPrint testPuzzle2)
-- Right (Grid {width = 4, height = 4, constraints = [CellInit (ACell (Number 0) (Number 1)) (Number (-5))]})

-- Test 3: Grid with constraints and a repeat rule
testPuzzle3 :: PuzzleSyntax
testPuzzle3 = Grid 4 4
  [ CC (ConstrainedCells (PC (AddsTo (Number 10))) (Row (Number 0) Nothing)),
    Repeat (Range (Number 0, Number 2)) (CC (ConstrainedCells (PC Always) All))
  ]

-- Test 4: Complex grid with multiple nested repeats and constraints
testPuzzle4 :: PuzzleSyntax
testPuzzle4 = Grid 3 3
  [ CC (ConstrainedCells (PC (AddsTo (Number 15))) All),
    Repeat (Range (Number 0, Number 2)) (CC (ConstrainedCells (PC (AddsTo (Number 15))) (Row (RepeatVar 0) Nothing))),
    Repeat (Range (Number 0, Number 2)) (CC (ConstrainedCells (PC (AddsTo (Number 15))) (Col (RepeatVar 0) Nothing))),
    CC (ConstrainedCells (PC (AddsTo (Number 15))) (CellList [ACell (Number 0) (Number 0), ACell (Number 1) (Number 1), ACell (Number 2) (Number 2)])),
    CC (ConstrainedCells (PC (AddsTo (Number 15))) (CellList [ACell (Number 0) (Number 2), ACell (Number 1) (Number 1), ACell (Number 2) (Number 0)])),
    CellInit (ACell (Number 0) (Number 1)) (Number 9),
    CellInit (ACell (Number 1) (Number 0)) (Number 7),
    CellInit (ACell (Number 1) (Number 2)) (Number 3),
    CellInit (ACell (Number 2) (Number 2)) (Number 8)
  ]

testPrettyPrint :: IO ()
testPrettyPrint = do
  -- Test 1: Basic grid
  putStrLn "Test 1: Basic grid with no constraints"
  putStrLn $ prettyPrint testPuzzle1

  -- Test 2: Grid with a single cell initialization
  putStrLn "\nTest 2: Grid with a single cell initialization"
  putStrLn $ prettyPrint testPuzzle2

  -- Test 3: Grid with constraints and a repeat rule
  putStrLn "\nTest 3: Grid with constraints and a repeat rule"
  putStrLn $ prettyPrint testPuzzle3

  -- Test 4: Complex grid with multiple nested repeats and constraints
  putStrLn "\nTest 4: Complex grid with multiple nested repeats and constraints"
  putStrLn $ prettyPrint testPuzzle4


main :: IO ()
main = do
  testPrettyPrint
  putStrLn "Printing PuzzleE:"
  putStrLn $ printPuzzleE examplePuzzleE
  putStrLn "Printing PuzzleSolution:"
  putStrLn $ printPuzzleSolution examplePuzzleSolution
  putStrLn "Printing Large PuzzleE:"
  putStrLn $ printPuzzleE exampleLargePuzzleE
  putStrLn "Printing Large PuzzleSolution:"
  putStrLn $ printPuzzleSolution exampleLargePuzzleSolution



