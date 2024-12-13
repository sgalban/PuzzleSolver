module PuzzleSolver(solve, validate, checkProps, PuzzleSolution) where
import qualified PuzzleSyntax as PS
import qualified Test.QuickCheck as QC
import Data.Maybe (isJust, isNothing, mapMaybe)
import qualified Data.List as List
import qualified Data.Map as Map
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import PuzzleEvaluator (evaluatePuzzle, EvalError, PuzzleE, ConstraintE (CE))
import qualified PuzzleEvaluator as PE
import qualified Data.Set as Set

valueAt :: PuzzleSolution -> (Int, Int) -> Maybe Int
valueAt ps coord = cellValues ps Map.!? coord

-- | A (potentially partial) solution for a puzzle
data PuzzleSolution = PuzzleSolution {
  puzzle :: PE.PuzzleE,
  cellValues :: Map.Map Cell Int
  } deriving (Show, Eq)

-- | Sets a value in a PuzzleSolution at the specified coordinate. If a value
-- | already exists at that coordinate, it will be overridden
-- | Fails if the coordinate is not in the bounds of the puzzle
putCellValue :: PuzzleSolution -> Cell -> Int -> Maybe PuzzleSolution
putCellValue (PuzzleSolution p s) coord@(row, col) val =
  if row > 0 && col > 0 && row < PE.height p && col < PE.width p
    then Just $ PuzzleSolution p (Map.insert coord val s)
    else Nothing

-- | Removes the value of the solution at the specified coordinate.
-- | If the coordinate does not exist in the solution, this is a no-op
removeCellValue :: PuzzleSolution -> Cell -> PuzzleSolution
removeCellValue (PuzzleSolution p s) coord@(row, col) =
  PuzzleSolution p (Map.delete coord s)

valueAt :: PuzzleSolution -> Cell -> Maybe Int
valueAt ps coord = cellValues ps !? coord

valuesIn :: PuzzleSolution -> Set.Set Cell -> [Int]
valuesIn ps cg = mapMaybe (valueAt ps) (Set.toList cg)

solve :: PuzzleSyntax -> Maybe PuzzleSolution
solve puzzle = undefined

-- | Determines if the cell values make for a valid (partial) solution
validate :: PuzzleSolution -> Bool
validate ps = not (hasOobCell || hasViolatedConstraint)
  where
    pe = puzzle ps
    pw = PE.width pe
    ph = PE.height pe
    cellIsOob (r, c) = r < 0 || r >= ph || c < 0 || c >= pw
    hasOobCell = any cellIsOob (Map.keys (cellValues ps))
    hasViolatedConstraint = any (constraintViolated ps) (PE.constraints pe)

-- | Determines if a particular constraint is unviolated. The rules used to
-- | determine what qualifies as a violation varies from constraint to
-- | constraint
constraintViolated :: PuzzleSolution -> ConstraintE -> Bool
-- | Always valid
constraintViolated _ (CE PE.Always _) = True
-- | Never valid
constraintViolated _ (CE PE.Never _) = False
-- | Valid if all non-empty cells match the value
constraintViolated ps (CE (PE.Value val) cg) = all ( == val) (valuesIn ps cg)
-- | Valid if all non-empty cells have values within the range, and there are
-- | no duplicate (non-empty) values in the group
constraintViolated ps (CE (PE.Unique (from, to)) cg) = allInRange && allUnique
  where
    values = valuesIn ps cg
    allInRange = all (\x -> x >= from && x <= to) values
    allUnique = length values == Set.size (Set.fromList values)
-- | Valid if either there's at least one non-empty cell in the group and the
-- | sum of the values in the non-empty cells is less than the target sum, or
-- | every cell in the group is non-empty and the the sum is exactly the target
constraintViolated ps (CE (PE.AddsTo sum') cg) =
  totalConstraintViolated sum ps sum' cg
-- | Valid if either there's at least one non-empty cell in the group and the
-- | produc of the values in the non-empty cells is less than the target
-- | product, or every cell in the group is non-empty and the the product is
-- | exactly the target
constraintViolated ps (CE (PE.MultsTo prod) cg) =
  totalConstraintViolated product ps prod cg
-- | Valid if either the target cell is empty, or every non-empty value in the
-- | group is greater than the value in the target
constraintViolated ps (CE (PE.GreaterThan r c) cg) =
  compareConstraintViolated (>) ps r c cg
-- | Valid if either the target cell is empty, or every non-empty value in the
-- | group is less than the value in the target
constraintViolated ps (CE (PE.LessThan r c) cg) =
  compareConstraintViolated (<) ps r c cg

totalConstraintViolated :: ([Int] -> Int) -> PuzzleSolution -> Int
  -> Set.Set (Int, Int) -> Bool
totalConstraintViolated totalF ps val cg
  | total > val = False
  | total < val && length values == Set.size cg = False
  | otherwise = True
  where
    values = valuesIn ps cg
    total = totalF values

compareConstraintViolated :: (Int -> Int -> Bool)
  -> PuzzleSolution -> Int -> Int -> Set.Set (Int, Int) -> Bool
compareConstraintViolated compF ps r c cg = case valueAt ps (r, c) of
  Nothing -> True
  Just val -> all (`compF` val) (valuesIn ps cg)

-- | Solver implementation

-- | An error that can be thrown while solving
data SolveError
  = EvaluationError PE.EvalError
  | NoSolutionFound
  deriving (Show, Eq)

-- | Represents all the values a cell could have based on the current state of
-- | the solver
data Guess
  = Potential (Set.Set Int) -- Multiple possible values
  | Known Int -- Exactly 1 possible value
  | NoSol -- No possible values
  deriving (Show, Eq)

-- | Remove a potential value from a guess
removeGuessValue :: Int -> Guess -> Guess
removeGuessValue val (Potential vals) = case Set.toList newVals of
  [] -> NoSol
  [x] -> Known x
  _ -> Potential newVals
  where
    newVals = Set.delete val vals
removeGuessValue val (Known x)
  | x == val = NoSol
  | otherwise = Known x
removeGuessValue _ NoSol = NoSol

-- | Remove all but one value from a guess
setGuessKnown :: Int -> Guess -> Guess
setGuessKnown _ NoSol = NoSol
setGuessKnown val (Known x)
  | x == val = Known x
  | otherwise = NoSol
setGuessKnown val (Potential vals)
  | Set.member val vals = Known val
  | otherwise = NoSol

-- | Contains guesses for every cell
-- | If the exact value of the cell is known, the cell coordinate will be in
-- | "known", along with its value
-- | Otherwise, it will be in "potential", along with a set of all potential
-- | values in that cell (determined by heuristics and the process of
-- | elimination)
-- | If both maps ever contain the same key, something went horribly wrong
data Guesses = GS {
  known :: Map.Map Cell Int,
  potential :: Map.Map Cell (Set.Set Int)
}

-- | Fetches a guess at a particular cell from a Guesses
getGuessAt :: Guesses -> Cell -> Guess
getGuessAt gs cell = case known gs !? cell of
  Just val -> Known val
  _ -> case potential gs !? cell of
    Just vals -> Potential vals
    _ -> NoSol

-- | Transforms a Guesses by performing some operation at a particular cell
-- | given a value. Returns Nothing if the operation results in an insolvable
-- | puzzle
doGuessOp :: (Int -> Guess -> Guess) -> Cell -> Guesses -> Int -> Maybe Guesses
doGuessOp op cell gs val = 
  let
    current = getGuessAt gs cell
    guess = op val current
  in case guess of
    NoSol -> Nothing
    Potential vals -> Just $ GS {
      potential = Map.insert cell vals (potential gs),
      known = known gs }
    Known val -> Just $ GS {
      potential = Map.delete cell (potential gs),
      known = Map.insert cell val (known gs) }

-- | Set a known value at a cell
-- | Returns Nothing if that value has already been ruled out for that cell
setGuessAt :: Cell -> Guesses -> Int -> Maybe Guesses
setGuessAt = doGuessOp setGuessKnown
  where
    setGuessKnown _ NoSol = NoSol
    setGuessKnown val (Known x)
      | x == val = Known x
      | otherwise = NoSol
    setGuessKnown val (Potential vals)
      | Set.member val vals = Known val
      | otherwise = NoSol

-- | Removes a potential value from a cell
-- | Returns Nothing if this rules out all possible values for the cell
removeGuessValueAt :: Cell -> Guesses -> Int -> Maybe Guesses
removeGuessValueAt = doGuessOp removeGuessValue
  where
    removeGuessValue val (Potential vals) = case Set.toList newVals of
      [] -> NoSol
      [x] -> Known x
      _ -> Potential newVals
      where
        newVals = Set.delete val vals
    removeGuessValue val (Known x)
      | x == val = NoSol
      | otherwise = Known x
    removeGuessValue _ NoSol = NoSol

-- | Get's the set of initial guesses for a puzzle, using its dimensions.
-- | All cells could potentially be anything from 0 to 9
initialGuesses :: PE.PuzzleE -> Guesses
initialGuesses pe = GS Map.empty (Map.fromList [(key, allVals) | key <- cells])
  where
    h = PE.height pe
    w = PE.width pe
    cells = [(r, c) | r <- [0..(h - 1)], c <- [0..(w - 1)]]
    allVals = Set.fromList [0..9]

-- | A type solely used in the below function
type TakenInits = Either SolveError (PE.PuzzleE, Guesses, Set.Set Cell)

-- | Apply the Value constraints (cellInits) to the puzzle. This produces a
-- | triple containing the following values:
-- | 1) An evaluated puzzle with all Value constraints removed, but is otherwise
-- |    identical to the input
-- | 2) A Guesses in which all cells affected by a Value have been set to,
-- |    MustBe, unless 2 different values are applied to a single cell, in which
-- |    case an error is thrown
-- | 3) A set of cells affected by Value constraints
takeCellInits :: PE.PuzzleE -> TakenInits
takeCellInits pe = foldr applyCons (Right (pe, gs, Set.empty)) cons
  where
    gs = initialGuesses pe
    cons = PE.constraints pe

    applyCons :: PE.ConstraintE -> TakenInits -> TakenInits
    applyCons c@(PE.CE (PE.Value val) cells) (Right (peAcc, gsAcc, cellsAcc)) =
      let newPe = peAcc { PE.constraints = Set.delete c (PE.constraints peAcc) }
      in foldr (setCellKnown val) (Right (newPe, gsAcc, cellsAcc)) cells
    applyCons _ acc = acc
    
    setCellKnown :: Int -> Cell -> TakenInits -> TakenInits
    setCellKnown val cell (Right (peAcc, gsAcc, cellsAcc)) =
      let
        newCells = Set.insert cell cellsAcc
        newGs = setGuessAt cell gsAcc val
      in case newGs of
        Just newGs' -> Right (peAcc, newGs', newCells)
        Nothing -> Left NoSolutionFound
    setCellKnown _ _ err = err

-- | Given that a cell is now known to have a given value, narrow down the
-- | remaining guesses using heuristics with respect to the given constraint, or
-- | throw an error if this results in an unsolvable puzzle
applyHeuristic :: Cell -> Int -> PE.ConstraintE -> Guesses -> Either SolveError Guesses
applyHeuristic cell val (PE.CE (PE.Unique (from, to)) _) gs = Right gs
applyHeuristic cell val _ gs = Right gs

-- | Given that a cell is now known to have a given value, narrow down the
-- | remaining guesses using the heuristics that apply to the constraints
-- | affected by that cell. Throws an error if this results in an unsolvable
-- | puzzle
applyHeuristics :: Cell -> Int -> PE.CellConstraints -> Guesses -> Either SolveError Guesses
applyHeuristics cell val ccs gs = foldr step (Right gs) cons
  where
    step :: PE.ConstraintE -> Either SolveError Guesses -> Either SolveError Guesses
    step _ (Left ex) = Left ex
    step con (Right acc) = applyHeuristic cell val con acc
    cons = Map.findWithDefault Set.empty cell ccs

solve :: PS.PuzzleSyntax -> Either SolveError PuzzleSolution
solve puzzle = case PE.evaluatePuzzle puzzle of
  Left evalError -> Left (EvaluationError evalError)
  Right pe -> solveE pe

-- | Converts Guesses into a PuzzleSolution

{-
 General Strategy:
 Step 1: Apply all the `Value` (cellInit) constraints, since they're
 straightforward. Remove them from the puzzle since this makes them no longer
 relevant.
 Step 2: Obtain a mapping from each cell to all the constraints that
 they affect (usually just all the constraints that include them in their cell
 group, though this isn't always the case).
 Step 3: Apply heuristics for constraints affecting cells that were affected by
 Step 1. Throw an exception if this results in a NoSol.
 Step 4: If all values are known, return a solution. Otherwise, go to Step 5
 Step 5: Of the remaining Potentials, choose the first one, then lock in an
 arbitrary value that it could be.
 Step 6: Apply heuristics for the chosen cell and value.
 Step 7: Validate the partial solution. If it fails to validate, undo the change
 made in Step 5 and remove the value from the guess set. Throw an error if this
 results in a NoSol. Either way, return to Step 4
-}

solveE :: PE.PuzzleE -> Either SolveError PuzzleSolution
solveE pe = do
  -- Step 1
  (iPe, iGs, affectedCells) <- takeCellInits pe

  -- Step 2
  let cellCons = PE.cellConstraints iPe

  -- Step 3
  gs <- foldr (applyHeuristicsForCell cellCons) (Right iGs) affectedCells

  -- Step 4
  doLoop gs cellCons

  where
    getKnown (Known v) = v
    getKnown _ = -1
    applyHeuristicsForCell :: PE.CellConstraints -> Cell -> Either SolveError Guesses -> Either SolveError Guesses
    applyHeuristicsForCell cellCons cell acc = do
      guesses <- acc
      let val = getKnown (getGuessAt guesses cell)
      applyHeuristics cell val cellCons guesses
    
    doLoop = undefined 

    

-- | Tests

instance QC.Arbitrary PuzzleSolution where
  arbitrary :: QC.Gen PuzzleSolution
  arbitrary = do
    puzzle <- (QC.arbitrary :: QC.Gen PE.PuzzleE)
    let w = PE.width puzzle
    let h = PE.height puzzle
    count <- QC.choose (0, w * h `div` 2)
    rs <- QC.vectorOf count (QC.choose (0, h - 1))
    cs <- QC.vectorOf count (QC.choose (0, w - 1))
    vals <- QC.vectorOf count (QC.choose (0, 9 :: Int))
    let map = Map.fromList (zip (zip rs cs) vals)
    return $ PuzzleSolution puzzle map

  shrink :: PuzzleSolution -> [PuzzleSolution]
  shrink ps = PuzzleSolution <$> QC.shrink (puzzle ps) <*> QC.shrink (cellValues ps)

-- | Transforms a list of numbers into a grid of numbers, given the width of the
-- | grid. This will make it easier to define the hardcoded puzzle solutions
toSolutionMap :: Int -> [a] -> Map.Map (Int, Int) a
toSolutionMap cols values = Map.fromList (solList values)
  where
    solList :: [a] -> [((Int, Int), a)]
    solList = zipWith (\i v -> ((i `div` cols, i `mod` cols), v)) [0..]

sSudSmall :: PuzzleSolution
sSudSmall = PuzzleSolution PE.eSudSmall $ toSolutionMap 4 [
  1, 2, 4, 3,
  3, 4, 2, 1,
  4, 1, 3, 2,
  2, 3, 1, 4]

sMagSquare :: PuzzleSolution
sMagSquare = PuzzleSolution PE.eMagSquare $ toSolutionMap 3 [
  2, 9, 4,
  7, 5, 3,
  6, 1, 8]

sKakSmall :: PuzzleSolution
sKakSmall = PuzzleSolution PE.eKakSmall $ toSolutionMap 4 [
  8, 4, 3, 0,
  3, 1, 4, 0,
  0, 9, 2, 4,
  0, 2, 8, 9]

right :: c -> (b -> c) -> Either a b -> c
right def = either (const def)

prop_solveSamePuzzle :: PE.PuzzleE -> QC.Property
prop_solveSamePuzzle p = isRight ps QC.==> right True (\ps' -> puzzle ps' == p) ps
  where
    ps = solveE p

prop_solveValid :: PS.PuzzleSyntax -> QC.Property
prop_solveValid p = isRight ps QC.==> right True validate ps
  where
    ps = solve p

prop_solveComplete :: PS.PuzzleSyntax -> QC.Property
prop_solveComplete p@(PS.Grid w h _) = isRight ps QC.==> right True complete ps
  where
    ps = solve p
    coords = List.sort [(r, c) | r <- [0..h-1], c <- [0..w-1]]
    complete :: PuzzleSolution -> Bool
    complete (PuzzleSolution _ s) = List.sort (Map.keys s) == coords

prop_emptyValid :: PE.PuzzleE -> Bool
prop_emptyValid p = validate $ PuzzleSolution p Map.empty

prop_subsolutionValid :: PuzzleSolution -> QC.Property
prop_subsolutionValid ps@(PuzzleSolution _ s) =
  validate ps QC.==> case Map.keys s of
    (k : _) -> validate $ removeCellValue ps k
    _ -> True

prop_addInvalid :: PuzzleSolution -> Int -> Int -> Int -> QC.Property
prop_addInvalid ps@(PuzzleSolution p@(PE.PE w h _) s) r c v =
  not (validate ps) && not (Map.member pair s) QC.==> case Map.keys s of
    (k : _) -> case ps' of
      Just sol -> not . validate $ sol
      _ -> True
    _ -> True
    where
      pair = (r `mod` h, c `mod` h)
      ps' = putCellValue ps pair v

test_solve :: Test
test_solve =
  "Testing Solver"
    ~: TestList
      [solve PS.pSudSmall ~?= Right sSudSmall,
      solve PS.pMagSquare ~?= Right sMagSquare,
      solve PS.pKakSmall ~?= Right sKakSmall]

checkProps :: IO ()
checkProps = do
  putStrLn "solveSamePuzzle"
  QC.quickCheck prop_solveSamePuzzle
  putStrLn "solveValid"
  QC.quickCheck prop_solveValid
  putStrLn "solveComplete"
  QC.quickCheck prop_solveComplete
  putStrLn "emptyValid"
  QC.quickCheck prop_emptyValid
  putStrLn "subsolutionValid"
  QC.quickCheck prop_subsolutionValid
  putStrLn "addInvalid"
  QC.quickCheck prop_addInvalid