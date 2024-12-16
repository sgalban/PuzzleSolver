module PuzzleSolver where
import qualified PuzzleSyntax as PS
import qualified Test.QuickCheck as QC
import Data.Maybe (isJust, isNothing, mapMaybe, fromMaybe)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map ((!?))
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import qualified PuzzleEvaluator as PE
import qualified Data.Set as Set
import Data.Either (isRight)
import GHC.Base (Alternative((<|>)))
import Debug.Trace (trace)
import Control.Monad (void)

type Cell = (Int, Int)

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
  if row >= 0 && col >= 0 && row < PE.height p && col < PE.width p
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

-- | Validate Implementation

-- | Determines if the cell values make for a valid (partial) solution
validate :: PuzzleSolution -> Bool
validate ps = not (hasOobCell || hasViolatedConstraint)
  where
    pe = puzzle ps
    pw = PE.width pe
    ph = PE.height pe
    cellIsOob (r, c) = r < 0 || r >= ph || c < 0 || c >= pw
    hasOobCell = any cellIsOob (Map.keys (cellValues ps))
    hasViolatedConstraint = not $ all (constraintUnviolated ps) (PE.constraints pe)

isComplete :: PuzzleSolution -> Bool
isComplete (PuzzleSolution (PE.PE w h _) cellVals) =
  PE.allCells w h == Map.keysSet cellVals

-- | Determines if a particular constraint is unviolated. The rules used to
-- | determine what qualifies as a violation varies from constraint to
-- | constraint
constraintUnviolated :: PuzzleSolution -> PE.ConstraintE -> Bool
-- | Always valid
constraintUnviolated _ (PE.CE PE.Always _) = True
-- | Never valid
constraintUnviolated _ (PE.CE PE.Never _) = False
-- | Valid if all non-empty cells match the value
constraintUnviolated ps (PE.CE (PE.Value val) cg) = all ( == val) (valuesIn ps cg)
-- | Valid if all non-empty cells have values within the range, and there are
-- | no duplicate (non-empty) values in the group
constraintUnviolated ps (PE.CE (PE.Unique (from, to)) cg) = allInRange && allUnique
  where
    values = valuesIn ps cg
    allInRange = all (\x -> x >= from && x <= to) values
    allUnique = length values == Set.size (Set.fromList values)
-- | Valid if either there's at least one non-empty cell in the group and the
-- | sum of the values in the non-empty cells is less than the target sum, or
-- | every cell in the group is non-empty and the the sum is exactly the target
constraintUnviolated ps (PE.CE (PE.AddsTo sum') cg) =
  totalconstraintUnviolated sum ps sum' cg
-- | Valid if either there's at least one non-empty cell in the group and the
-- | product of the values in the non-empty cells is less than the target
-- | product, or every cell in the group is non-empty and the the product is
-- | exactly the target
constraintUnviolated ps (PE.CE (PE.MultsTo prod) cg) =
  totalconstraintUnviolated product ps prod cg
-- | Valid if either the target cell is empty, or every non-empty value in the
-- | group is greater than the value in the target
constraintUnviolated ps (PE.CE (PE.GreaterThan r c) cg) =
  compareconstraintUnviolated (>) ps r c cg
-- | Valid if either the target cell is empty, or every non-empty value in the
-- | group is less than the value in the target
constraintUnviolated ps (PE.CE (PE.LessThan r c) cg) =
  compareconstraintUnviolated (<) ps r c cg

totalconstraintUnviolated :: ([Int] -> Int) -> PuzzleSolution -> Int
  -> Set.Set Cell -> Bool
totalconstraintUnviolated totalF ps val cg
  | total > val = False
  | total < val && length values == Set.size cg = False
  | otherwise = True
  where
    values = valuesIn ps cg
    total = totalF values

compareconstraintUnviolated :: (Int -> Int -> Bool)
  -> PuzzleSolution -> Int -> Int -> Set.Set Cell -> Bool
compareconstraintUnviolated compF ps r c cg = case valueAt ps (r, c) of
  Nothing -> True
  Just val -> all (`compF` val) (valuesIn ps cg)

-- | Solver implementation

-- | An error that can be thrown while solving
data SolveError
  = EvaluationError PE.EvalError
  | NoSolutionFound
  | BadState
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

-- | Validates a `Guesses` directly, given a puzzle
validateG :: Guesses -> PE.PuzzleE -> Bool
validateG gs pe = validate $ solutionFromGuesses gs pe

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
-- | Returns Nothing if this rules out all possible values for the cell. Note
-- | that this operation can result in `potential`s containing a single cell,
-- | and will not automatically move them to `known`
removeGuessValueAt :: Cell -> Guesses -> Int -> Maybe Guesses
removeGuessValueAt = doGuessOp removeGuessValue
  where
    removeGuessValue val (Potential vals) = case Set.toList newVals of
      [] -> NoSol
      _ -> Potential newVals
      where
        newVals = Set.delete val vals
    removeGuessValue val (Known x)
      | x == val = NoSol
      | otherwise = Known x
    removeGuessValue _ NoSol = NoSol

-- | Same as the above function but takes in a Maybe Guesses, returning Nothing
-- | if it's Nothing. Useful for foldr
removeGuessValueAtM :: Cell -> Maybe Guesses -> Int -> Maybe Guesses
removeGuessValueAtM cell gs val = do
  gs' <- gs
  removeGuessValueAt cell gs' val

solutionFromGuesses :: Guesses -> PE.PuzzleE -> PuzzleSolution
solutionFromGuesses gs pe = PuzzleSolution pe (known gs)

-- | Get's the set of initial guesses for a puzzle, using its dimensions.
-- | All cells could potentially be anything from 0 to maxValue
initialGuesses :: Int -> PE.PuzzleE -> Guesses
initialGuesses maxValue pe = GS Map.empty (Map.fromList [(key, allVals) | key <- cells])
  where
    h = PE.height pe
    w = PE.width pe
    cells = [(r, c) | r <- [0..(h - 1)], c <- [0..(w - 1)]]
    allVals = Set.fromList [0..maxValue]

-- | A type solely used in the below function
type TakenInits = Maybe (PE.PuzzleE, Guesses, Set.Set Cell)

-- | Apply the Value constraints (cellInits) to the puzzle. This produces a
-- | triple containing the following values:
-- | 1) An evaluated puzzle with all Value constraints removed, but is otherwise
-- |    identical to the input
-- | 2) A Guesses in which all cells affected by a Value have been set to,
-- |    MustBe, unless 2 different values are applied to a single cell, in which
-- |    case an error is thrown
-- | 3) A set of cells affected by Value constraints
takeCellInits :: Int -> PE.PuzzleE -> TakenInits
takeCellInits maxValue pe = foldr applyCons (Just (pe, gs, Set.empty)) cons
  where
    gs = initialGuesses maxValue pe
    cons = PE.constraints pe

    applyCons :: PE.ConstraintE -> TakenInits -> TakenInits
    applyCons c@(PE.CE (PE.Value val) cells) (Just (peAcc, gsAcc, cellsAcc)) =
      let newPe = peAcc { PE.constraints = Set.delete c (PE.constraints peAcc) }
      in foldr (setCellKnown val) (Just (newPe, gsAcc, cellsAcc)) cells
    applyCons _ acc = acc

    setCellKnown :: Int -> Cell -> TakenInits -> TakenInits
    setCellKnown val cell (Just (peAcc, gsAcc, cellsAcc)) =
      let
        newCells = Set.insert cell cellsAcc
        newGs = setGuessAt cell gsAcc val
      in case newGs of
        Just newGs' -> Just (peAcc, newGs', newCells)
        Nothing -> Nothing
    setCellKnown _ _ err = err

-- | Given that a cell is now known to have a given value, narrow down the
-- | remaining guesses using heuristics with respect to the given constraint, or
-- | throw an error if this results in an unsolvable puzzle
applyHeuristic :: Cell -> Int -> PE.ConstraintE -> Guesses -> Maybe Guesses
applyHeuristic cell val (PE.CE (PE.Unique (from, to)) _) gs = Just gs
applyHeuristic cell val (PE.CE cType cells) gs =
  foldr (step cell val cType) (Just gs) cells
  where
    step knownCell val cType cell gs = do
      gs' <- gs
      if knownCell == cell
        then Just gs'
        else guessForHeuristic knownCell val cType cell gs'
    guessForHeuristic :: Cell -> Int -> PE.ConstraintEType -> Cell -> Guesses -> Maybe Guesses
    guessForHeuristic _ val (PE.Unique (to, from)) cell gs =
      removeGuessValueAt cell gs val
    guessForHeuristic knownCell val (PE.LessThan r c) cell gs
      | knownCell == (r, c) =
        foldr (flip (removeGuessValueAtM cell)) (Just gs) [val + 1 .. 9]
      | cell == (r, c) =
        foldr (flip (removeGuessValueAtM cell)) (Just gs) [1 .. val - 1]
      | otherwise = Just gs
    guessForHeuristic knownCell val (PE.GreaterThan r c) cell gs
      | cell == (r, c) =
        foldr (flip (removeGuessValueAtM cell)) (Just gs) [val + 1 .. 9]
      | knownCell == (r, c) =
        foldr (flip (removeGuessValueAtM cell)) (Just gs) [1 .. val - 1]
      | otherwise = Just gs
    guessForHeuristic _ _ _ _ gs = Just gs

-- | Given that a cell is now known to have a given value, narrow down the
-- | remaining guesses using the heuristics that apply to the constraints
-- | affected by that cell. Throws an error if this results in an unsolvable
-- | puzzle
applyHeuristics :: Cell -> Int -> PE.CellConstraints -> Guesses -> Maybe Guesses
applyHeuristics cell val ccs gs = foldr step (Just gs) cons
  where
    step :: PE.ConstraintE -> Maybe Guesses -> Maybe Guesses
    step _ Nothing = Nothing
    step con (Just acc) = applyHeuristic cell val con acc
    cons = Map.findWithDefault Set.empty cell ccs

solve :: PS.PuzzleSyntax -> Either SolveError PuzzleSolution
solve puzzle = case PE.evaluatePuzzle puzzle of
  Left evalError -> Left (EvaluationError evalError)
  Right pe -> case solveE 9 pe of
    Just sol -> Right sol
    Nothing -> Left NoSolutionFound

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
solveE :: Int -> PE.PuzzleE -> Maybe PuzzleSolution
solveE maxValue pe = do
  -- Step 1
  (iPe, iGs, affectedCells) <- takeCellInits maxValue pe

  -- Step 2
  let cc = PE.cellConstraints iPe

  -- Step 3
  gs <- foldr (applyHeuristicsForCell cc) (Just iGs) affectedCells

  -- Steps 4 - 7
  solution <- (`solutionFromGuesses` iPe) <$> doLoop 0 cc iPe gs

  -- Ensure the original puzzle matches
  return $ solution { puzzle = pe }

  where
    applyHeuristicsForCell :: PE.CellConstraints -> Cell -> Maybe Guesses -> Maybe Guesses
    applyHeuristicsForCell cellCons cell acc = do
      guesses <- acc
      case getGuessAt guesses cell of
        Known val -> applyHeuristics cell val cellCons guesses
        _ -> return guesses

    doLoop :: Int -> PE.CellConstraints -> PE.PuzzleE -> Guesses -> Maybe Guesses
    doLoop depth cc pe gs = case Map.lookupMin (potential gs) of
      -- Step 4
      Nothing -> return gs
      -- Step 5
      Just cellVals -> tryAtCell depth cc pe gs cellVals

    tryAtCell :: Int -> PE.CellConstraints -> PE.PuzzleE -> Guesses -> (Cell, Set.Set Int) -> Maybe Guesses
    tryAtCell depth cc pe gs (cell, potentials) = foldr step Nothing potentials
      where
        step val acc = tryValue val <|> acc
        tryValue val = do
          let gs' = setGuessAt cell gs val
          -- Step 6
          newGuesses <- applyHeuristicsForCell cc cell gs'
          -- Step 7
          if validateG newGuesses pe
            then doLoop (depth + 1) cc pe newGuesses
            else Nothing

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

sFutoshiki :: PuzzleSolution
sFutoshiki = PuzzleSolution PE.eFutoshiki $ toSolutionMap 5 [
  5, 4, 3, 2, 1,
  4, 3, 1, 5, 2,
  2, 1, 4, 3, 5,
  3, 5, 2, 1, 4,
  1, 2, 5, 4, 3]

sKenKen :: PuzzleSolution
sKenKen = PuzzleSolution PE.eKenKen $ toSolutionMap 4 [
  1, 4, 2, 3,
  3, 2, 1, 4,
  2, 3, 4, 1,
  4, 1, 3, 2]

prop_solveSamePuzzle :: PE.PuzzleE -> QC.Property
prop_solveSamePuzzle pe = isJust pSol QC.==> (puzzle <$> pSol) == Just pe
  where
    pSol = solveE 4 pe

prop_solveValid :: PE.PuzzleE -> QC.Property
prop_solveValid pe = isJust pSol QC.==> (validate <$> pSol) == Just True
  where
    pSol = solveE 4 pe

prop_solveComplete :: PE.PuzzleE -> QC.Property
prop_solveComplete pe = isJust pSol QC.==> (isComplete <$> pSol) == Just True
  where
    pSol = solveE 4 pe

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

test_solveE :: Test
test_solveE =
  "Testing Solver"
    ~: TestList
      [solveE 9 PE.eSudSmall ~?= Just sSudSmall,
      solveE 9 PE.eMagSquare ~?= Just sMagSquare,
      solveE 9 PE.eKakSmall ~?= Just sKakSmall,
      solveE 9 PE.eFutoshiki ~?= Just sFutoshiki,
      solveE 9 PE.eKenKen ~?= Just sKenKen]

test_solveFull :: Test
test_solveFull =
  "Testing Solver (Full Trip)"
    ~: TestList
      [solve PS.pSudSmall ~?= Right sSudSmall,
      solve PS.pMagSquare ~?= Right sMagSquare,
      solve PS.pKakSmall ~?= Right sKakSmall,
      solve PS.pFutoshiki ~?= Right sFutoshiki,
      solve PS.pKenKen ~?= Right sKenKen]

runAllTests :: IO ()
runAllTests = do
  _ <- runTestTT test_solveE
  _ <- runTestTT test_solveFull
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