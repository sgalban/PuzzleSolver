module PuzzleSolver(solve, validate, isValid, checkProps) where
import PuzzleSyntax
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

valuesIn :: PuzzleSolution -> Set.Set (Int, Int) -> [Int]
valuesIn ps cg = mapMaybe (valueAt ps) (Set.toList cg)

solve :: PuzzleSyntax -> Maybe PuzzleSolution
solve puzzle = undefined

-- | Determines if a puzzle solution is valid, i.e., does not violate any of its
-- | constraints, and none of the cell values are out of bounds.
-- | A non-constrained in-bounds cell is always considered non-violated.
validate :: PuzzleSolution -> Either EvalError Bool
validate ps = validateE ps <$> evaluatePuzzle (puzzle ps)

-- | Determines if the cell values make for a valid (partial) solution given
-- | the evaluated puzzle
validateE :: PuzzleSolution -> PuzzleE -> Bool
validateE ps pe = not (hasOobCell || hasViolatedConstraint)
  where
    pw = width $ puzzle ps
    ph = height $ puzzle ps
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

-- | Returns true iff the partial solution validates and the underlying puzzle
-- | can be evaluated
isValid :: PuzzleSolution -> Bool
isValid ps = case validate ps of
  Left _ -> False
  Right b -> b

prop_solveSamePuzzle :: PuzzleSyntax -> QC.Property
prop_solveSamePuzzle p = isJust ps QC.==> maybe True (\ps' -> puzzle ps' == p) ps
  where
    ps = solve p

prop_solveValid :: PuzzleSyntax -> QC.Property
prop_solveValid p = isJust ps QC.==> maybe True isValid ps
  where
    ps = solve p

prop_solveComplete :: PuzzleSyntax -> QC.Property
prop_solveComplete p@(Grid w h _) = isJust ps QC.==> maybe True complete ps
  where
    ps = solve p
    coords = List.sort [(r, c) | r <- [0..h-1], c <- [0..w-1]]
    complete :: PuzzleSolution -> Bool
    complete (PuzzleSolution _ s) = List.sort (Map.keys s) == coords

prop_emptyValid :: PuzzleSyntax -> Bool
prop_emptyValid p = case validate (PuzzleSolution p Map.empty) of
  Left _ -> True
  Right b -> b

prop_subsolutionValid :: PuzzleSolution -> QC.Property
prop_subsolutionValid ps@(PuzzleSolution _ s) =
  isValid ps QC.==> case Map.keys s of
    (k : _) -> isValid $ removeCellValue ps k
    _ -> True

prop_addInvalid :: PuzzleSolution -> Int -> Int -> Int -> QC.Property
prop_addInvalid ps@(PuzzleSolution p@(Grid w h _) s) r c v =
  not (isValid ps) && not (Map.member pair s) QC.==> case Map.keys s of
    (k : _) -> case ps' of
      Just sol -> not . isValid $ sol
      _ -> True
    _ -> True
    where
      pair = (r `mod` h, c `mod` h)
      ps' = putCellValue ps pair v

test_solve :: Test
test_solve =
  "Testing Solver"
    ~: TestList
      [solve pSudSmall ~?= Just sSudSmall,
      solve pMagSquare ~?= Just sMagSquare,
      solve pKakSmall ~?= Just sKakSmall]

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