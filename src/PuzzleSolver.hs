module PuzzleSolver(solve, validate) where
import Puzzle
import qualified Test.QuickCheck as QC
import Data.Maybe (isJust)
import qualified Data.List as List
import qualified Data.Map as Map

solve :: Puzzle -> Maybe PuzzleSolution
solve puzzle = undefined

validate :: PuzzleSolution -> Bool
validate ps = undefined

prop_solveSamePuzzle :: Puzzle -> QC.Property
prop_solveSamePuzzle p = isJust ps QC.==> maybe True (\ps' -> puzzle ps' == p) ps
  where
    ps = solve p

prop_solveValid :: Puzzle -> QC.Property
prop_solveValid p = isJust ps QC.==> maybe True validate ps
  where
    ps = solve p

prop_solveComplete :: Puzzle -> QC.Property
prop_solveComplete p@(Grid w h _) = isJust ps QC.==> maybe True complete ps
  where
    ps = solve p
    coords = List.sort [(r, c) | r <- [0..h-1], c <- [0..w-1]]
    complete :: PuzzleSolution -> Bool
    complete (PuzzleSolution _ s) = List.sort (Map.keys s) == coords

prop_emptyValid :: Puzzle -> QC.Property
prop_emptyValid p = isValidPuzzle p QC.==> validate $ PuzzleSolution p Map.empty

prop_subsolutionValid :: PuzzleSolution -> QC.Property
prop_subsolutionValid ps@(PuzzleSolution p s) =
  validate ps QC.==> case Map.keys s of
    (k : _) ->  validate $ PuzzleSolution p (Map.delete k s)
    _ -> True

prop_addInvalid :: PuzzleSolution -> Int -> Int -> Int -> QC.Property
prop_addInvalid ps@(PuzzleSolution p@(Grid w h _) s) r c v =
  not (validate ps) QC.==> case Map.keys s of
    (k : _) ->  not . validate $ PuzzleSolution p (Map.insert pair v s)
    _ -> True
    where
      pair = (r `mod` h, c `mod` h)

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