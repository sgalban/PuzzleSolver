module Main where
import qualified PuzzlePrinter
import qualified PuzzleParser
import qualified PuzzleEvaluator
import qualified PuzzleSolver
import PuzzleCLI (looper)

main :: IO ()
main = looper

runTests :: IO ()
runTests = do
  _ <- PuzzlePrinter.runAllTests
  _ <- PuzzleParser.runAllTests
  _ <- PuzzleEvaluator.runAllTests
  _ <- PuzzleSolver.runAllTests
  return ()
