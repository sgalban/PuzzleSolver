module PuzzleCLI where
import Puzzle (Puzzle, PuzzleSolution (puzzle))

data Looper = Looper
  { filename :: Maybe String,
    puzzleSolution :: Maybe PuzzleSolution }

initialLooper :: Looper
initialLooper =
  Looper
    { filename = Nothing,
      puzzleSolution = Nothing}

looper :: IO ()
looper = go initialLooper
  where
    go :: Looper -> IO ()
    go l = do
      undefined