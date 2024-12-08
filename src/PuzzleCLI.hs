module PuzzleCLI where
import PuzzleSyntax (PuzzleSyntax, PuzzleSolution (puzzle))
import Data.Maybe (fromMaybe)
import Data.List qualified as List

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
      putStr (fromMaybe "" (filename l) ++ "> ")
      str <- getLine
      case List.uncons (words str) of
        -- | Load a new puzzle file and reset the looper
        Just (":l", [fn]) -> loadFile fn l
        -- | Validate the current solution
        Just (":v", _) -> undefined
        -- | Reset the current solution, but not the underlying puzzle
        Just (":r", _) -> undefined
        -- | Attempt to solve the current puzzle, starting with the current solution
        Just (":s", _) -> undefined
        -- | Print out the current puzzle as an ASCII grid
        Just (":p", [arg]) -> undefined
        -- | Add a value to the current solution
        Just (":a", args) -> undefined
        -- | Delete a value from the current solution
        Just (":d", args) -> undefined
        -- | Quit the looper
        Just (":q", _) -> return ()
        _ -> do
          putStrLn ("Unknown command \"" <> str <> "\"")
          go l

-- | Load a new puzzle file and reset the looper
loadFile :: String -> Looper -> IO ()
loadFile fn l = undefined