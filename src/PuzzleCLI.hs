module PuzzleCLI where
import PuzzleSyntax qualified as PSyn
import PuzzleSolver qualified as PSol
import PuzzleEvaluator qualified as PE
import PuzzleParser qualified as PP
import Data.Maybe (fromMaybe, isJust)
import Data.Map qualified as Map
import Data.List qualified as List
import PuzzlePrinter (printPuzzleE, printPuzzleSolution, prettyPrint)
import Text.Read (readMaybe)

data Looper = Looper
  { filename :: Maybe String,
    pSyn :: Maybe PSyn.PuzzleSyntax,
    pe :: Maybe PE.PuzzleE,
    pSol :: Maybe PSol.PuzzleSolution,
    prev :: Maybe Looper }

initialLooper :: Looper
initialLooper =
  Looper
    { filename = Nothing,
      pSyn = Nothing,
      pe = Nothing,
      pSol = Nothing,
      prev = Nothing }

initSol :: PE.PuzzleE -> PSol.PuzzleSolution
initSol pe = PSol.PuzzleSolution pe Map.empty

looper :: IO ()
looper = go initialLooper
  where
    printAndGo :: Looper -> String -> IO()
    printAndGo l msg = do
      putStrLn msg
      go l

    printNoPuzzleLoaded :: Looper -> IO ()
    printNoPuzzleLoaded l =
      printAndGo l "No puzzle loaded. Load a puzzle with 'load <filename>'"

    -- | Load a new puzzle file and reset the looper
    loadFile :: String -> Looper -> IO ()
    loadFile fn l = do
      res <- PP.parsePuzzleFromFile fn
      case res of
        Left err -> printAndGo l ("Error loading file: " ++ err)
        Right pSyn -> do
          putStrLn ("Loaded " ++ fn)
          case PE.evaluatePuzzle pSyn of
            Left err' -> printAndGo l (PE.getErrorString err')
            Right pe' -> go Looper {
              filename = Just fn,
              pSyn = Just pSyn,
              pe = Just pe',
              pSol = Just $ initSol pe',
              prev = Just l
            }

    within :: Int -> Int -> Int -> Bool
    within x y z = z >= x && z <= y
    
    getCellArgs :: Int -> Int -> String -> String -> String -> IO (Maybe (Int, Int, Int))
    getCellArgs w h r c val = let
      args = do
        r' <- readMaybe r
        c' <- readMaybe c
        val' <- readMaybe val
        return (r', c', val')
      in case args of
        Nothing -> do
          putStrLn "Invalid arguments: Arguments must be positive integers" 
          return Nothing
        Just (r', c', val') -> if within 0 (h - 1) r' && within 0 (w - 1) c'
          then return args
          else do
            putStrLn "Invalid arguments: Cell is out of bounds" 
            return Nothing

    go :: Looper -> IO ()
    go l = do
      putStr (fromMaybe "" (filename l) ++ "> ")
      str <- getLine
      case List.uncons (words str) of
        -- | Load a new puzzle file and reset the looper
        Just ("load", fn : _) -> loadFile fn l
        Just ("load", []) -> printAndGo l "A filename must be provided"

        -- | Validate the current solution
        Just ("validate", _) -> case pSol l of
          Nothing -> printNoPuzzleLoaded l
          Just pSol' -> if PSol.validate pSol'
            then printAndGo l "Current solution is VALID"
            else printAndGo l "Current solution is INVALID"

        -- | Reset the current solution, but not the underlying puzzle
        Just ("reset", _) -> case pe l of
          Nothing -> printNoPuzzleLoaded l
          Just pe' -> go $ l { pSol = Just $ initSol pe', prev = Just l }

        -- | Attempt to solve the current puzzle, starting with the current solution
        Just ("solve", _) -> case pe l of
          Nothing -> printNoPuzzleLoaded l
          Just pe' -> case PSol.solveE pe' of
            Nothing -> printAndGo l "Unable to solve puzzle"
            Just ps' -> go $ l { pSol = Just ps', prev = Just l }
            
        -- | Print out the current loaded puzzle code
        Just ("print", _) -> case pSyn l of
          Nothing -> printNoPuzzleLoaded l
          Just pSyn' -> printAndGo l (prettyPrint pSyn')
          
        -- | Print out the current puzzle as an ASCII grid
        Just ("show-puzzle", _) -> case pe l of
          Nothing -> printNoPuzzleLoaded l
          Just pe' -> printAndGo l (printPuzzleE pe')

        -- | Print out the current solution as an ASCII grid
        Just ("show-solution", _) -> case pSol l of
          Nothing -> printNoPuzzleLoaded l
          Just pSol' -> printAndGo l (printPuzzleSolution pSol')

        -- | Add a value to the current solution
        Just ("set", r : c : val : _) -> case pe l of
          Nothing -> printNoPuzzleLoaded l
          Just pe' -> do
            let w = PE.width pe'
            let h = PE.height pe'
            let sol = pSol l
            args <- getCellArgs w h r c val
            let newSol = case args of
                          Nothing -> Nothing
                          Just (r', c', val') -> do
                            sol' <- sol
                            PSol.putCellValue sol' (r', c') (val' `mod` 10)
            if isJust newSol
              then go (l { pSol = newSol, prev = Just l})
              else printAndGo l "There was an error setting the cell value"

        Just ("set", _) -> printAndGo l "Command takes 3 arguments: row, column, and value"

        -- | Delete a value from the current solution
        Just ("remove", r : c : _) -> case pe l of
          Nothing -> printNoPuzzleLoaded l
          Just pe' -> do
            let w = PE.width pe'
            let h = PE.height pe'
            let sol = pSol l
            args <- getCellArgs w h r c "0"
            let newSol = case args of
                          Nothing -> Nothing
                          Just (r', c', _) -> do
                            sol' <- sol
                            Just $ PSol.removeCellValue sol' (r', c')
            if isJust newSol
              then go (l { pSol = newSol, prev = Just l})
              else printAndGo l "There was an error deleting the cell value"
        Just ("remove", _) -> printAndGo l "Command takes 2 arguments: row and column"

        -- | Undo the last state-changing step

        Just ("undo", _) -> case prev l of
          Nothing -> printAndGo l "No action to undo"
          Just l' -> go l'

        Just ("help", _) -> printAndGo l $ List.intercalate "\n" [
          "help -- Displays this menu.",
          "load <filename> -- Loads a puzzle into the program state. This clears out any previous state.",
          "validate -- Determines if the current working solution violates any constraints.",
          "reset -- Clears the current solution without unloading the current puzzle.",
          "solve -- If possible, sets the current solution to a complete, valid one.",
          "print -- Prints the current loaded puzzle as Puzzle code.",
          "show-puzzle -- Prints the initial values of the loaded puzzle as an ASCII grid.",
          "show-solution -- Prints the current working solution as an ASCII grid.",
          "set <row> <col> <val> -- Sets the value at the coordinate (row, col) to val in the current solution.",
          "remove <row> <col> -- Removes the cell with coordinate (row, col) from the current solution.",
          "undo -- Undoes the last action that changed the program state.",
          "quit -- Exists the Puzzle Solver CLI." ]

        -- | Quit the looper
        Just ("quit", _) -> return ()
        _ -> do
          putStrLn ("Unknown command \"" ++ str ++ "\"")
          go l