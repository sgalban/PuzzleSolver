module PuzzleEvaluator where
import PuzzleSyntax qualified as PS
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set ((\\))
import Data.Bifunctor (second, bimap)
import Control.Monad.Except ( MonadError(throwError), ExceptT, runExceptT )
import Control.Monad.State ( StateT, MonadState(get), modify, runStateT)
import Control.Monad.Identity (Identity, runIdentity, IdentityT (runIdentityT))
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import qualified Test.QuickCheck as QC
import Test.QuickCheck (Arbitrary)
import Control.Monad (liftM2)
import Data.Either (isRight, fromRight)

-- | A simplified representation of a puzzle containing the actual constraints
-- | and the raw collection of cells they affect
-- | Essentially the product of evaluating Puzzle code
data PuzzleE = PE {
    width :: Int,
    height :: Int,
    constraints :: Set.Set ConstraintE
} deriving (Show, Eq)

-- | An evaluated constraint rule
data ConstraintE = CE {
    constraintType :: ConstraintEType,
    cells :: Set.Set (Int, Int)
} deriving (Show, Eq, Ord)

-- | A basic constraint type
-- | Similar to Puzzle.PrimitiveConstraint, but expressions are evaluated and
-- | it bundles in CellInit
data ConstraintEType
  = Unique (Int, Int)
  | AddsTo Int
  | MultsTo Int
  | GreaterThan Int Int
  | LessThan Int Int
  | Always
  | Never
  | Value Int
  deriving (Show, Eq, Ord)

-- | An error that can be thrown during evaluation
data EvalError
  = InvalidVar Int
  | DivByZero
  | ZeroExponent
  | NegativeNumber
  | InvalidRange
  | OutOfBounds
  deriving (Show, Eq)

getErrorString :: EvalError -> String
getErrorString err = "Evaluation Error: " ++ errString err
  where
    errString (InvalidVar v) = "Illegal repeat variable (" ++ show v ++ ")"
    errString DivByZero = "Division by zero"
    errString ZeroExponent = "Zero exponent"
    errString NegativeNumber = "Negative number"
    errString InvalidRange = "Invaid range"
    errString OutOfBounds = "Cell group out of bounds"

-- | The puzzle context at a given point of evaluation.
-- | This includes all repeat variables (if we're inside of a repeat), and also
-- | all constrained cells, which we must keep track of for in order to properly
-- | evaluate the `unconstrained` cell group
data Context = Context {
  repeatVars :: Map.Map Int Int,
  constrainedCells :: Set.Set (Int, Int),
  repeatDepth :: Int,
  puzzleWidth :: Int,
  puzzleHeight :: Int
}

-- | The output of an evaluation
type PEval a = ExceptT EvalError (StateT Context Identity) a

-- | Convenince function to make a singleton set
ss :: a -> Set.Set a
ss = Set.singleton

-- | Generates the set of all possible cell coordinates for a grid with the
-- | given width and height
allCells :: Int -> Int -> Set.Set (Int, Int)
allCells w h = Set.fromList [(r, c) | r <- [0..(h - 1)], c <- [0..(w - 1)]]

-- | Evaluate a repeat var. Throws if we're not a repeat rule
evalRepeatVar :: Int -> PEval Int
evalRepeatVar var = do
  ctx <- get
  case repeatVars ctx Map.!? var of
    Nothing -> throwError (InvalidVar var)
    Just val -> if val < 0
      then throwError NegativeNumber
      else return val

-- | Evaluate a numerical expression
evalNumExp :: PS.NumExp -> PEval Int
evalNumExp (PS.Number val) = if val < 0
  then throwError NegativeNumber
  else return val
evalNumExp (PS.RepeatVar var) = evalRepeatVar var
evalNumExp (PS.Op2 exp1 bop exp2) = do
  val1 <- evalNumExp exp1
  val2 <- evalNumExp exp2
  evalBop val1 bop val2

-- | Evaluate a binary operation
evalBop :: Int -> PS.Bop -> Int -> PEval Int
evalBop val1 PS.Plus val2 = return $ val1 + val2
evalBop val1 PS.Minus val2 = if val1 < val2
  then throwError NegativeNumber
  else return $ val1 - val2
evalBop val1 PS.Times val2 = return $ val1 * val2
evalBop val1 PS.Divide val2 = if val2 == 0
  then throwError DivByZero
  else return $ val1 `div` val2
evalBop val1 PS.Modulo val2 = if val2 == 0
  then throwError DivByZero
  else return $ val1 `mod` val2
evalBop val1 PS.Power val2 = if val1 == 0 && val2 == 0
  then throwError ZeroExponent
  else return $ val1 ^ val2

-- | Evaluates a range, converting it into an Int pair.
-- | Fails if the upper bound is less than the lower bound.
evalRange :: PS.Range -> PEval (Int, Int)
evalRange (PS.Range (lExp, uExp)) = do
  lower <- evalNumExp lExp
  upper <- evalNumExp uExp
  if upper < lower
    then throwError InvalidRange
    else return (lower, upper)

-- | Evaluates a cell group, converting it into a set of cells, and updating
-- | the context. Can throw an OutOfBounds error if the cell group falls outside
-- | of the grid as defined by the context
evalCellGroup :: PS.CellGroup -> PEval (Set.Set (Int, Int))
evalCellGroup cg = do
  ctx <- get
  let output = go cg ctx
  (newCells, _) <- output
  modify (addConstrainedCells newCells)
  fst <$> output
  where
    isOob row col ctx = row >= puzzleHeight ctx || col >= puzzleWidth ctx
    addConstrainedCells newCells ctx =
      ctx { constrainedCells = constrainedCells ctx <> newCells}
    -- | Note that while `go` may alter the context in a persistent manner, i.e.
    -- | for recursive calls, it should never actually modifiy the state of the
    -- | transformer. This can cause problems regarding the interaction between
    -- | `CellList`, `Inverse`, and `Unconstrained`
    go :: PS.CellGroup -> Context -> PEval (Set.Set (Int, Int), Context)
    go (PS.ACell rowExp colExp) ctx = do
      row <- evalNumExp rowExp
      col <- evalNumExp colExp
      if isOob row col ctx
        then throwError OutOfBounds
        else return (ss (row, col), ctx)

    go (PS.CellList []) ctx = return (Set.empty, ctx)

    go (PS.CellList (x : xs)) ctx = do
        (headCells, ctx') <- go x ctx
        (tailCells, ctx'') <- go (PS.CellList xs) ctx'
        return (headCells <> tailCells, ctx'')

    go (PS.Subgrid rowExp colExp wExp hExp) ctx = do
      startRow <- evalNumExp rowExp
      startCol <- evalNumExp colExp
      w <- evalNumExp wExp
      h <- evalNumExp hExp
      let endRow = startRow + h - 1
      let endCol = startCol + w - 1
      let rows = [startRow..endRow]
      let cols = [startCol..endCol]
      if isOob endRow endCol ctx || isOob startRow startCol ctx
        then throwError OutOfBounds
        else return (Set.fromList [(r, c) | r <- rows, c <- cols], ctx)

    go (PS.Row rowExp Nothing) ctx = do
      row <- evalNumExp rowExp
      let cols = [0..(puzzleWidth ctx - 1)]
      if isOob row 0 ctx
        then throwError OutOfBounds
        else return (Set.fromList [(row, c) | c <- cols], ctx)

    go (PS.Row rowExp (Just colRange)) ctx = do
      row <- evalNumExp rowExp
      (colStart, colEnd) <- evalRange colRange
      let cols = [colStart..colEnd]
      if isOob row colEnd ctx
        then throwError OutOfBounds
        else return (Set.fromList [(row, c) | c <- cols], ctx)

    go (PS.Col colExp Nothing) ctx = do
      col <- evalNumExp colExp
      let rows = [0..(puzzleHeight ctx - 1)]
      if isOob 0 col ctx
        then throwError OutOfBounds
        else return (Set.fromList [(r, col) | r <- rows], ctx)

    go (PS.Col colExp (Just rowRange)) ctx = do
      col <- evalNumExp colExp
      (rowStart, rowEnd) <- evalRange rowRange
      let rows = [rowStart..rowEnd]
      if isOob rowEnd col ctx
        then throwError OutOfBounds
        else return (Set.fromList [(r, col) | r <- rows], ctx)

    go PS.All ctx = do
      let cells = allCells (puzzleWidth ctx) (puzzleHeight ctx)
      return (cells, ctx)

    go PS.Unconstrained ctx = do
      let constrained = constrainedCells ctx
      (allCells, ctx') <- go PS.All ctx
      return (allCells Set.\\ constrained, ctx')

    go (PS.Inverse cg) ctx = do
      (cells, ctx') <- go cg ctx
      (allCells, ctx'') <- go PS.All ctx'
      return (allCells Set.\\ cells, ctx'')

-- | Maps primitive constraints to their SimpleConstraint counterpart,
-- | evaluating numerical expressions in the process
evalPrimitiveConstraint :: PS.PrimitiveConstraint -> PEval ConstraintEType
evalPrimitiveConstraint (PS.Unique range) = do
  range' <- evalRange range
  return $ Unique range'
evalPrimitiveConstraint (PS.AddsTo valExp) = do
  val <- evalNumExp valExp
  return $ AddsTo val
evalPrimitiveConstraint (PS.MultsTo valExp) = do
  val <- evalNumExp valExp
  return $ MultsTo val
evalPrimitiveConstraint (PS.GreaterThan rowExp colExp) = do
  row <- evalNumExp rowExp
  col <- evalNumExp colExp
  return $ GreaterThan row col
evalPrimitiveConstraint (PS.LessThan rowExp colExp) = do
  row <- evalNumExp rowExp
  col <- evalNumExp colExp
  return $ LessThan row col
evalPrimitiveConstraint PS.Always = return Always
evalPrimitiveConstraint PS.Never = return Never

-- | Evaluates ConstrainedCells into a list of constraint/cellgroup pairs
-- | The evaluated list is empty if there are no constraints or no cells
evalConstrainedCells :: PS.ConstrainedCells -> PEval (Set.Set ConstraintE)
evalConstrainedCells (PS.ConstrainedCells (PS.PC pc) cg) = do
  cells <- evalCellGroup cg
  cType <- evalPrimitiveConstraint pc
  if null cells
    then return Set.empty
    else return $ ss (CE cType cells)
evalConstrainedCells (PS.ConstrainedCells (PS.ConstraintList cl) cg) = do
  cells <- evalCellGroup cg
  typeList <- mapM evalPrimitiveConstraint cl
  if null cells || null cl
    then return Set.empty
    else return ((`CE` cells) `Set.map` Set.fromList typeList)

-- | Evaluates a single ConstraintRule into any number of SimpleConstraints
evalConstraintRule :: PS.ConstraintRule -> PEval (Set.Set ConstraintE)
evalConstraintRule (PS.CellInit cg valExp) = do
  cells <- evalCellGroup cg
  val <- evalNumExp valExp
  if Set.null cells
    then return Set.empty
    else return $ ss (CE (Value val) cells)
evalConstraintRule (PS.CC ccs) = evalConstrainedCells ccs
evalConstraintRule (PS.Repeat range cr) = do
  (start, end) <- evalRange range
  modify (\ctx -> ctx {
    repeatDepth = repeatDepth ctx + 1
  })
  res <- evalRepeat start end cr
  modify (\ctx -> ctx {
    repeatDepth = repeatDepth ctx - 1
  })
  return res
  where
    evalRepeat :: Int -> Int -> PS.ConstraintRule -> PEval (Set.Set ConstraintE)
    evalRepeat start end cr = if start > end
      then return Set.empty
      else do
        ctx <- get
        let var = repeatDepth ctx - 1
        modify (\ctx -> ctx {
          repeatVars = Map.insert var start (repeatVars ctx)
        })
        nextScs <- evalConstraintRule cr
        restScs <- evalRepeat (start + 1) end cr
        modify (\ctx -> ctx {
          repeatVars = Map.delete var (repeatVars ctx)
        })
        return $ nextScs <> restScs

-- | Sets the initial context for a puzzle
initialContext :: PS.PuzzleSyntax -> Context
initialContext (PS.Grid w h _) = Context {
  repeatVars = Map.empty,
  constrainedCells = Set.empty,
  repeatDepth = 0,
  puzzleWidth = w,
  puzzleHeight = h
}

-- | Used for testing
emptyContext :: Context
emptyContext = Context {
  repeatVars = Map.empty,
  constrainedCells = Set.empty,
  repeatDepth = 0,
  puzzleWidth = 0,
  puzzleHeight = 0
}

-- | Runs an evaluation from a context
runEval :: Context -> PEval a -> Either EvalError a
runEval ctx ev = fst $ runIdentity (runStateT (runExceptT ev) ctx)

-- | Converts a Puzzle (syntax) into a fully evaluated puzzle
evaluatePuzzle :: PS.PuzzleSyntax -> Either EvalError PuzzleE
evaluatePuzzle p@(PS.Grid w h rules) = let
  initCtx = initialContext p;
  result = runEval initCtx (evaluateRules rules)
  in PE w h <$> result
  where
    rules = PS.constraints p
    evaluateRules :: [PS.ConstraintRule] -> PEval (Set.Set ConstraintE)
    evaluateRules [] = return Set.empty
    evaluateRules (cr : crs) = do
      headRules <- evalConstraintRule cr
      tailRules <- evaluateRules crs
      return (headRules <> tailRules)

-- | A map from cell coordinates to constraints the cell is affected by.
-- | Keys are usually in the cell groups of the constraints in the value set,
-- | but not always
type CellConstraints = Map.Map (Int, Int) (Set.Set ConstraintE)

-- | Creates a mapping from every cell in the grid to the set of constraints
-- | that affect them
cellConstraints :: PuzzleE -> CellConstraints
cellConstraints (PE w h cs) = Map.unionWith (<>) inverses extras
  where
    inverses = foldr invertConstraint initMap cs
    initMap = Map.fromList [(k, Set.empty) | k <- Set.toList (allCells w h)]
    invertConstraint con acc = foldr (addCellToMap con) acc (cells con)
    addCellToMap con cell = Map.insertWith (<>) cell (ss con)
    extras = foldMap addExtras cs
    -- | Some constraints affect cells outside their cell group
    addExtras con@(CE (GreaterThan r c) _) = Map.singleton (r, c) (ss con)
    addExtras con@(CE (LessThan r c) _) = Map.singleton (r, c) (ss con)
    addExtras _ = Map.empty

-- | Evaluation Tests

testEvalNumExp :: Test
testEvalNumExp =
  "testEvalNumExp"
    ~: TestList [
      run (PS.Number 3) ~?= Right 3,
      run (PS.Op2 (PS.Number 2) PS.Plus (PS.Number 3)) ~?= Right 5,
      run (PS.Op2 (PS.Number 3) PS.Minus (PS.Number 2)) ~?= Right 1,
      run (PS.Op2 (PS.Number 3) PS.Times (PS.Number 2)) ~?= Right 6,
      run (PS.Op2 (PS.Number 10) PS.Divide (PS.Number 3)) ~?= Right 3,
      run (PS.Op2 (PS.Number 10) PS.Modulo (PS.Number 3)) ~?= Right 1,
      run (PS.Op2 (PS.Number 2) PS.Power (PS.Number 3)) ~?= Right 8,
      run (PS.Number (-3)) ~?= Left NegativeNumber,
      run (PS.Op2 (PS.Number 2) PS.Minus (PS.Number 3)) ~?= Left NegativeNumber,
      run (PS.Op2 (PS.Number 3) PS.Divide (PS.Number 0)) ~?= Left DivByZero,
      run (PS.Op2 (PS.Number 3) PS.Modulo (PS.Number 0)) ~?= Left DivByZero,
      run (PS.Op2 (PS.Number 0) PS.Power (PS.Number 0)) ~?= Left ZeroExponent,
      run (PS.Op2 (PS.Number 0) PS.Power (PS.RepeatVar 0)) ~?= Left (InvalidVar 0),
      runEval repeatContext (evalNumExp (PS.RepeatVar 0)) ~?= Right 3
    ]
    where
      run numExp = runEval emptyContext (evalNumExp numExp)
      repeatContext = emptyContext { repeatDepth = 1, repeatVars = Map.singleton 0 3 }

-- >>> runTestTT testEvalNumExp
-- Counts {cases = 14, tried = 14, errors = 0, failures = 0}

testEvalRange :: Test
testEvalRange =
  "testEvalRange"
    ~: TestList [
      run (PS.Range (PS.Number 3, PS.Number 6)) ~?= Right (3, 6),
      run (PS.Range
        (PS.Op2 (PS.Number 3) PS.Plus (PS.Number 4),
         PS.Op2 (PS.Number 3) PS.Times (PS.Number 4))) ~?= Right (7, 12),
      run (PS.Range (PS.Number 5, PS.Number 2)) ~?= Left InvalidRange
    ]
    where
      run range = runEval emptyContext (evalRange range)

-- >>> runTestTT testEvalRange
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

testEvalCellGroup :: Test
testEvalCellGroup =
  "testEvalCellGroup"
  ~: TestList [
    run (PS.ACell (num 3) (num 3)) ~?= Right (ss (3, 3)),
    run (PS.ACell (num 3) (num 30)) ~?= Left OutOfBounds,
    run (PS.ACell (num (-3)) (num 3)) ~?= Left NegativeNumber,
    run (PS.Row (num 3) Nothing) ~?= Right (Set.fromList [(3, c) | c <- [0..8]]),
    run (PS.Col (num 3) Nothing) ~?= Right (Set.fromList [(r, 3) | r <- [0..8]]),
    run (PS.Row (num 3) (Just $ PS.Range (num 2, num 5))) ~?= Right (Set.fromList [(3, c) | c <- [2..5]]),
    run (PS.Col (num 3) (Just $ PS.Range (num 2, num 5))) ~?= Right (Set.fromList [(r, 3) | r <- [2..5]]),
    run (PS.CellList []) ~?= Right Set.empty,
    run (PS.CellList [PS.ACell (num 6) (num 6), PS.Col (num 3) (Just $ PS.Range (num 2, num 5))]) ~?=
      Right (ss (6, 6) <> Set.fromList [(r, 3) | r <- [2..5]]),
    run (PS.Subgrid (num 1) (num 2) (num 3) (num 4)) ~?=
      Right (Set.fromList [(r, c) | r <- [1..4], c <- [2..4]]),
      run (PS.Subgrid (num 1) (num 2) (num 9) (num 4)) ~?= Left OutOfBounds,
      run (PS.Subgrid (num 1) (num 2) (num 3) (num 0)) ~?= Right Set.empty,
    run PS.All ~?= Right (Set.fromList [(r, c) | r <- [0..8], c <- [0..8]]),
    run (PS.Inverse PS.All) ~?= Right Set.empty,
    run (PS.Inverse (PS.ACell (num 3) (num 3))) ~?=
      Right (Set.fromList [(r, c) | r <- [0..8], c <- [0..8]] \\ ss (3, 3)),
    run PS.Unconstrained ~?=
      Right ((Set.fromList [(r, c) | r <- [0..8], c <- [0..8]]) \\ Set.fromList [(0, 0), (1, 1), (1, 2)]),
    run (PS.CellList [PS.Inverse (PS.Row (num 0) Nothing), PS.Unconstrained]) ~?=
       Right (Set.fromList [(r, c) | r <- [0..8], c <- [0..8]] \\ ss (0, 0))
  ]
  where
    num = PS.Number
    ctx = Context {
      puzzleWidth = 9,
      puzzleHeight = 9,
      repeatDepth = 0,
      repeatVars = Map.empty,
      constrainedCells = Set.fromList [(0, 0), (1, 1), (1, 2)]
    }
    run cg = runEval ctx (evalCellGroup cg)

-- >>> runTestTT testEvalCellGroup
-- Counts {cases = 17, tried = 17, errors = 0, failures = 0}

testEvalPrimitiveConstraint :: Test
testEvalPrimitiveConstraint =
  "testEvalPrimitiveConstraint (and CellInit)"
  ~: TestList [
    run (PS.CellInit cell23 (num 1)) ~?=
      Right (ss (CE (Value 1) (ss (2, 3)))),
    run (PS.CC (PS.ConstrainedCells (PS.PC $ PS.Unique (PS.Range (num 1, num 2))) cell23)) ~?=
      Right (ss (CE (Unique (1, 2)) (ss (2, 3)))),
    run (PS.CC (PS.ConstrainedCells (PS.PC $ PS.AddsTo (num 3)) cell23)) ~?=
      Right (ss (CE (AddsTo 3) (ss (2, 3)))),
    run (PS.CC (PS.ConstrainedCells (PS.PC $ PS.MultsTo (num 3)) cell23)) ~?=
      Right (ss (CE (MultsTo 3) (ss (2, 3)))),
    run (PS.CC (PS.ConstrainedCells (PS.PC $ PS.GreaterThan (num 1) (num 2)) cell23)) ~?=
      Right (ss (CE (GreaterThan 1 2) (ss (2, 3)))),
    run (PS.CC (PS.ConstrainedCells (PS.PC $ PS.LessThan (num 1) (num 2)) cell23)) ~?=
      Right (ss (CE (LessThan 1 2) (ss (2, 3)))),
    run (PS.CC (PS.ConstrainedCells (PS.PC PS.Always) cell23)) ~?=
      Right (ss (CE Always (ss (2, 3)))),
    run (PS.CC (PS.ConstrainedCells (PS.PC PS.Never) cell23)) ~?=
      Right (ss (CE Never (ss (2, 3)))),
    run (PS.CC (PS.ConstrainedCells (PS.PC PS.Never) cell23)) ~?=
      Right (ss (CE Never (ss (2, 3)))),
    run (PS.CC (PS.ConstrainedCells (PS.PC PS.Always) (PS.CellList []))) ~?= Right Set.empty
  ]
  where
    ctx = emptyContext { puzzleWidth = 9, puzzleHeight = 9 }
    run cr = runEval ctx (evalConstraintRule cr)
    num = PS.Number
    cell23 = PS.ACell (num 2) (num 3)

-- >>> runTestTT testEvalPrimitiveConstraint
-- Counts {cases = 10, tried = 10, errors = 0, failures = 0}

testEvalConstraintList :: Test
testEvalConstraintList =
  "testEvalConstraintList"
  ~: TestList [
    run (PS.CC (PS.ConstrainedCells (PS.ConstraintList []) cell23)) ~?= Right Set.empty,
    run (PS.CC (PS.ConstrainedCells (PS.ConstraintList [PS.Always]) (PS.CellList []))) ~?=
      Right Set.empty,
    run (PS.CC (PS.ConstrainedCells (PS.ConstraintList [PS.Always]) cell23)) ~?=
      Right (ss (CE Always (Set.singleton (2, 3)))),
    run (PS.CC (PS.ConstrainedCells (PS.ConstraintList [PS.Always, PS.Never]) cell23)) ~?=
      Right (Set.fromList [CE Always (Set.singleton (2, 3)), CE Never (Set.singleton (2, 3))])
  ]
  where
    ctx = emptyContext { puzzleWidth = 9, puzzleHeight = 9 }
    run cr = runEval ctx (evalConstraintRule cr)
    num = PS.Number
    cell23 = PS.ACell (num 2) (num 3)

-- >>> runTestTT testEvalConstraintList
-- Counts {cases = 4, tried = 4, errors = 0, failures = 0}

testEvalRepeat :: Test
testEvalRepeat =
  "testEvalRepeat"
  ~: TestList [
    run (repeat 1 0 (PS.CellInit (PS.ACell (num 2) (num 3)) rv0)) ~?=
      Left InvalidRange,
    run (repeat 1 1 (PS.CellInit (PS.ACell (num 2) (num 3)) rv0)) ~?=
      Right (ss (CE (Value 1) (Set.singleton (2, 3)))),
    run (repeat 1 3 (PS.CellInit (PS.ACell rv0 rv0) rv0)) ~?=
      Right (Set.fromList [CE (Value 1) (Set.singleton (1, 1)),
             CE (Value 2) (Set.singleton (2, 2)),
             CE (Value 3) (Set.singleton (3, 3))]),
    run (repeat 1 2 (PS.Repeat (PS.Range (num 0, rv0)) (PS.CellInit (PS.ACell rv0 rv1) rv0))) ~?=
      Right (Set.fromList [CE (Value 1) (Set.singleton (1, 0)),
             CE (Value 1) (Set.singleton (1, 1)),
             CE (Value 2) (Set.singleton (2, 0)),
             CE (Value 2) (Set.singleton (2, 1)),
             CE (Value 2) (Set.singleton (2, 2))])
  ]
  where
    ctx = emptyContext { puzzleWidth = 9, puzzleHeight = 9 }
    run cr = runEval ctx (evalConstraintRule cr)
    num = PS.Number
    rv0 = PS.RepeatVar 0
    rv1 = PS.RepeatVar 1
    range s e = PS.Range (num s, num e)
    repeat :: Int -> Int -> PS.ConstraintRule -> PS.ConstraintRule
    repeat s e = PS.Repeat (range s e)

-- >>> runTestTT testEvalRepeat
-- Counts {cases = 4, tried = 4, errors = 0, failures = 0}

eEmpty :: PuzzleE
eEmpty  = PE 5 5 Set.empty

eSudSmall :: PuzzleE
eSudSmall = PE 4 4 $ Set.fromList [
  CE (Unique (1, 4)) (Set.fromList [(0, c) | c <- [0..3]]),
  CE (Unique (1, 4)) (Set.fromList [(1, c) | c <- [0..3]]),
  CE (Unique (1, 4)) (Set.fromList [(2, c) | c <- [0..3]]),
  CE (Unique (1, 4)) (Set.fromList [(3, c) | c <- [0..3]]),

  CE (Unique (1, 4)) (Set.fromList [(r, 0) | r <- [0..3]]),
  CE (Unique (1, 4)) (Set.fromList [(r, 1) | r <- [0..3]]),
  CE (Unique (1, 4)) (Set.fromList [(r, 2) | r <- [0..3]]),
  CE (Unique (1, 4)) (Set.fromList [(r, 3) | r <- [0..3]]),

  CE (Unique (1, 4)) (Set.fromList [(0, 0), (0, 1), (1, 0), (1, 1)]),
  CE (Unique (1, 4)) (Set.fromList [(2, 0), (2, 1), (3, 0), (3, 1)]),
  CE (Unique (1, 4)) (Set.fromList [(0, 2), (0, 3), (1, 2), (1, 3)]),
  CE (Unique (1, 4)) (Set.fromList [(2, 2), (2, 3), (3, 2), (3, 3)]),

  CE (Value 3) (ss (0, 3)),
  CE (Value 4) (ss (1, 1)),
  CE (Value 3) (ss (2, 2)),
  CE (Value 2) (ss (2, 3))]

eMagSquare :: PuzzleE
eMagSquare = PE 3 3 $ Set.fromList [
  CE (Unique (1, 9)) (Set.fromList [(r, c) | r <- [0..2], c <- [0..2]]),

  CE (AddsTo 15) (Set.fromList [(0, c) | c <- [0..2]]),
  CE (AddsTo 15) (Set.fromList [(1, c) | c <- [0..2]]),
  CE (AddsTo 15) (Set.fromList [(2, c) | c <- [0..2]]),

  CE (AddsTo 15) (Set.fromList [(r, 0) | r <- [0..2]]),
  CE (AddsTo 15) (Set.fromList [(r, 1) | r <- [0..2]]),
  CE (AddsTo 15) (Set.fromList [(r, 2) | r <- [0..2]]),

  CE (AddsTo 15) (Set.fromList [(v, v) | v <- [0..2]]),
  CE (AddsTo 15) (Set.fromList [(v, 2 - v) | v <- [0..2]]),

  CE (Value 9) (ss (0, 1)),
  CE (Value 7) (ss (1, 0)),
  CE (Value 3) (ss (1, 2)),
  CE (Value 8) (ss (2, 2))]

eKakSmall :: PuzzleE
eKakSmall = PE 4 4 $ Set.fromList [
  CE (Unique (1, 9)) (Set.fromList [(0, c) | c <- [0..2]]),
  CE (AddsTo 15) (Set.fromList [(0, c) | c <- [0..2]]),

  CE (Unique (1, 9)) (Set.fromList [(1, c) | c <- [0..2]]),
  CE (AddsTo 8) (Set.fromList [(1, c) | c <- [0..2]]),

  CE (Unique (1, 9)) (Set.fromList [(2, c) | c <- [1..3]]),
  CE (AddsTo 15) (Set.fromList [(2, c) | c <- [1..3]]),

  CE (Unique (1, 9)) (Set.fromList [(3, c) | c <- [1..3]]),
  CE (AddsTo 19) (Set.fromList [(3, c) | c <- [1..3]]),

  CE (Unique (1, 9)) (Set.fromList [(r, 0) | r <- [0..1]]),
  CE (AddsTo 11) (Set.fromList [(r, 0) | r <- [0..1]]),

  CE (Unique (1, 9)) (Set.fromList [(r, 1) | r <- [0..3]]),
  CE (AddsTo 16) (Set.fromList [(r, 1) | r <- [0..3]]),

  CE (Unique (1, 9)) (Set.fromList [(r, 2) | r <- [0..3]]),
  CE (AddsTo 17) (Set.fromList [(r, 2) | r <- [0..3]]),

  CE (Unique (1, 9)) (Set.fromList [(r, 3) | r <- [2..3]]),
  CE (AddsTo 13) (Set.fromList [(r, 3) | r <- [2..3]]),

  CE (Value 3) (ss (0, 2)),
  CE (Value 1) (ss (1, 1)),
  CE (Value 2) (ss (2, 2)),
  CE (Value 2) (ss (3, 1)),

  CE (Value 0) (Set.fromList [(0, 3), (1, 3), (2, 0), (3, 0)])]

eFutoshiki :: PuzzleE
eFutoshiki = PE 5 5 $ Set.fromList [
  CE (Unique (1, 5)) (Set.fromList [(0, 0), (0, 1), (0, 2), (0, 3), (0, 4)]),
  CE (Unique (1, 5)) (Set.fromList [(1, 0), (1, 1), (1, 2), (1, 3), (1, 4)]),
  CE (Unique (1, 5)) (Set.fromList [(2, 0), (2, 1), (2, 2), (2, 3), (2, 4)]),
  CE (Unique (1, 5)) (Set.fromList [(3, 0), (3, 1), (3, 2), (3, 3), (3, 4)]),
  CE (Unique (1, 5)) (Set.fromList [(4, 0), (4, 1), (4, 2), (4, 3), (4, 4)]),

  CE (Unique (1, 5)) (Set.fromList [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0)]),
  CE (Unique (1, 5)) (Set.fromList [(0, 1), (1, 1), (2, 1), (3, 1), (4, 1)]),
  CE (Unique (1, 5)) (Set.fromList [(0, 2), (1, 2), (2, 2), (3, 2), (4, 2)]),
  CE (Unique (1, 5)) (Set.fromList [(0, 3), (1, 3), (2, 3), (3, 3), (4, 3)]),
  CE (Unique (1, 5)) (Set.fromList [(0, 4), (1, 4), (2, 4), (3, 4), (4, 4)]),

  CE (GreaterThan 0 1) (ss (0, 0)),
  CE (GreaterThan 0 3) (ss (0, 2)),
  CE (GreaterThan 0 4) (ss (0, 3)),
  CE (LessThan 3 4) (ss (3, 3)),
  CE (LessThan 4 1) (ss (4, 0)),
  CE (LessThan 4 2) (ss (4, 1)),

  CE (Value 4) (ss (1, 0)),
  CE (Value 2) (ss (1, 4)),
  CE (Value 4) (ss (2, 2)),
  CE (Value 4) (ss (3, 4))]

eKenKen :: PuzzleE
eKenKen = PE 4 4 $ Set.fromList [
  CE (Unique (1, 4)) (Set.fromList [(0, c) | c <- [0..3]]),
  CE (Unique (1, 4)) (Set.fromList [(1, c) | c <- [0..3]]),
  CE (Unique (1, 4)) (Set.fromList [(2, c) | c <- [0..3]]),
  CE (Unique (1, 4)) (Set.fromList [(3, c) | c <- [0..3]]),

  CE (Unique (1, 4)) (Set.fromList [(r, 0) | r <- [0..3]]),
  CE (Unique (1, 4)) (Set.fromList [(r, 1) | r <- [0..3]]),
  CE (Unique (1, 4)) (Set.fromList [(r, 2) | r <- [0..3]]),
  CE (Unique (1, 4)) (Set.fromList [(r, 3) | r <- [0..3]]),

  CE (AddsTo 5) (Set.fromList [(0, 0), (0, 1)]),
  CE (MultsTo 96) (Set.fromList [(0, 2), (0, 3), (1, 2), (1, 3), (2, 2)]),
  CE (MultsTo 12) (Set.fromList [(1, 0), (1, 1), (2, 0)]),
  CE (AddsTo 10) (Set.fromList [(2, 1), (3, 1), (3, 2), (3, 3), (2, 3)])]


testEvalPuzzle :: Test
testEvalPuzzle =
  "testEvalPuzzle"
  ~: TestList [
    evaluatePuzzle PS.pEmpty ~?= Right eEmpty,
    evaluatePuzzle PS.pSudSmall ~?= Right eSudSmall,
    evaluatePuzzle PS.pMagSquare ~?= Right eMagSquare,
    evaluatePuzzle PS.pKakSmall ~?= Right eKakSmall,
    evaluatePuzzle PS.pFutoshiki ~?= Right eFutoshiki,
    evaluatePuzzle PS.pKenKen ~?= Right eKenKen
  ]

-- >>> runTestTT testEvalPuzzle
-- Counts {cases = 6, tried = 6, errors = 0, failures = 0}

testAll :: IO Counts
testAll = runTestTT $ TestList [
  testEvalNumExp,
  testEvalRange,
  testEvalCellGroup,
  testEvalPrimitiveConstraint,
  testEvalConstraintList,
  testEvalRepeat,
  testEvalPuzzle]

-- >>> testAll
-- Counts {cases = 55, tried = 55, errors = 0, failures = 0}

instance Arbitrary PuzzleE where
  arbitrary :: QC.Gen PuzzleE
  arbitrary = do
    w <- QC.choose (3, 5)
    h <- QC.choose (3, 5)
    cons <- Set.fromList <$> QC.resize 3 (QC.listOf (arbCons w h))
    return (PE w h cons)
    where
      arbCons w h = CE <$> QC.arbitrary <*> (Set.fromList <$> QC.resize 5 (QC.listOf (arbPair w h)))
      arbPair w h = liftM2 (,) (QC.choose (0, h - 1)) (QC.choose (0, w - 1))
      cells w h = QC.suchThat (QC.resize 5 $ QC.listOf (arbPair w h)) (not . null)

  shrink :: PuzzleE -> [PuzzleE]
  shrink pe = case Set.toList $ constraints pe of
    (x : xs) -> [PE (width pe) (height pe) (Set.fromList xs)]
    _ -> []

instance Arbitrary ConstraintEType where
  arbitrary :: QC.Gen ConstraintEType
  arbitrary = QC.oneof [
    liftM2 (curry Unique) arbInt arbInt,
    AddsTo <$> arbInt,
    MultsTo <$> arbInt,
    GreaterThan <$> arbInt <*> arbInt,
    LessThan <$> arbInt <*> arbInt,
    Value <$> arbInt]
    where
      arbInt = QC.choose (1, 9)

prop_noEmptyCellGroups :: PS.PuzzleSyntax -> QC.Property
prop_noEmptyCellGroups ps = isRight pe QC.==> not (any (Set.null . cells) (constraints pe'))
  where
    pe = evaluatePuzzle ps
    pe' = fromRight (PE 0 0 Set.empty) pe

prop_inverseConstraints :: PuzzleE -> QC.Property
prop_inverseConstraints pe = not (Set.null (constraints pe)) QC.==>
  Set.fromList (Map.keys cellMap) == cells' &&
  all (\con -> all (`hasCon` con) (cells con)) (constraints pe) &&
  all (\cell -> all (Set.member cell . cells) (cellMap Map.! cell) ) cells'
    where
      cells' = allCells (width pe) (height pe)
      cellMap = cellConstraints pe
      hasCon cell con = Set.member con (cellMap Map.! cell)

runTests :: IO ()
runTests = do
  _ <- testAll
  putStrLn "quickCheck prop_noEmptyCellGroups"
  QC.quickCheck prop_noEmptyCellGroups
  putStrLn "quickCheck prop_inverseConstraints"
  QC.quickCheck prop_inverseConstraints
