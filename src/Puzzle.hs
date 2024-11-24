module Puzzle where
import Test.QuickCheck (Arbitrary)
import qualified Test.QuickCheck as QC
import Control.Monad (liftM2, liftM3)
import qualified Data.Map as Map

data NumExp
  = Number Int
  | Op2 NumExp Bop NumExp
  | RepeatVar Int
  deriving (Show, Eq)

data Bop
  = Plus
  | Minus
  | Times
  | Divide
  | Modulo
  | Power
  deriving (Show, Eq, Enum, Bounded)

newtype Range = Range (NumExp, NumExp) deriving (Show, Eq)

data Puzzle = Grid {
  width :: Int,
  height :: Int,
  constraints :: [ConstraintRule]
  } deriving(Show, Eq)

data ConstraintRule
  = CellInit CellGroup NumExp
  | CC ConstrainedCells
  | Repeat Range ConstraintRule
  deriving (Show, Eq)

data ConstrainedCells = ConstrainedCells {
  constraint :: Constraint,
  cellGroup :: CellGroup
  } deriving (Show, Eq)

data Constraint
  = PC PrimitiveConstraint
  | ConstraintList [PrimitiveConstraint]
  deriving (Show, Eq)

instance Semigroup Constraint where
  (<>) :: Constraint -> Constraint -> Constraint
  (PC Always) <> c = c
  c <> (PC Always) = c
  n@(PC Never) <> _ = n
  _ <> n@(PC Never) = n
  ConstraintList l1 <> ConstraintList l2 = ConstraintList $ l1 ++ l2
  ConstraintList l <> (PC p) = ConstraintList $ p : l
  (PC p) <> ConstraintList l = ConstraintList $ p : l
  (PC p1) <> (PC p2) = ConstraintList [p1, p2]

instance Monoid Constraint where
  mempty :: Constraint
  mempty = PC Never

data PrimitiveConstraint
  = Unique Range
  | AddsTo NumExp
  | MultsTo NumExp
  | GreaterThan NumExp NumExp
  | LessThan NumExp NumExp
  | Always
  | Never
  deriving (Show, Eq)

data CellGroup
  = ACell NumExp NumExp
  | CellList [CellGroup]
  | Subgrid NumExp NumExp NumExp NumExp
  | Row NumExp (Maybe Range)
  | Col NumExp (Maybe Range)
  | Inverse CellGroup
  | All
  | Unconstrained
  deriving (Show, Eq)

instance Semigroup CellGroup where
  (<>) :: CellGroup -> CellGroup -> CellGroup
  All <> _ = All
  _ <> All = All
  cg <> Inverse All = cg
  Inverse All <> cg = cg
  CellList l1 <> CellList l2 = CellList $ l1 ++ l2
  CellList l <> cg = CellList $ cg : l
  cg <> CellList l = CellList $ cg : l
  cg1 <> cg2 = CellList [cg1, cg2]

instance Monoid CellGroup where
  mempty :: CellGroup
  mempty = CellList []

isValidPuzzle :: Puzzle -> Bool
isValidPuzzle = undefined

data PuzzleSolution = PuzzleSolution {
  puzzle :: Puzzle,
  cellValues :: Map.Map (Int, Int) Int
  } deriving (Show, Eq)

-- QuickCheck instances

instance Arbitrary Bop where
  arbitrary :: QC.Gen Bop
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary NumExp where
  arbitrary :: QC.Gen NumExp
  arbitrary = QC.sized genExp
    where
      genExp 0 = QC.frequency [
        (1, return $ RepeatVar 0),
        (1, return $ RepeatVar 1),
        (2, Number <$> QC.arbitrary)]
      genExp n = QC.frequency [
        (2, genExp 0),
        (n, Op2 <$> genExp n' <*> QC.arbitrary <*> genExp n')]
          where
            n' = n `div` 2

instance Arbitrary Range where
  arbitrary :: QC.Gen Range
  arbitrary = liftM2 (curry Range) QC.arbitrary QC.arbitrary

instance Arbitrary PrimitiveConstraint where
  arbitrary :: QC.Gen PrimitiveConstraint
  arbitrary = QC.oneof [
    Unique <$> QC.arbitrary,
    AddsTo <$> QC.arbitrary,
    MultsTo <$> QC.arbitrary,
    GreaterThan <$> QC.arbitrary <*> QC.arbitrary,
    LessThan <$> QC.arbitrary <*> QC.arbitrary,
    return Always,
    return Never]

instance Arbitrary Constraint where
  arbitrary :: QC.Gen Constraint
  arbitrary = QC.sized genCon
    where
      genCon 0 = PC <$> QC.arbitrary
      genCon n = QC.oneof [
        genCon 0,
        ConstraintList <$> QC.listOf QC.arbitrary]

instance Arbitrary CellGroup where
  arbitrary :: QC.Gen CellGroup
  arbitrary = QC.oneof [
    ACell <$> QC.arbitrary <*> QC.arbitrary,
    CellList <$> QC.listOf QC.arbitrary,
    Subgrid <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary,
    Row <$> QC.arbitrary <*> rowColBoundGen,
    Col <$> QC.arbitrary <*> rowColBoundGen,
    Inverse <$> QC.arbitrary,
    return All,
    return Unconstrained]
    where
      rowColBoundGen :: QC.Gen (Maybe Range)
      rowColBoundGen = QC.oneof [
        Just <$> QC.arbitrary,
        return Nothing]

instance Arbitrary ConstrainedCells where
  arbitrary :: QC.Gen ConstrainedCells
  arbitrary = liftM2 ConstrainedCells QC.arbitrary QC.arbitrary

instance Arbitrary ConstraintRule where
  arbitrary :: QC.Gen ConstraintRule
  arbitrary = QC.sized genConRule
    where
      genConRule 0 = QC.oneof [
        CellInit <$> QC.arbitrary <*> QC.arbitrary,
        CC <$> QC.arbitrary]
      genConRule n = QC.frequency [
        (2, genConRule 0),
        (1, Repeat <$> QC.arbitrary <*> genConRule (n `div` 2))]

instance Arbitrary Puzzle where
  arbitrary :: QC.Gen Puzzle
  arbitrary = liftM3 Grid genDim genDim $ QC.listOf QC.arbitrary
    where
      genDim :: QC.Gen Int
      genDim = QC.suchThat QC.arbitrary (\x -> x > 1 && x < 10)

instance Arbitrary PuzzleSolution where
  arbitrary :: QC.Gen PuzzleSolution
  arbitrary = undefined

-- Sample Puzzles and Solutions

-- | Transforms a list of numbers into a grid of numbers, given the width of the
-- | grid. This will make it easier to define the hardcoded puzzle solutions
toSolutionMap :: Int -> [a] -> Map.Map (Int, Int) a
toSolutionMap cols values = Map.fromList (solList values)
  where
    solList :: [a] -> [((Int, Int), a)]
    solList = zipWith (\i v -> ((i `div` cols, i `mod` cols), v)) [0..]

-- | A couple of functions to make hardcoding puzzles less verbose

range :: Int -> Int -> Range
range a b = Range(Number a, Number b)

cell :: Int -> Int -> CellGroup
cell a b = ACell (Number a) (Number b)

-- | sudoku-small.pz
pSudSmall :: Puzzle
pSudSmall = Grid 4 4 [
  Repeat (range 0 3) (CC $ ConstrainedCells (PC $ Unique $ range 1 4) (Row (RepeatVar 0) Nothing)),
  Repeat (range 0 3) (CC $ ConstrainedCells (PC $ Unique $ range 1 4) (Col (RepeatVar 0) Nothing)),
  Repeat (range 0 1) (Repeat (range 0 1) (CC (ConstrainedCells (PC $ Unique $ range 1 4) (Subgrid (Op2 (RepeatVar 0) Times (Number 2)) (Op2 (RepeatVar 0) Times (Number 2)) (Number 2) (Number 2))))),
  CellInit (cell 0 3) (Number 3),
  CellInit (cell 1 1) (Number 4),
  CellInit (cell 2 2) (Number 3),
  CellInit (cell 2 3) (Number 2)]

sSudSmall :: PuzzleSolution
sSudSmall = PuzzleSolution pSudSmall $ toSolutionMap 4 [
  1, 2, 4, 3,
  3, 4, 2, 1,
  4, 1, 3, 2,
  2, 3, 1, 4]

-- | magicsquare.pz
pMagSquare :: Puzzle
pMagSquare = Grid 3 3 [
  CC $ ConstrainedCells (PC $ Unique (range 1 9)) All,
  Repeat (range 0 2) (CC $ ConstrainedCells (PC $ AddsTo $ Number 15) (Row (RepeatVar 0) Nothing)),
  Repeat (range 0 2) (CC $ ConstrainedCells (PC $ AddsTo $ Number 15) (Col (RepeatVar 0) Nothing)),
  CC $ ConstrainedCells (PC $ AddsTo $ Number 15) (CellList [cell 0 0, cell 1 1, cell 2 2]),
  CC $ ConstrainedCells (PC $ AddsTo $ Number 15) (CellList [cell 0 2, cell 1 1, cell 2 0]),
  CellInit (cell 0 1) (Number 9),
  CellInit (cell 1 0) (Number 7),
  CellInit (cell 1 2) (Number 3),
  CellInit (cell 2 2) (Number 8)]

sMagSquare :: PuzzleSolution
sMagSquare = PuzzleSolution pMagSquare $ toSolutionMap 3 [
  2, 9, 4,
  7, 5, 3,
  6, 1, 8]

  -- | kakuro-small.pz
pKakSmall :: Puzzle
pKakSmall = Grid 4 4 [
  CC $ ConstrainedCells (ConstraintList [Unique (range 1 9), AddsTo (Number 15)]) (Row (Number 0) (Just $ range 0 2)),
  CC $ ConstrainedCells (ConstraintList [Unique (range 1 9), AddsTo (Number 8)]) (Row (Number 1) (Just $ range 0 2)),
  CC $ ConstrainedCells (ConstraintList [Unique (range 1 9), AddsTo (Number 15)]) (Row (Number 2) (Just $ range 1 3)),
  CC $ ConstrainedCells (ConstraintList [Unique (range 1 9), AddsTo (Number 19)]) (Row (Number 3) (Just $ range 1 3)),
  CC $ ConstrainedCells (ConstraintList [Unique (range 1 9), AddsTo (Number 11)]) (Col (Number 0) (Just $ range 0 1)),
  CC $ ConstrainedCells (ConstraintList [Unique (range 1 9), AddsTo (Number 16)]) (Col (Number 1) (Just $ range 0 3)),
  CC $ ConstrainedCells (ConstraintList [Unique (range 1 9), AddsTo (Number 17)]) (Col (Number 2) (Just $ range 0 3)),
  CC $ ConstrainedCells (ConstraintList [Unique (range 1 9), AddsTo (Number 13)]) (Col (Number 3) (Just $ range 2 3)),
  CellInit (cell 0 2) (Number 3),
  CellInit (cell 1 1) (Number 1),
  CellInit (cell 2 2) (Number 2),
  CellInit (cell 3 1) (Number 2),
  CellInit Unconstrained (Number 0)]

sKakSmall :: PuzzleSolution
sKakSmall = PuzzleSolution pKakSmall $ toSolutionMap 4 [
  8, 4, 3, 0,
  3, 1, 4, 0,
  0, 9, 2, 4,
  0, 2, 8, 9]