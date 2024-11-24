module Puzzle where
import Data.Map ( Map )
import Test.QuickCheck (Arbitrary)
import qualified Test.QuickCheck as QC
import Control.Monad (liftM2, liftM3)

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
  constraints :: [ConstrainedCells]
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
  | Row NumExp (Maybe (NumExp, NumExp))
  | Col NumExp (Maybe (NumExp, NumExp))
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
  cellValues :: Map (Int, Int) Int
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
      rowColBoundGen :: QC.Gen (Maybe (NumExp, NumExp))
      rowColBoundGen = QC.oneof [
        Just <$> liftM2 (,) QC.arbitrary QC.arbitrary,
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