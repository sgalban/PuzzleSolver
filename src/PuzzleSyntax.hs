module PuzzleSyntax where
import Test.QuickCheck (Arbitrary)
import qualified Test.QuickCheck as QC
import Control.Monad (liftM2, liftM3)
import qualified Data.Map as Map
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP

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

data PuzzleSyntax = Grid {
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

-- QuickCheck instances

genNat :: QC.Gen Int
genNat = abs <$> QC.arbitrary

instance Arbitrary Bop where
  arbitrary :: QC.Gen Bop
  arbitrary = QC.frequency [
    (4, pure Plus),
    (1, pure Minus),
    (4, pure Times),
    (1, pure Divide),
    (1, pure Modulo),
    (1, pure Power)]

genNumExp :: Bool -> QC.Gen NumExp
genNumExp inRepeat = QC.resize 5 $ QC.sized genExp
  where
    genExp 0 = QC.frequency [
      (if inRepeat then 1 else 0, return $ RepeatVar 0),
      (if inRepeat then 1 else 0, return $ RepeatVar 1),
      (4, Number <$> genNat)]
    genExp n = QC.frequency [
      (20, genExp 0),
      (n, Op2 <$> genExp n' <*> QC.arbitrary <*> genExp n')]
        where
          n' = n `div` 2

instance Arbitrary NumExp where
  arbitrary :: QC.Gen NumExp
  arbitrary = genNumExp True

  shrink :: NumExp -> [NumExp]
  shrink (Op2 ne1 bop ne2) = [ne1, ne2]
  shrink _ = []

genRange :: Bool -> QC.Gen Range
genRange inRepeat = QC.frequency [
  (1, liftM2 (curry Range) (genNumExp inRepeat) (genNumExp inRepeat)),
  (5, genValidRange)]
  where
    genValidRange = do
      start <- genNat
      size <- genNat
      return $ Range (Number start, Number (start + size))

instance Arbitrary Range where
  arbitrary :: QC.Gen Range
  arbitrary = genRange True

genPC :: Bool -> QC.Gen PrimitiveConstraint
genPC inRepeat = QC.oneof [
  Unique <$> genRange inRepeat,
  AddsTo <$> genNumExp inRepeat,
  MultsTo <$> genNumExp inRepeat,
  GreaterThan <$> genNumExp inRepeat <*> genNumExp inRepeat,
  LessThan <$> genNumExp inRepeat <*> genNumExp inRepeat,
  return Always]

instance Arbitrary PrimitiveConstraint where
  arbitrary :: QC.Gen PrimitiveConstraint
  arbitrary = genPC True

genCon :: Bool -> QC.Gen Constraint
genCon inRepeat = QC.sized genCon'
  where
    genCon' 0 = PC <$> genPC inRepeat
    genCon' n = QC.oneof [
      genCon' 0,
      ConstraintList <$> QC.resize 3 (QC.listOf (genPC inRepeat))]

instance Arbitrary Constraint where
  arbitrary :: QC.Gen Constraint
  arbitrary = genCon True

  shrink :: Constraint -> [Constraint]
  shrink (ConstraintList [c]) = [PC c]
  shrink (ConstraintList list) = ConstraintList <$> QC.shrink list
  shrink _ = []

genCG :: Bool -> QC.Gen CellGroup
genCG inRepeat = QC.frequency [
  (5, ACell <$> genNE <*> genNE),
  (2, CellList <$> QC.resize 3 (QC.listOf (genCG inRepeat))),
  (3, Subgrid <$> genNE <*> genNE <*> genNE <*> genNE),
  (3, Row <$> genNE <*> rowColBoundGen),
  (3, Col <$> genNE <*> rowColBoundGen),
  (1, Inverse <$> genCG inRepeat),
  (1, return All),
  (1, return Unconstrained)]
  where
    rowColBoundGen :: QC.Gen (Maybe Range)
    rowColBoundGen = QC.oneof [
      Just <$> genRange inRepeat,
      return Nothing]
    genNE = genNumExp inRepeat

instance Arbitrary CellGroup where
  arbitrary :: QC.Gen CellGroup
  arbitrary = genCG True

  shrink :: CellGroup -> [CellGroup]
  shrink (CellList [cg]) = [cg]
  shrink (CellList list) = CellList <$> QC.shrink list
  shrink _ = []

genCC :: Bool -> QC.Gen ConstrainedCells
genCC inRepeat = liftM2 ConstrainedCells (genCon inRepeat) (genCG inRepeat)

instance Arbitrary ConstrainedCells where
  arbitrary :: QC.Gen ConstrainedCells
  arbitrary = genCC True

genCR :: Bool -> QC.Gen ConstraintRule
genCR inRepeat' = QC.sized (genConRule inRepeat')
    where
      genConRule :: Bool -> Int -> QC.Gen ConstraintRule
      genConRule inRepeat 0 = QC.oneof [
        CellInit <$> genCG inRepeat <*> genNumExp inRepeat,
        CC <$> genCC inRepeat]
      genConRule inRepeat n = QC.frequency [
        (2, genConRule inRepeat 0),
        (1, Repeat <$> genRange inRepeat <*> genConRule False (n `div` 2))]

instance Arbitrary ConstraintRule where
  arbitrary :: QC.Gen ConstraintRule
  arbitrary = genCR True

instance Arbitrary PuzzleSyntax where
  arbitrary :: QC.Gen PuzzleSyntax
  arbitrary = liftM3 Grid genDim genDim (QC.resize 15 (QC.listOf (genCR False)))
    where
      genDim :: QC.Gen Int
      genDim = QC.choose (2, 5)

  shrink :: PuzzleSyntax -> [PuzzleSyntax]
  shrink ps = Grid (width ps) (height ps) <$> QC.shrink (constraints ps)

-- Sample Puzzles

-- | A couple of functions to make hardcoding puzzles less verbose

range :: Int -> Int -> Range
range a b = Range (Number a, Number b)

cell :: Int -> Int -> CellGroup
cell a b = ACell (Number a) (Number b)

-- | empty.pz
pEmpty :: PuzzleSyntax
pEmpty = Grid 5 5 []

-- | sudoku-small.pz
pSudSmall :: PuzzleSyntax
pSudSmall = Grid 4 4 [
  Repeat (range 0 3) (CC $ ConstrainedCells (PC $ Unique $ range 1 4) (Row (RepeatVar 0) Nothing)),
  Repeat (range 0 3) (CC $ ConstrainedCells (PC $ Unique $ range 1 4) (Col (RepeatVar 0) Nothing)),
  Repeat (range 0 1) (Repeat (range 0 1) (CC (ConstrainedCells (PC $ Unique $ range 1 4) (Subgrid (Op2 (RepeatVar 0) Times (Number 2)) (Op2 (RepeatVar 1) Times (Number 2)) (Number 2) (Number 2))))),
  CellInit (cell 0 3) (Number 3),
  CellInit (cell 1 1) (Number 4),
  CellInit (cell 2 2) (Number 3),
  CellInit (cell 2 3) (Number 2)]

-- | magicsquare.pz
pMagSquare :: PuzzleSyntax
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

  -- | kakuro-small.pz
pKakSmall :: PuzzleSyntax
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

-- | futoshiki.pz
pFutoshiki :: PuzzleSyntax
pFutoshiki = Grid 5 5 [
  Repeat (range 0 4) (CC $ ConstrainedCells (PC $ Unique $ range 1 5) (Row (RepeatVar 0) Nothing)),
  Repeat (range 0 4) (CC $ ConstrainedCells (PC $ Unique $ range 1 5) (Col (RepeatVar 0) Nothing)),

  CC $ ConstrainedCells (PC $ GreaterThan (Number 0) (Number 1)) (cell 0 0),
  CC $ ConstrainedCells (PC $ GreaterThan (Number 0) (Number 3)) (cell 0 2),
  CC $ ConstrainedCells (PC $ GreaterThan (Number 0) (Number 4)) (cell 0 3),

  CC $ ConstrainedCells (PC $ LessThan (Number 3) (Number 4)) (cell 3 3),
  CC $ ConstrainedCells (PC $ LessThan (Number 4) (Number 1)) (cell 4 0),
  CC $ ConstrainedCells (PC $ LessThan (Number 4) (Number 2)) (cell 4 1),
  
  CellInit (cell 1 0) (Number 4),
  CellInit (cell 1 4) (Number 2),
  CellInit (cell 2 2) (Number 4),
  CellInit (cell 3 4) (Number 4)]

-- | kenken.pz
pKenKen :: PuzzleSyntax
pKenKen = Grid 4 4 [
  Repeat (range 0 3) (CC $ ConstrainedCells (PC $ Unique $ range 1 4) (Row (RepeatVar 0) Nothing)),
  Repeat (range 0 3) (CC $ ConstrainedCells (PC $ Unique $ range 1 4) (Col (RepeatVar 0) Nothing)),

  CC $ ConstrainedCells (PC $ AddsTo (Number 5)) (CellList [cell 0 0, cell 0 1]),
  CC $ ConstrainedCells (PC $ MultsTo (Number 96)) (CellList [cell 0 2, cell 0 3, cell 1 2, cell 1 3, cell 2 2]),
  CC $ ConstrainedCells (PC $ MultsTo (Number 12)) (CellList [cell 1 0, cell 1 1, cell 2 0]),
  CC $ ConstrainedCells (PC $ AddsTo (Number 10)) (CellList [cell 2 1, cell 3 1, cell 3 2, cell 3 3, cell 2 3])]