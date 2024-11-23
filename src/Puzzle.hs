module Puzzle where
import Data.Map ( Map )

data NumExp =
  Number Int |
  Op2 NumExp Bop NumExp |
  RepeatVar Int

data Bop
  = Plus
  | Minus
  | Times
  | Divide
  | Modulo
  | Power

newtype Range = Range (NumExp, NumExp)

data Puzzle = Grid {
  width :: Int,
  height :: Int,
  constraints :: [ConstrainedCells]
}

data ConstraintRule
  = CellInit CellGroup NumExp
  | CC ConstrainedCells
  | Repeat Range ConstraintRule

data ConstrainedCells = ConstrainedCells {
  constraint :: Constraint,
  cellGroup :: CellGroup
}

data Constraint
  = PC PrimitiveConstraint
  | ConstraintList [Constraint]

instance Semigroup Constraint where
  (<>) :: Constraint -> Constraint -> Constraint
  (PC Always) <> c = c
  c <> (PC Always) = c
  n@(PC Never) <> _ = n
  _ <> n@(PC Never) = n
  ConstraintList l1 <> ConstraintList l2 = ConstraintList $ l1 ++ l2
  ConstraintList l <> c = ConstraintList $ c : l
  c <> ConstraintList l = ConstraintList $ c : l
  c1 <> c2 = ConstraintList [c1, c2]

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

data CellGroup
  = ACell NumExp NumExp
  | CellList [CellGroup]
  | Subgrid NumExp NumExp NumExp NumExp
  | Row NumExp (Maybe (NumExp, NumExp))
  | Col NumExp (Maybe (NumExp, NumExp))
  | Inverse CellGroup
  | All
  | Unconstrained

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

data PuzzleSolution = PuzzleSolution {
    puzzle :: Puzzle,
    cellValues :: Map (Int, Int) Int
}