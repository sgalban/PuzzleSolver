
module PuzzleParser where

import Control.Applicative
import Data.Char qualified as Char
import Puzzle
import Parser (Parser)
import Parser qualified as P
import Text.PrettyPrint (Doc, (<+>), text, nest, hsep, vcat, parens, braces, brackets, render)
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen


-- | Parse a number
numberP :: Parser NumExp
numberP = Number <$> wsP P.int

-- | Parse a binary operation
bopP :: Parser Bop
bopP = undefined

-- | Parse a numerical expression
numExpP :: Parser NumExp
numExpP = undefined

-- | Parse a repeat variable (e.g., $1)
repeatVarP :: Parser NumExp
repeatVarP = undefined
-- | Parse a range (e.g., 1 to 5)
rangeP :: Parser Range
rangeP =undefined

-- | Parse a grid definition
gridP :: Parser Puzzle
gridP = undefined

-- | Parse a constraint
constraintsP :: Parser ConstrainedCells
constraintsP = undefined

-- | Parse a constraint rule
constraintP :: Parser Constraint
constraintP = undefined

-- | Parse primitive constraints
primitiveConstraintP :: Parser Constraint
primitiveConstraintP = undefined

-- | Parse a cell group
cellGroupP :: Parser CellGroup
cellGroupP = undefined

-- | Parse a cell initialization
cellInitP :: Parser ConstrainedCells
cellInitP = undefined

-- Utility parsers
wsP :: Parser a -> Parser a
wsP p = p <* many P.space

stringP :: String -> Parser ()
stringP s = wsP (P.string s) *> pure ()

parens :: Parser a -> Parser a
parens p = P.between (stringP "(") p (stringP ")")

braces :: Parser a -> Parser a
braces p = P.between (stringP "{") p (stringP "}")

brackets :: Parser a -> Parser a
brackets p = P.between (stringP "[") p (stringP "]")

-- Test Cases
test_numberP :: Test
test_numberP =
  TestList
    [ P.parse numberP "123" ~?= Right (Number 123),
      P.parse numberP "0" ~?= Right (Number 0)
    ]

test_bopP :: Test
test_bopP =
  TestList
    [ P.parse bopP "+" ~?= Right Plus,
      P.parse bopP "-" ~?= Right Minus,
      P.parse bopP "*" ~?= Right Times
    ]

test_gridP :: Test
test_gridP =
  TestList
    [ P.parse gridP "grid(4,4){ }" ~?= Right (Grid 4 4 []),
      P.parse gridP "grid(3,3){ init cell(0,0) with 5 }"
        ~?= Right (Grid 3 3 [CC (ConstrainedCells (PC Always) (ACell (Number 0) (Number 0)))])
    ]

runTests :: IO Counts
runTests = runTestTT $ TestList [test_numberP, test_bopP, test_gridP]

-- >>> runTests

-- | Pretty print a number
prettyNumExp :: NumExp -> Doc
prettyNumExp  = undefined

prettyBop :: Bop -> Doc
prettyBop = undefined

prettyPuzzle :: Puzzle -> Doc
prettyPuzzle = undefined

prettyConstrainedCells :: ConstrainedCells -> Doc
prettyConstrainedCells = undefined

prettyPrimitiveConstraint :: PrimitiveConstraint -> Doc
prettyPrimitiveConstraint = undefined

prettyCellGroup :: CellGroup -> Doc
prettyCellGroup = undefined



-- Property 1: Roundtrip parsing
-- If you parse a value and then pretty-print it back, parsing the result should yield the same value.
prop_roundtrip_numExp :: NumExp -> Bool
prop_roundtrip_numExp exp =
  case P.parse numExpP (render (prettyNumExp exp)) of
    Right parsed -> parsed == exp
    _ -> False

-- Property 2: Parser always succeeds for valid inputs
prop_parser_succeeds :: NumExp -> Bool
prop_parser_succeeds exp = P.parse numExpP (render (prettyNumExp exp)) /= Left "No parses"

-- Property 3: Binary Operations Preserve Structure
prop_bop_structure :: NumExp -> Bop -> NumExp -> Bool
prop_bop_structure left op right =
  let input = render (prettyNumExp (Op2 left op right))
   in case P.parse numExpP input of
        Right (Op2 l o r) -> l == left && o == op && r == right
        _ -> False

qc :: IO ()
qc = do
  putStrLn "QuickCheck Tests:"
  QC.quickCheck prop_roundtrip_numExp
  QC.quickCheck prop_parser_succeeds
  QC.quickCheck prop_bop_structure
