module PuzzleParser where

import Prelude hiding (filter)
import Control.Applicative (Alternative(..))
import PuzzleSyntax
import PuzzlePrinter (prettyPrint)
import Data.Char qualified as Char
import Parser (Parser)
import Parser qualified as P
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck (Arbitrary, Gen, Property, quickCheck, (==>), property, counterexample)
import Debug.Trace (trace)
import Data.Functor (($>), (<&>))


-- Parser for whitespace-aware parsing
wsP :: Parser a -> Parser a
wsP p = p <* many P.space

-- Parsing a keyword
keyword :: String -> Parser ()
keyword s = wsP (P.string s) $> ()

-- Parsing an integer
parseInt :: Parser Int
-- parseInt = wsP $ read <$> some digit

parseInt = wsP $ (negate <$> (P.char '-' *> parseDigit)) <|> parseDigit
  where
    parseDigit = read <$> some P.digit

-- Wrapping in parentheses
parens :: Parser a -> Parser a
parens p = P.between (keyword "(") p (keyword ")")

-- Wrapping in brackets
brackets :: Parser a -> Parser a
brackets p = P.between (keyword "[") p (keyword "]")

-- Wrapping in braces
braces :: Parser a -> Parser a
braces p = P.between (keyword "{") p (keyword "}")

-- Top-level parser for numeric expressions
parseNumExp :: Parser NumExp
parseNumExp = parseTerm `P.chainl1` parseAddSub

-- Parser for terms (higher precedence: * / % ^)
parseTerm :: Parser NumExp
parseTerm = parseFactor `P.chainl1` parseMulDivMod

-- Parser for factors (highest precedence: numbers, variables, and parenthesis)
parseFactor :: Parser NumExp
parseFactor = P.choice [parseRepeatVar, parseNumber, parens parseNumExp]
  where
    parseRepeatVar = RepeatVar <$> (P.char '$' *> parseInt)
    parseNumber = Number <$> parseInt

-- Parser for addition and subtraction
parseAddSub :: Parser (NumExp -> NumExp -> NumExp)
parseAddSub = (keyword "+" $> (`Op2` Plus)) <|> (keyword "-" $> (`Op2` Minus))

-- Parser for multiplication, division, power and modulo
parseMulDivMod :: Parser (NumExp -> NumExp -> NumExp)
parseMulDivMod = P.choice [
  keyword "*" $> (`Op2` Times),
  keyword "//" $> (`Op2` Divide),
  keyword "%" $> (`Op2` Modulo),
  keyword "^" $> (`Op2` Power)]

testParseNumExp :: Test
testParseNumExp = TestList
  [ P.parse parseNumExp "42" ~?= Right (Number 42),
    P.parse parseNumExp "$100" ~?= Right (RepeatVar 100),
    P.parse parseNumExp "(-1 + 2)" ~?= Right (Op2 (Number (-1)) Plus (Number 2)),
    P.parse parseNumExp "(3 * (4 + 5))" ~?= Right (Op2 (Number 3) Times (Op2 (Number 4) Plus (Number 5)))
  ]
-- >>> parse parseNumExp "1 - -2"
-- Right (Op2 (Number 1) Minus (Number (-2)))

-- >>> runTestTT testParseNumExp
-- Counts {cases = 4, tried = 4, errors = 0, failures = 0}


-- Parsing a range (e.g., "1 to 10")
parseRange :: Parser Range
parseRange = Range <$> ((,) <$> parseNumExp <*> (keyword "to" *> parseNumExp))
  <|> (Range <$> ((,) <$> parseNumExp <*> (keyword "," *> parseNumExp)))

testParseRange :: Test
testParseRange = TestList
  [ P.parse parseRange "1 to 10" ~?= Right (Range (Number 1, Number 10)),
    P.parse parseRange "3 to 7" ~?= Right (Range (Number 3, Number 7)),
    P.parse parseRange "(1 + 2) to (3 * 4)" ~?= Right (Range (Op2 (Number 1) Plus (Number 2), Op2 (Number 3) Times (Number 4))),
    P.parse parseRange "$1 to $2" ~?= Right (Range (RepeatVar 1, RepeatVar 2)),
    P.parse parseRange "1 to " ~?= Left "No parses",  -- Incomplete range
    P.parse parseRange "to 10" ~?= Left "No parses", -- Missing start of range
    P.parse parseRange "1 - 10" ~?= Left "No parses" -- Invalid separator
  ]
-- >>> runTestTT testParseRange
-- Counts {cases = 7, tried = 7, errors = 0, failures = 0}

parsePrimitiveConstraint :: Parser PrimitiveConstraint
parsePrimitiveConstraint = P.choice [
  (keyword "unique" *> parens parseRange) <&> Unique,
  (keyword "addsTo" *> parens parseNumExp) <&> AddsTo,
  (keyword "multTo" *> parens parseNumExp) <&> MultsTo,
  (keyword "greaterThan" *> parens ((,) <$> parseNumExp <*> (wsP (P.char ',') *> parseNumExp))) <&> uncurry GreaterThan,
  (keyword "lessThan" *> parens ((,) <$> parseNumExp <*> (wsP (P.char ',') *> parseNumExp))) <&> uncurry LessThan,
  keyword "always" $> Always,
  keyword "never" $> Never]


testParsePrimitiveConstraint :: Test
testParsePrimitiveConstraint = TestList
  [ P.parse parsePrimitiveConstraint "unique(1 to 10)" ~?= Right (Unique (Range (Number 1, Number 10))),
    P.parse parsePrimitiveConstraint "addsTo(15)" ~?= Right (AddsTo (Number 15)),
    P.parse parsePrimitiveConstraint "multTo(20)" ~?= Right (MultsTo (Number 20)),
    P.parse parsePrimitiveConstraint "greaterThan(1, 2)" ~?= Right (GreaterThan (Number 1) (Number 2)),
    P.parse parsePrimitiveConstraint "lessThan(5, 3)" ~?= Right (LessThan (Number 5) (Number 3)),
    P.parse parsePrimitiveConstraint "always" ~?= Right Always,
    P.parse parsePrimitiveConstraint "never" ~?= Right Never
  ]

-- >>> runTestTT testParsePrimitiveConstraint
-- Counts {cases = 7, tried = 7, errors = 0, failures = 0}

-- Parsing a single primitive constraint prefixed by "constraint"
parseSinglePrimitiveConstraint :: Parser PrimitiveConstraint
parseSinglePrimitiveConstraint =
  keyword "constraint" *> parsePrimitiveConstraint

-- Parsing a constraint list
parseConstraintList :: Parser Constraint
parseConstraintList = 
  let pcsP = parsePrimitiveConstraint `P.sepBy` keyword ","
  in ConstraintList <$> (keyword "constraints" *> brackets pcsP)

testParseConstraintList :: Test
testParseConstraintList = TestList
  [ P.parse parseConstraintList "constraints[unique(1 to 10)]"
      ~?= Right (ConstraintList [Unique (Range (Number 1, Number 10))]),
    P.parse parseConstraintList "constraints[unique(1 to 10), addsTo(15)]"
      ~?= Right (ConstraintList [Unique (Range (Number 1, Number 10)), AddsTo (Number 15)]),
    P.parse parseConstraintList "constraints[unique(1 to 10), addsTo(15), multTo(20)]"
      ~?= Right (ConstraintList [Unique (Range (Number 1, Number 10)), AddsTo (Number 15), MultsTo (Number 20)]),
    P.parse parseConstraintList "constraints[always, never]"
      ~?= Right (ConstraintList [Always, Never]),
    P.parse parseConstraintList "constraints[unique(1 to 5), greaterThan(1, 2), lessThan(5, 3)]"
      ~?= Right (ConstraintList [Unique (Range (Number 1, Number 5)), GreaterThan (Number 1) (Number 2), LessThan (Number 5) (Number 3)]),
    P.parse parseConstraintList "constraints[]"
      ~?= Right (ConstraintList [])
  ]

-- >>> runTestTT testParseConstraintList
-- Counts {cases = 6, tried = 6, errors = 0, failures = 0}

-- Parsing a single constraint
parseConstraint :: Parser Constraint
parseConstraint =
  (PC <$> parseSinglePrimitiveConstraint)
    <|> parseConstraintList


testParseConstraint :: Test
testParseConstraint = TestList
  [ -- Parsing a single primitive constraint
    P.parse parseConstraint "constraint unique(1 to 10)"
      ~?= Right (PC (Unique (Range (Number 1, Number 10)))),
    P.parse parseConstraint "constraint addsTo(15)"
      ~?= Right (PC (AddsTo (Number 15))),
    P.parse parseConstraint "constraint multTo(20)"
      ~?= Right (PC (MultsTo (Number 20))),
    P.parse parseConstraint "constraint greaterThan(5, 3)"
      ~?= Right (PC (GreaterThan (Number 5) (Number 3))),
    P.parse parseConstraint "constraint lessThan(4, 6)"
      ~?= Right (PC (LessThan (Number 4) (Number 6))),
    P.parse parseConstraint "constraint always"
      ~?= Right (PC Always),
    P.parse parseConstraint "constraint never"
      ~?= Right (PC Never),

    -- Parsing a constraint list
    P.parse parseConstraint "constraints[unique(1 to 10)]"
      ~?= Right (ConstraintList [Unique (Range (Number 1, Number 10))]),
    P.parse parseConstraint "constraints[unique(1 to 10), addsTo(15)]"
      ~?= Right (ConstraintList [Unique (Range (Number 1, Number 10)), AddsTo (Number 15)]),
    P.parse parseConstraint "constraints[unique(1 to 10), addsTo(15), multTo(20)]"
      ~?= Right (ConstraintList [Unique (Range (Number 1, Number 10)), AddsTo (Number 15), MultsTo (Number 20)]),
    P.parse parseConstraint "constraints[always, never]"
      ~?= Right (ConstraintList [Always, Never]),
    P.parse parseConstraint "constraints[unique(1 to 5), greaterThan(1, 2), lessThan(5, 3)]"
      ~?= Right (ConstraintList [Unique (Range (Number 1, Number 5)), GreaterThan (Number 1) (Number 2), LessThan (Number 5) (Number 3)]),

    -- Invalid inputs
    P.parse parseConstraint "invalid" ~?= Left "No parses",
    P.parse parseConstraint "constraints[invalid]" ~?= Left "No parses"
  ]

-- >>> runTestTT testParseConstraint
-- Counts {cases = 14, tried = 14, errors = 0, failures = 0}

-- Parsing a cell group
parseCellGroup :: Parser CellGroup
parseCellGroup = P.choice [
  keyword "cell" *>
    parens (ACell <$> parseNumExp <*> (keyword "," *> parseNumExp)),
  keyword "cells" *>
    brackets (CellList <$> parseCellGroup `P.sepBy` keyword ","),
  keyword "row" *> (Row <$> parseNumExp <*> parseRowColRange),
  keyword "col" *> (Col <$> parseNumExp <*> parseRowColRange),
  keyword "subgrid" *> parens (Subgrid <$> parseNumExp
    <*> (keyword "," *> parseNumExp)
    <*> (keyword "," *> parseNumExp)
    <*> (keyword "," *> parseNumExp)),
  keyword "subgrid" *> parens
    (pairP >>= \(r, c) ->
      Subgrid r c <$>
        (keyword "," *> parseNumExp) <*>
        (keyword "," *> parseNumExp)),
  keyword "all" $> All,
  keyword "unconstrained" $> Unconstrained,
  P.char '~' *> (Inverse <$> parseCellGroup)]
  where
    pairP = parens ((,) <$> parseNumExp <*> (keyword "," *> parseNumExp))

testParseCellGroup :: Test
testParseCellGroup = TestList
  [ P.parse parseCellGroup "cell(1,2)"
      ~?= Right (ACell (Number 1) (Number 2)),
    P.parse parseCellGroup "cells[cell(1,2), cell(3,4)]"
      ~?= Right (CellList [ACell (Number 1) (Number 2), ACell (Number 3) (Number 4)]),
    P.parse parseCellGroup "row 1"
      ~?= Right (Row (Number 1) Nothing),
    P.parse parseCellGroup "row 1 from 0 to 3"
      ~?= Right (Row (Number 1) (Just (Range (Number 0, Number 3)))),
    P.parse parseCellGroup "col 2"
      ~?= Right (Col (Number 2) Nothing),
    P.parse parseCellGroup "col 2 from 0 to 4"
      ~?= Right (Col (Number 2) (Just (Range (Number 0, Number 4)))),
    P.parse parseCellGroup "subgrid(0,0,2,2)"
      ~?= Right (Subgrid (Number 0) (Number 0) (Number 2) (Number 2)),
    P.parse parseCellGroup "all"
      ~?= Right All,
    P.parse parseCellGroup "unconstrained"
      ~?= Right Unconstrained,
    P.parse parseCellGroup "invalid"
      ~?= Left "No parses"
  ]

-- >>> runTestTT testParseCellGroup
-- Counts {cases = 10, tried = 10, errors = 0, failures = 0}

-- Helper for parsing optional ranges in rows and columns
parseRowColRange :: Parser (Maybe Range)
parseRowColRange = (keyword "from" *> (Just <$> parseRange)) <|> pure Nothing

parseConstraintRule :: Parser ConstraintRule
parseConstraintRule = P.choice [
  keyword "init" *> (CellInit <$> parseCellGroup <*> (keyword "with" *> parseNumExp)),
  keyword "repeat" *> (Repeat <$> parens parseRange <*> braces parseSingleOrMultipleRules),
  CC <$> (ConstrainedCells <$> parseConstraint <*> (keyword "in" *> parseCellGroup))]

parseSingleOrMultipleRules :: Parser ConstraintRule
parseSingleOrMultipleRules = many parseConstraintRule >>= wrapMultipleRules
  where
    wrapMultipleRules :: [ConstraintRule] -> Parser ConstraintRule
    wrapMultipleRules [rule] = pure rule
    wrapMultipleRules rules  = pure $ CC $ ConstrainedCells (ConstraintList []) All

testParseConstraintRule :: Test
testParseConstraintRule = TestList
  [ -- Test for CellInit
    P.parse parseConstraintRule "init cell(1,2) with 42"
      ~?= Right (CellInit (ACell (Number 1) (Number 2)) (Number 42)),

    P.parse parseConstraintRule "init row 1 from 0 to 3 with $1"
      ~?= Right (CellInit (Row (Number 1) (Just (Range (Number 0, Number 3)))) (RepeatVar 1)),

    P.parse parseConstraintRule "init cell(0,0) with 1"
      ~?= Right (CellInit (ACell (Number 0) (Number 0)) (Number 1)),
    P.parse parseConstraintRule "repeat(1 to 4) { init cell(0,0) with $1 }"
      ~?= Right (Repeat
                   (Range (Number 1, Number 4))
                   (CellInit (ACell (Number 0) (Number 0)) (RepeatVar 1))),
    P.parse parseConstraintRule "repeat(0 to 1) { repeat(0 to 1) { init cell(0,0) with $0 } }"
      ~?= Right (Repeat
                   (Range (Number 0, Number 1))
                   (Repeat
                     (Range (Number 0, Number 1))
                     (CellInit (ACell (Number 0) (Number 0)) (RepeatVar 0)))),

    -- Test for CC
    P.parse parseConstraintRule "constraints [unique(1 to 9), addsTo(15)] in col 2 from 0 to 3"
      ~?= Right (CC (ConstrainedCells
                    (ConstraintList [Unique (Range (Number 1, Number 9)), AddsTo (Number 15)])
                    (Col (Number 2) (Just (Range (Number 0, Number 3)))))),

    P.parse parseConstraintRule "constraints [greaterThan(1, 2), always] in subgrid(0,0,2,2)"
      ~?= Right (CC (ConstrainedCells
                    (ConstraintList [GreaterThan (Number 1) (Number 2), Always])
                    (Subgrid (Number 0) (Number 0) (Number 2) (Number 2)))),

    -- Invalid inputs
    P.parse parseConstraintRule "invalid"
      ~?= Left "No parses",

    P.parse parseConstraintRule "init with cell(1,1) 42"
      ~?= Left "No parses"
  ]
-- >>> P.parse parseConstraintRule "constraints [greaterThan(1, 2), always] in subgrid((($0 * 2), ($1 * 2)), 2, 2)"
-- Right (CC (ConstrainedCells {constraint = ConstraintList [GreaterThan (Number 1) (Number 2),Always], cellGroup = Subgrid (Op2 (RepeatVar 0) Times (Number 2)) (Op2 (RepeatVar 1) Times (Number 2)) (Number 2) (Number 2)}))

-- >>> runTestTT testParseConstraintRule
-- Counts {cases = 9, tried = 9, errors = 0, failures = 0}

-- Parsing a grid (the puzzle)
parsePuzzle :: Parser PuzzleSyntax
parsePuzzle =
  keyword "grid"
    *> parens ((,) <$> parseInt <*> (keyword "," *> parseInt))
    >>= \(width, height) -> Grid width height <$>
    (keyword "{" *> many parseConstraintRule <* keyword "}")

testParsePuzzle :: Test
testParsePuzzle = TestList
  [ -- Valid puzzle with a single rule
    P.parse parsePuzzle "grid(4,4) { init cell(0,0) with 1 }"
      ~?= Right (Grid
                  4
                  4
                  [CellInit (ACell (Number 0) (Number 0)) (Number 1)]),

    -- Valid puzzle with multiple rules
    P.parse parsePuzzle "grid(4,4) { init cell(0,0) with 1 repeat (1 to 4) { init cell(0,1) with $1 } }"
      ~?= Right (Grid
                  4
                  4
                  [ CellInit (ACell (Number 0) (Number 0)) (Number 1),
                    Repeat (Range (Number 1, Number 4))
                           (CellInit (ACell (Number 0) (Number 1)) (RepeatVar 1))
                  ]),

    -- Missing closing brace
    P.parse parsePuzzle "grid(4,4) { init cell(0,0) with 1"
      ~?= Left "No parses",

    -- Empty grid
    P.parse parsePuzzle "grid(4,4) { }"
      ~?= Right (Grid 4 4 [])
  ]

-- >>> P.parse parsePuzzle "grid(4, 4) {repeat(0, 3) { constraint unique(1 to 4) in row $0 } repeat(0, 3) { constraint unique(1 to 4) in col $0 } repeat(0, 1) { repeat(0, 1) { constraint unique(1 to 4) in subgrid(($0 * 2, $1 * 2), 2, 2)}} init cell(0, 3) with 3 init cell(1, 1) with 4 init cell(2, 2) with 3 init cell(2, 3) with 2 } "
-- Left "No parses"

-- >>> runTestTT testParsePuzzle
-- Counts {cases = 4, tried = 4, errors = 0, failures = 0}

-- | Parse a puzzle directly from a file
parsePuzzleFromFile :: String -> IO (Either P.ParseError PuzzleSyntax)
parsePuzzleFromFile = P.parseFromFile parsePuzzle

testConstraintRule :: String
testConstraintRule = "repeat(0, 2) { constraint addsTo(15) in row $0 }"

-- >>> P.parse parseConstraintRule testConstraintRule
-- Right (Repeat (Range (Number 0,Number 2)) (CC (ConstrainedCells {constraint = PC (AddsTo (Number 15)), cellGroup = Row (RepeatVar 0) Nothing})))

-- >>> P.parse parseConstraintRule "constraints [unique(1 to 9)] in all"
-- Right (CC (ConstrainedCells {constraint = ConstraintList [Unique (Range (Number 1,Number 9))], cellGroup = All}))

prop_roundtrip :: PuzzleSyntax -> Property
prop_roundtrip puzzle =
  let prettyStr = prettyPrint puzzle
      parsed = P.parse parsePuzzle prettyStr
  in 
     case parsed of
       Left err ->
         counterexample ("Parsing failed: " ++ show err) False
       Right parsedPuzzle ->
         (parsedPuzzle == puzzle)  -- This is a Bool, which is automatically converted to a Property
         ==> parsedPuzzle == puzzle  -- This ensures that it gets turned into a Property

testAll :: IO Counts
testAll = runTestTT $ TestList [
  testParseCellGroup,
  testParseConstraint,
  testParseConstraintList,
  testParseConstraintRule,
  testParseNumExp,
  testParsePrimitiveConstraint,
  testParsePuzzle,
  testParseRange]

testParseSample :: String -> PuzzleSyntax -> IO Counts
testParseSample filename expected = do
    parsed <- parsePuzzleFromFile filename
    putStrLn ("\nTesting " ++ filename)
    runTestTT (parsed ~?= Right expected)

runAllTests :: IO ()
runAllTests = do
  _ <- testAll
  _ <- testParseSample "samples/empty.pz" pEmpty
  _ <- testParseSample "samples/sudoku-small.pz" pSudSmall
  _ <- testParseSample "samples/kakuro-small.pz" pKakSmall
  _ <- testParseSample "samples/magicsquare.pz" pMagSquare
  _ <- testParseSample "samples/futoshiki.pz" pFutoshiki
  _ <- testParseSample "samples/kenken.pz" pKenKen
  putStrLn "\nTesting Roundtrip"
  quickCheck prop_roundtrip
  return ()
