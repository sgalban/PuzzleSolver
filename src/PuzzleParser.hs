module PuzzleParser where

import Prelude hiding (filter)
import Control.Applicative (Alternative(..))
import Parser
import PuzzleSyntax
import Data.Char qualified as Char
import Parser (Parser)
import Parser qualified as P
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Text.PrettyPrint (Doc, (<+>), text, nest, hsep, vcat, parens, braces, brackets, render, int, char, comma, hcat, punctuate, empty)
import Test.QuickCheck (Arbitrary, Gen, Property, quickCheck, (==>), property, counterexample)
import Debug.Trace (trace)


-- Parser for whitespace-aware parsing
wsP :: Parser a -> Parser a
wsP p = p <* many space

-- Parsing a keyword
keyword :: String -> Parser ()
keyword s = wsP (string s) *> pure ()

-- Parsing an integer
parseInt :: Parser Int
-- parseInt = wsP $ read <$> some digit

parseInt = wsP $ (negate <$> (Parser.char '-' *> parseDigit)) <|> parseDigit
  where
    parseDigit = read <$> some digit

-- Wrapping in parentheses
parens :: Parser a -> Parser a
parens p = between (keyword "(") p (keyword ")")

-- Wrapping in brackets
brackets :: Parser a -> Parser a
brackets p = between (keyword "[") p (keyword "]")

-- Wrapping in braces
braces :: Parser a -> Parser a
braces p = between (keyword "{") p (keyword "}")

parseNumExp :: Parser NumExp
parseNumExp = parseRepeatVar <|> parseNumber <|> PuzzleParser.parens parseOpExpr
  where
    parseRepeatVar = RepeatVar <$> (Parser.char '$' *> parseInt)
    parseNumber = Number <$> parseInt

parseOpExpr :: Parser NumExp
parseOpExpr = parseNumExp `chainl1` parseOperator

parseOperator :: Parser (NumExp -> NumExp -> NumExp)
parseOperator = (\op left right -> Op2 left op right) <$> parseBop

parseBop :: Parser Bop
parseBop =
      (keyword "+" *> pure Plus)
  <|> (keyword "-" *> pure Minus)
  <|> (keyword "*" *> pure Times)
  <|> (keyword "//" *> pure Divide)
  <|> (keyword "%" *> pure Modulo)
  <|> (keyword "^" *> pure Power)
testParseNumExp :: Test
testParseNumExp = TestList
  [ parse parseNumExp "42" ~?= Right (Number 42),
    parse parseNumExp "$100" ~?= Right (RepeatVar 100),
    parse parseNumExp "(-1 + 2)" ~?= Right (Op2 (Number (-1)) Plus (Number 2)),
    parse parseNumExp "(3 * (4 + 5))" ~?= Right (Op2 (Number 3) Times (Op2 (Number 4) Plus (Number 5)))
  ]
-- >>> parse parseNumExp "(1 - -2)"
-- Right (Op2 (Number 1) Minus (Number (-2)))
-- >>> runTestTT testParseNumExp
-- Counts {cases = 4, tried = 4, errors = 0, failures = 0}


-- Parsing a range (e.g., "1 to 10")
parseRange :: Parser Range
parseRange = Range <$> ((,) <$> parseNumExp <*> (keyword "to" *> parseNumExp))
            <|> (Range <$> ((,) <$> parseNumExp <*> (keyword "," *> parseNumExp)))

testParseRange :: Test
testParseRange = TestList
  [ parse parseRange "1 to 10" ~?= Right (Range (Number 1, Number 10)),
    parse parseRange "3 to 7" ~?= Right (Range (Number 3, Number 7)),
    parse parseRange "(1 + 2) to (3 * 4)" ~?= Right (Range (Op2 (Number 1) Plus (Number 2), Op2 (Number 3) Times (Number 4))),
    parse parseRange "$1 to $2" ~?= Right (Range (RepeatVar 1, RepeatVar 2)),
    parse parseRange "1 to " ~?= Left "No parses",  -- Incomplete range
    parse parseRange "to 10" ~?= Left "No parses", -- Missing start of range
    parse parseRange "1 - 10" ~?= Left "No parses" -- Invalid separator
  ]
-- >>> runTestTT testParseRange
-- Counts {cases = 7, tried = 7, errors = 0, failures = 0}


parsePrimitiveConstraint :: Parser PrimitiveConstraint
parsePrimitiveConstraint =
      (keyword "unique" *> PuzzleParser.parens parseRange >>= return . Unique)
  <|> (keyword "addsTo" *> PuzzleParser.parens parseNumExp >>= return . AddsTo)
  <|> (keyword "multTo" *> PuzzleParser.parens parseNumExp >>= return . MultsTo)
  <|> (keyword "greaterThan" *> PuzzleParser.parens ((,) <$> parseNumExp <*> (wsP (Parser.char ',') *> parseNumExp)) >>= return . uncurry GreaterThan)
  <|> (keyword "lessThan" *> PuzzleParser.parens ((,) <$> parseNumExp <*> (wsP (Parser.char ',') *> parseNumExp)) >>= return . uncurry LessThan)
  <|> (keyword "always" *> pure Always)
  <|> (keyword "never" *> pure Never)


testParsePrimitiveConstraint :: Test
testParsePrimitiveConstraint = TestList
  [ parse parsePrimitiveConstraint "unique(1 to 10)" ~?= Right (Unique (Range (Number 1, Number 10))),
    parse parsePrimitiveConstraint "addsTo(15)" ~?= Right (AddsTo (Number 15)),
    parse parsePrimitiveConstraint "multTo(20)" ~?= Right (MultsTo (Number 20)),
    parse parsePrimitiveConstraint "greaterThan(1, 2)" ~?= Right (GreaterThan (Number 1) (Number 2)),
    parse parsePrimitiveConstraint "lessThan(5, 3)" ~?= Right (LessThan (Number 5) (Number 3)),
    parse parsePrimitiveConstraint "always" ~?= Right Always,
    parse parsePrimitiveConstraint "never" ~?= Right Never
  ]

-- >>> runTestTT testParsePrimitiveConstraint
-- Counts {cases = 7, tried = 7, errors = 0, failures = 0}

-- Parsing a single primitive constraint prefixed by "constraint"
parseSinglePrimitiveConstraint :: Parser PrimitiveConstraint
parseSinglePrimitiveConstraint =
  keyword "constraint" *> parsePrimitiveConstraint


-- Parsing a constraint list
parseConstraintList :: Parser Constraint
parseConstraintList = ConstraintList <$> (keyword "constraints" *> PuzzleParser.brackets (parsePrimitiveConstraint `sepBy` keyword ","))

testParseConstraintList :: Test
testParseConstraintList = TestList
  [ parse parseConstraintList "constraints[unique(1 to 10)]"
      ~?= Right (ConstraintList [Unique (Range (Number 1, Number 10))]),
    parse parseConstraintList "constraints[unique(1 to 10), addsTo(15)]"
      ~?= Right (ConstraintList [Unique (Range (Number 1, Number 10)), AddsTo (Number 15)]),
    parse parseConstraintList "constraints[unique(1 to 10), addsTo(15), multTo(20)]"
      ~?= Right (ConstraintList [Unique (Range (Number 1, Number 10)), AddsTo (Number 15), MultsTo (Number 20)]),
    parse parseConstraintList "constraints[always, never]"
      ~?= Right (ConstraintList [Always, Never]),
    parse parseConstraintList "constraints[unique(1 to 5), greaterThan(1, 2), lessThan(5, 3)]"
      ~?= Right (ConstraintList [Unique (Range (Number 1, Number 5)), GreaterThan (Number 1) (Number 2), LessThan (Number 5) (Number 3)]),
    parse parseConstraintList "constraints[]"
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
    parse parseConstraint "constraint unique(1 to 10)"
      ~?= Right (PC (Unique (Range (Number 1, Number 10)))),
    parse parseConstraint "constraint addsTo(15)"
      ~?= Right (PC (AddsTo (Number 15))),
    parse parseConstraint "constraint multTo(20)"
      ~?= Right (PC (MultsTo (Number 20))),
    parse parseConstraint "constraint greaterThan(5, 3)"
      ~?= Right (PC (GreaterThan (Number 5) (Number 3))),
    parse parseConstraint "constraint lessThan(4, 6)"
      ~?= Right (PC (LessThan (Number 4) (Number 6))),
    parse parseConstraint "constraint always"
      ~?= Right (PC Always),
    parse parseConstraint "constraint never"
      ~?= Right (PC Never),

    -- Parsing a constraint list
    parse parseConstraint "constraints[unique(1 to 10)]"
      ~?= Right (ConstraintList [Unique (Range (Number 1, Number 10))]),
    parse parseConstraint "constraints[unique(1 to 10), addsTo(15)]"
      ~?= Right (ConstraintList [Unique (Range (Number 1, Number 10)), AddsTo (Number 15)]),
    parse parseConstraint "constraints[unique(1 to 10), addsTo(15), multTo(20)]"
      ~?= Right (ConstraintList [Unique (Range (Number 1, Number 10)), AddsTo (Number 15), MultsTo (Number 20)]),
    parse parseConstraint "constraints[always, never]"
      ~?= Right (ConstraintList [Always, Never]),
    parse parseConstraint "constraints[unique(1 to 5), greaterThan(1, 2), lessThan(5, 3)]"
      ~?= Right (ConstraintList [Unique (Range (Number 1, Number 5)), GreaterThan (Number 1) (Number 2), LessThan (Number 5) (Number 3)]),

    -- Invalid inputs
    parse parseConstraint "invalid" ~?= Left "No parses",
    parse parseConstraint "constraints[invalid]" ~?= Left "No parses"
  ]

-- >>> runTestTT testParseConstraint
-- Counts {cases = 14, tried = 14, errors = 0, failures = 0}


-- Parsing a cell group
parseCellGroup :: Parser CellGroup
parseCellGroup =
      (keyword "cell" *> PuzzleParser.parens (ACell <$> parseNumExp <*> (keyword "," *> parseNumExp)))
  <|> (keyword "cells" *> PuzzleParser.brackets (CellList <$> parseCellGroup `sepBy` keyword ","))
  <|> (keyword "row" *> (Row <$> parseNumExp <*> parseRowColRange))
  <|> (keyword "col" *> (Col <$> parseNumExp <*> parseRowColRange))
  <|> (keyword "subgrid" *> PuzzleParser.parens (Subgrid <$> parseNumExp <*> (keyword "," *> parseNumExp) <*> (keyword "," *> parseNumExp) <*> (keyword "," *> parseNumExp)))
  <|> (keyword "subgrid" *> PuzzleParser.parens
        (PuzzleParser.parens ((,) <$> parseNumExp <*> (keyword "," *> parseNumExp)) >>= \(r, c) ->
         Subgrid r c <$> (keyword "," *> parseNumExp) <*> (keyword "," *> parseNumExp)))
  <|> (keyword "all" *> pure All)
  <|> (keyword "unconstrained" *> pure Unconstrained)
  <|> (Parser.char '~' *> (Inverse <$> parseCellGroup))

testParseCellGroup :: Test
testParseCellGroup = TestList
  [ parse parseCellGroup "cell(1,2)"
      ~?= Right (ACell (Number 1) (Number 2)),
    parse parseCellGroup "cells[cell(1,2), cell(3,4)]"
      ~?= Right (CellList [ACell (Number 1) (Number 2), ACell (Number 3) (Number 4)]),
    parse parseCellGroup "row 1"
      ~?= Right (Row (Number 1) Nothing),
    parse parseCellGroup "row 1 from 0 to 3"
      ~?= Right (Row (Number 1) (Just (Range (Number 0, Number 3)))),
    parse parseCellGroup "col 2"
      ~?= Right (Col (Number 2) Nothing),
    parse parseCellGroup "col 2 from 0 to 4"
      ~?= Right (Col (Number 2) (Just (Range (Number 0, Number 4)))),
    parse parseCellGroup "subgrid(0,0,2,2)"
      ~?= Right (Subgrid (Number 0) (Number 0) (Number 2) (Number 2)),
    parse parseCellGroup "all"
      ~?= Right All,
    parse parseCellGroup "unconstrained"
      ~?= Right Unconstrained,
    parse parseCellGroup "invalid"
      ~?= Left "No parses"
  ]

-- >>> runTestTT testParseCellGroup
-- Counts {cases = 10, tried = 10, errors = 0, failures = 0}



-- Helper for parsing optional ranges in rows and columns
parseRowColRange :: Parser (Maybe Range)
parseRowColRange = (keyword "from" *> (Just <$> parseRange)) <|> pure Nothing

parseConstraintRule :: Parser ConstraintRule
parseConstraintRule =
      (keyword "init" *> (CellInit <$> parseCellGroup <*> (keyword "with" *> parseNumExp)))
  <|> (keyword "repeat" *> (Repeat <$> PuzzleParser.parens parseRange <*> PuzzleParser.braces parseSingleOrMultipleRules))
  <|> (CC <$> (ConstrainedCells <$> parseConstraint <*> (keyword "in" *> parseCellGroup)))

parseSingleOrMultipleRules :: Parser ConstraintRule
parseSingleOrMultipleRules = many parseConstraintRule >>= wrapMultipleRules
  where
    wrapMultipleRules :: [ConstraintRule] -> Parser ConstraintRule
    wrapMultipleRules [rule] = pure rule
    wrapMultipleRules rules  = pure $ CC $ ConstrainedCells (ConstraintList []) All



testParseConstraintRule :: Test
testParseConstraintRule = TestList
  [ -- Test for CellInit
    parse parseConstraintRule "init cell(1,2) with 42"
      ~?= Right (CellInit (ACell (Number 1) (Number 2)) (Number 42)),

    parse parseConstraintRule "init row 1 from 0 to 3 with $1"
      ~?= Right (CellInit (Row (Number 1) (Just (Range (Number 0, Number 3)))) (RepeatVar 1)),

    parse parseConstraintRule "init cell(0,0) with 1"
      ~?= Right (CellInit (ACell (Number 0) (Number 0)) (Number 1)),
    parse parseConstraintRule "repeat(1 to 4) { init cell(0,0) with $1 }"
      ~?= Right (Repeat
                   (Range (Number 1, Number 4))
                   (CellInit (ACell (Number 0) (Number 0)) (RepeatVar 1))),
    parse parseConstraintRule "repeat(0 to 1) { repeat(0 to 1) { init cell(0,0) with $0 } }"
      ~?= Right (Repeat
                   (Range (Number 0, Number 1))
                   (Repeat
                     (Range (Number 0, Number 1))
                     (CellInit (ACell (Number 0) (Number 0)) (RepeatVar 0)))),

    -- Test for CC
    parse parseConstraintRule "constraints [unique(1 to 9), addsTo(15)] in col 2 from 0 to 3"
      ~?= Right (CC (ConstrainedCells
                    (ConstraintList [Unique (Range (Number 1, Number 9)), AddsTo (Number 15)])
                    (Col (Number 2) (Just (Range (Number 0, Number 3)))))),

    parse parseConstraintRule "constraints [greaterThan(1, 2), always] in subgrid(0,0,2,2)"
      ~?= Right (CC (ConstrainedCells
                    (ConstraintList [GreaterThan (Number 1) (Number 2), Always])
                    (Subgrid (Number 0) (Number 0) (Number 2) (Number 2)))),

    -- Invalid inputs
    parse parseConstraintRule "invalid"
      ~?= Left "No parses",

    parse parseConstraintRule "init with cell(1,1) 42"
      ~?= Left "No parses"
  ]
-- >>> parse parseConstraintRule "constraints [greaterThan(1, 2), always] in subgrid((($0 * 2), ($1 * 2)), 2, 2)"
-- Right (CC (ConstrainedCells {constraint = ConstraintList [GreaterThan (Number 1) (Number 2),Always], cellGroup = Subgrid (Op2 (RepeatVar 0) Times (Number 2)) (Op2 (RepeatVar 1) Times (Number 2)) (Number 2) (Number 2)}))

-- >>> runTestTT testParseConstraintRule
-- Counts {cases = 9, tried = 9, errors = 0, failures = 0}


-- Parsing a grid (the puzzle)
parsePuzzle :: Parser PuzzleSyntax
parsePuzzle =
  keyword "grid"
    *> (PuzzleParser.parens ((,) <$> parseInt <*> (keyword "," *> parseInt)))
    >>= \(width, height) -> Grid width height <$> (keyword "{" *> many parseConstraintRule <* keyword "}")

testParsePuzzle :: Test
testParsePuzzle = TestList
  [ -- Valid puzzle with a single rule
    parse parsePuzzle "grid(4,4) { init cell(0,0) with 1 }"
      ~?= Right (Grid
                  4
                  4
                  [CellInit (ACell (Number 0) (Number 0)) (Number 1)]),

    -- Valid puzzle with multiple rules
    parse parsePuzzle "grid(4,4) { init cell(0,0) with 1 repeat (1 to 4) { init cell(0,1) with $1 } }"
      ~?= Right (Grid
                  4
                  4
                  [ CellInit (ACell (Number 0) (Number 0)) (Number 1),
                    Repeat (Range (Number 1, Number 4))
                           (CellInit (ACell (Number 0) (Number 1)) (RepeatVar 1))
                  ]),

    -- Missing closing brace
    parse parsePuzzle "grid(4,4) { init cell(0,0) with 1"
      ~?= Left "No parses",

    -- Empty grid
    parse parsePuzzle "grid(4,4) { }"
      ~?= Right (Grid 4 4 [])
  ]

-- >>> parse parsePuzzle "grid(4, 4) {repeat(0, 3) { constraint unique(1 to 4) in row $0 } repeat(0, 3) { constraint unique(1 to 4) in col $0 } repeat(0, 1) { repeat(0, 1) { constraint unique(1 to 4) in subgrid(($0 * 2, $1 * 2), 2, 2)}} init cell(0, 3) with 3 init cell(1, 1) with 4 init cell(2, 2) with 3 init cell(2, 3) with 2 } "
-- Left "No parses"

-- >>> runTestTT testParsePuzzle
-- Counts {cases = 4, tried = 4, errors = 0, failures = 0}




-- -- Main for testing
-- main :: IO ()
-- main = do
--   print testPuzzle
--   runTestTT $ TestList
--     [ "Whitespace Parser" ~: runTestTT test_wsP,
--       "Keyword Parser" ~: runTestTT test_keyword,
--       "Integer Parser" ~: runTestTT testParseInt,
--       "Binary Op Parser" ~: runTestTT testParseBop
--     ]

-- testInput :: String
-- testInput = unlines
--   [ "grid(3, 3) {"
--   , "  constraint unique(1 to 9) in all"
--   , ""
--   , "  repeat(0, 2) {"
--   , "    constraint addsTo(15) in row $0"
--   , "  }"
--   , ""
--   , "  repeat(0, 2) {"
--   , "    constraint addsTo(15) in col $0"
--   , "  }"
--   , ""
--   , "  constraint addsTo(15) in cells [cell(0, 0), cell(1, 1), cell(2, 2)]"
--   , "  constraint addsTo(15) in cells [cell(0, 2), cell(1, 1), cell(2, 0)]"
--   , ""
--   , "  init cell(0, 1) with 9"
--   , "  init cell(1, 0) with 7"
--   , "  init cell(1, 2) with 3"
--   , "  init cell(2, 2) with 8"
--   , "}"
--   ]

testInput :: String
testInput = unlines
  [ "grid(4, 4) {"
  , "  repeat(0, 3) {"
  , "    constraint unique(1 to 4) in row $0"
  , "  }"
  , "  repeat(0, 3) {"
  , "    constraint unique(1 to 4) in col $0"
  , "  }"
  , "  repeat(0, 1) {"
  , "    repeat(0, 1) {"
  , "        constraint unique(1 to 4) in subgrid((($0 * 2), ($1 * 2)), 2, 2)"
  , "    }"
  , "  }"
  , ""
  , "  init cell(0, 3) with 3"
  , "  init cell(1, 1) with 4"
  , "  init cell(2, 2) with 3"
  , "  init cell(2, 3) with 2"
  , "}"
  ]


-- >>> parse parsePuzzle testInput
-- Right (Grid {width = 4, height = 4, constraints = [Repeat (Range (Number 0,Number 3)) (CC (ConstrainedCells {constraint = PC (Unique (Range (Number 1,Number 4))), cellGroup = Row (RepeatVar 0) Nothing})),Repeat (Range (Number 0,Number 3)) (CC (ConstrainedCells {constraint = PC (Unique (Range (Number 1,Number 4))), cellGroup = Col (RepeatVar 0) Nothing})),Repeat (Range (Number 0,Number 1)) (Repeat (Range (Number 0,Number 1)) (CC (ConstrainedCells {constraint = PC (Unique (Range (Number 1,Number 4))), cellGroup = Subgrid (Op2 (RepeatVar 0) Times (Number 2)) (Op2 (RepeatVar 1) Times (Number 2)) (Number 2) (Number 2)}))),CellInit (ACell (Number 0) (Number 3)) (Number 3),CellInit (ACell (Number 1) (Number 1)) (Number 4),CellInit (ACell (Number 2) (Number 2)) (Number 3),CellInit (ACell (Number 2) (Number 3)) (Number 2)]})

testConstraintRule :: String
testConstraintRule = "repeat(0, 2) { constraint addsTo(15) in row $0 }"

-- >>> parse parseConstraintRule testConstraintRule
-- Right (Repeat (Range (Number 0,Number 2)) (CC (ConstrainedCells {constraint = PC (AddsTo (Number 15)), cellGroup = Row (RepeatVar 0) Nothing})))


-- >>> parse parseConstraintRule "constraints [unique(1 to 9)] in all"
-- Right (CC (ConstrainedCells {constraint = ConstraintList [Unique (Range (Number 1,Number 9))], cellGroup = All}))

-- PrettyPrint for NumExp
prettyNumExp :: NumExp -> Doc
prettyNumExp (Number n) = Text.PrettyPrint.int n
prettyNumExp (RepeatVar n) = Text.PrettyPrint.char '$' <> Text.PrettyPrint.int n
prettyNumExp (Op2 l op r) = Text.PrettyPrint.parens $ prettyNumExp l <+> prettyBop op <+> prettyNumExp r

-- PrettyPrint for Bop
prettyBop :: Bop -> Doc
prettyBop Plus = Text.PrettyPrint.char '+'
prettyBop Minus = Text.PrettyPrint.char '-'
prettyBop Times = Text.PrettyPrint.char '*'
prettyBop Divide = Text.PrettyPrint.text "//"
prettyBop Modulo = Text.PrettyPrint.char '%'
prettyBop Power = Text.PrettyPrint.char '^'

-- PrettyPrint for Range
prettyRange :: Range -> Doc
prettyRange (Range (start, end)) = prettyNumExp start <+> text "to" <+> prettyNumExp end

prettyCellGroup :: CellGroup -> Doc
prettyCellGroup (ACell r c) =
  text "cell" <> Text.PrettyPrint.parens (prettyNumExp r <> (comma <+> prettyNumExp c))
prettyCellGroup (CellList cells) =
    text "cells" <> Text.PrettyPrint.brackets (hcat $ punctuate comma (map prettyCellGroup cells))
prettyCellGroup (Subgrid r c w h) =
    text "subgrid" <> Text.PrettyPrint.parens (hsep $ punctuate comma [prettyNumExp r, prettyNumExp c, prettyNumExp w, prettyNumExp h])
prettyCellGroup (Row idx range) =
    text "row" <+> prettyNumExp idx <+> maybe Text.PrettyPrint.empty (\r -> text "from" <+> prettyRange r) range
prettyCellGroup (Col idx range) =
    text "col" <+> prettyNumExp idx <+> maybe Text.PrettyPrint.empty (\r -> text "from" <+> prettyRange r) range
prettyCellGroup (Inverse cells) =
    Text.PrettyPrint.char '~' <> prettyCellGroup cells
prettyCellGroup All =
    text "all"
prettyCellGroup Unconstrained =
    text "unconstrained"




-- PrettyPrint for PrimitiveConstraint
prettyPrimitiveConstraint :: PrimitiveConstraint -> Doc
prettyPrimitiveConstraint (Unique range) =
  text "unique" <> Text.PrettyPrint.parens (prettyRange range)
prettyPrimitiveConstraint (AddsTo n) =
  text "addsTo" <> Text.PrettyPrint.parens (prettyNumExp n)
prettyPrimitiveConstraint (MultsTo n) =
  text "multTo" <> Text.PrettyPrint.parens (prettyNumExp n)
prettyPrimitiveConstraint (GreaterThan r1 r2) =
  text "greaterThan" <> Text.PrettyPrint.parens (prettyNumExp r1 <> (comma <+> prettyNumExp r2))
prettyPrimitiveConstraint (LessThan r1 r2) =
  text "lessThan" <> Text.PrettyPrint.parens (prettyNumExp r1 <> (comma <+> prettyNumExp r2))
prettyPrimitiveConstraint Always =
  text "always"
prettyPrimitiveConstraint Never =
  text "never"

-- PrettyPrint for Constraint
prettyConstraint :: Constraint -> Doc
prettyConstraint (PC p) = text "constraint" <+> prettyPrimitiveConstraint p
prettyConstraint (ConstraintList ps) =
  text "constraints" <> Text.PrettyPrint.brackets (hcat $ punctuate comma (map prettyPrimitiveConstraint ps))

-- PrettyPrint for ConstrainedCells
prettyConstrainedCells :: ConstrainedCells -> Doc
prettyConstrainedCells (ConstrainedCells c group) =
  prettyConstraint c <+> text "in" <+> prettyCellGroup group

-- PrettyPrint for ConstraintRule
prettyConstraintRule :: ConstraintRule -> Doc
prettyConstraintRule (CellInit cells n) =
    text "init" <+> prettyCellGroup cells <+> text "with" <+> prettyNumExp n
prettyConstraintRule (Repeat range rule) =
    text "repeat" <+> Text.PrettyPrint.parens (prettyRange range) <+> Text.PrettyPrint.braces (prettyConstraintRule rule)
prettyConstraintRule (CC cc) =
    prettyConstrainedCells cc


prettyPuzzle :: PuzzleSyntax -> Doc
prettyPuzzle (Grid w h rules) =
  text "grid" <+> Text.PrettyPrint.parens ((Text.PrettyPrint.int w <> comma) <+> Text.PrettyPrint.int h) <+> Text.PrettyPrint.braces (vcat (map prettyConstraintRule rules))


-- Helper Function
prettyPrint :: PuzzleSyntax -> String
prettyPrint = render . prettyPuzzle


-- Test Inputs

-- Test 1: Basic grid with no constraints
testPuzzle1 :: PuzzleSyntax
testPuzzle1 = Grid 3 3 []

-- Test 2: Grid with a single cell initialization
testPuzzle2 :: PuzzleSyntax
testPuzzle2 = Grid 4 4 [CellInit (ACell (Number 0) (Number 1)) (Number (-5))]

-- >>> parse parsePuzzle (prettyPrint testPuzzle2)
-- Right (Grid {width = 4, height = 4, constraints = [CellInit (ACell (Number 0) (Number 1)) (Number (-5))]})

-- Test 3: Grid with constraints and a repeat rule
testPuzzle3 :: PuzzleSyntax
testPuzzle3 = Grid 4 4
  [ CC (ConstrainedCells (PC (AddsTo (Number 10))) (Row (Number 0) Nothing)),
    Repeat (Range (Number 0, Number 2)) (CC (ConstrainedCells (PC Always) All))
  ]

-- Test 4: Complex grid with multiple nested repeats and constraints
testPuzzle4 :: PuzzleSyntax
testPuzzle4 = Grid 3 3
  [ CC (ConstrainedCells (PC (AddsTo (Number 15))) All),
    Repeat (Range (Number 0, Number 2)) (CC (ConstrainedCells (PC (AddsTo (Number 15))) (Row (RepeatVar 0) Nothing))),
    Repeat (Range (Number 0, Number 2)) (CC (ConstrainedCells (PC (AddsTo (Number 15))) (Col (RepeatVar 0) Nothing))),
    CC (ConstrainedCells (PC (AddsTo (Number 15))) (CellList [ACell (Number 0) (Number 0), ACell (Number 1) (Number 1), ACell (Number 2) (Number 2)])),
    CC (ConstrainedCells (PC (AddsTo (Number 15))) (CellList [ACell (Number 0) (Number 2), ACell (Number 1) (Number 1), ACell (Number 2) (Number 0)])),
    CellInit (ACell (Number 0) (Number 1)) (Number 9),
    CellInit (ACell (Number 1) (Number 0)) (Number 7),
    CellInit (ACell (Number 1) (Number 2)) (Number 3),
    CellInit (ACell (Number 2) (Number 2)) (Number 8)
  ]


testPrettyPrint :: IO ()
testPrettyPrint = do
  -- Test 1: Basic grid
  putStrLn "Test 1: Basic grid with no constraints"
  putStrLn $ prettyPrint testPuzzle1

  -- Test 2: Grid with a single cell initialization
  putStrLn "\nTest 2: Grid with a single cell initialization"
  putStrLn $ prettyPrint testPuzzle2

  -- Test 3: Grid with constraints and a repeat rule
  putStrLn "\nTest 3: Grid with constraints and a repeat rule"
  putStrLn $ prettyPrint testPuzzle3

  -- Test 4: Complex grid with multiple nested repeats and constraints
  putStrLn "\nTest 4: Complex grid with multiple nested repeats and constraints"
  putStrLn $ prettyPrint testPuzzle4

prop_roundtrip :: PuzzleSyntax -> Property
prop_roundtrip puzzle =
  let prettyStr = prettyPrint puzzle
      parsed = parse parsePuzzle prettyStr
  in --trace ("Testing puzzle: " ++ show puzzle) $  -- Print the puzzle before anything happens  -- Print the puzzle before anything happens
       -- Print the puzzle before anything happens
     --trace ("Pretty printed: " ++ prettyStr) $  -- Print the pretty string as well  -- Print the pretty string as well
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
    code <- readFile filename
    let parsed = parse parsePuzzle code
    putStrLn ("\nTesting " ++ filename)
    runTestTT (parsed ~?= Right expected)

-- Main to Run Tests
main' :: IO ()
main' = do
  testPrettyPrint
  _ <- testAll
  _ <- testParseSample "samples/empty.pz" pEmpty
  _ <- testParseSample "samples/sudoku-small.pz" pSudSmall
  _ <- testParseSample "samples/kakuro-small.pz" pKakSmall
  _ <- testParseSample "samples/magicsquare.pz" pMagSquare
  _ <- testParseSample "samples/futoshiki.pz" pFutoshiki
  _ <- testParseSample "samples/kenken.pz" pKenKen
  quickCheck prop_roundtrip
  return ()
