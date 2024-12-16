# **Puzzle Solver in Haskell**

## **Authors**
- **Steven Galban** – *PennKey: sgalban*
- **Quynh Anh Huynh** – *PennKey: 61117675*

---

## **Project overview**
This project is a **Haskell-based Puzzle Solver** which can be used to solve grid-based logic puzzles including (but not limited to) **Sudoku**, **Magic Squares**, **Kakuro**. Such puzzles can be described via a custom DSL, which can then be evaluated and solved. The primary components of this project include:
- A **DSL (Domain-Specific Language) syntax** to define puzzles and their constraints.
- A **Parser** to parse puzzle definitions from code written in the DSL.
- An **Evaluator** to simplify syntax trees into a more manageable form.
- A **Solver** that both validates and computes solutions to puzzles.
- A **Command-Line Interface (CLI)** for interactive puzzle solving.
- A **Printer** to display puzzles and solutions as **ASCII grids**.

---

## **Project structure**

The project consists of the following modules, which should be read in the order they're listed:

1. **`PuzzleSyntax.hs`**  
   Defines the syntax tree of our custom DSL. Some structures include:
   - **`NumExp`**: Represents numeric expressions.
   - **`Constraint`**: Describes restrictions that can be placed on the values of certain cells.
   - **`CellGroup`**: Describes the set of cells affected by a particular constraint or constraint list.

   The DSL is described in detail (in EBNF) in `doc/puzzle-spec.txt`
   Sample puzzles are also available to browse in the `samples` directory.

2. **`PuzzleParser.hs`**  
   Provides a **parser** for reading puzzle definitions from text files.  
   - Parses grid definitions, constraints, and cell groups into syntax trees.
   - Includes test cases to validate parsing functionality.

3. **`PuzzleEvaluator.hs`**
   Evaluates syntax trees into a more manageable form, `PuzzleE`.
   - `NumExp`s are transformed into simple integers.
   - `CellGroup`s are transformed into sets of particular cells.

4. **`PuzzleSolver.hs`**
   - Defines the **`PuzzleSolution`** type, which represents a (potentially partial) solution to a puzzle.
   Implements the puzzle-solving logic:  
   - Checks the validity of solutions, i.e. whether or not they violate any constraints.
   - Uses a combination of a depth-first-search and heuristics associated with the constraints to find a solution to an evaluated puzzle, if one exists.

5. **`PuzzlePrinter.hs`**  
   Includes a variety of functions used to vizualize the various puzzle representations in more readable forms:
   - **`printPuzzleE`** and **`printPuzzleSolution`** both print puzzles as ASCII grids. The former only prints the initial cell values of a `PuzzleE`, while the latter prints current values in a `PuzzleSolution.
   - Includes a pretty printer, that can print a syntax tree (i.e. `PuzzleSyntax`) as Puzzle code

6. **`PuzzleCLI.hs`**  
   Provides an interactive **Command-Line Interface** (CLI) to load, validate, solve, and manipulate puzzles, along with the ability to undo actions. See the `help` command for more details.

7. **`Main.hs`**  
   The project entry point. It runs the **CLI** for interactive use.

---

## **Dependencies**
This project depends on the following libraries:
- Base: Standard Haskell libraries, including Prelude, Control.Applicative, Control.Monad, Data.Char, and System.IO.
- Containers: Provides efficient data structures, including Map and Set operations.
- QuickCheck: For property-based testing.
- HUnit: For unit testing.
- PrettyPrint: For rendering puzzles and solutions in a readable forma
- mtl: For monad transformers.
---
