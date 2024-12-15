# **Puzzle Solver in Haskell**

## **Authors**
- **Steven Galban** – *PennKey: *
- **Quynh Anh Huynh** – *PennKey: 61117675*

---

## **Project overview**
This project is a **Haskell-based Puzzle Solver** designed to handle grid-based logic puzzles such as **Sudoku**, **Magic Square**, and **Kakuro**. The project provides the following components:
- A **DSL (Domain-Specific Language) syntax** to define puzzles and constraints.
- A **Parser** to parse puzzle definitions from files.
- An **Evaluator** to validate and process puzzle constraints.
- A **Solver** to compute valid solutions to the puzzles.
- A **Command-Line Interface (CLI)** for interactive puzzle solving.
- A **Printer** to display puzzles and solutions as **ASCII grids**.

---

## **Project structure**

The project is organized into the following files:

1. **`PuzzleSyntax.hs`**  
   Defines the core data structures and types for puzzles:
   - **`NumExp`**: Represents numeric expressions.
   - **`PuzzleSyntax`**: The grid definition and constraints.
   - **`ConstraintRule`**: Rules applied to cells.

   **Start here** to understand the data model for puzzles.

2. **`PuzzleParser.hs`**  
   Provides a **parser** for reading puzzle definitions from text files.  
   - Parses grid definitions, constraints, and cell groups.
   - Includes test cases to validate parsing functionality.

   **Read this second** to see how puzzles are defined and parsed.

3. **`PuzzleEvaluator.hs`**  
   Evaluates parsed puzzles and constraints.  
   - Processes `NumExp`, ranges, and cell groups.
   - Validates constraints and prepares puzzles for solving.

   **Read this third** to understand how constraints are evaluated.

4. **`PuzzleSolver.hs`**
   - Defines **`PuzzleSolution`** that represents solutions to puzzles.
   Implements the puzzle-solving logic:  
   - Checks the validity of solutions.
   - Attempts to solve puzzles using the defined rules.
   - Includes properties tested with **QuickCheck**.

   **Read this fourth** to see the solving strategy.

5. **`PuzzlePrinter.hs`**  
   Provides functions to display puzzles and solutions as **ASCII grids**:
   - **`printPuzzleE`**: Prints a `PuzzleE` grid with constraints.
   - **`printPuzzleSolution`**: Prints a solved puzzle grid with cell values.

   Use this module for **visualizing puzzles** in an easy-to-read grid format.

6. **`PuzzleCLI.hs`**  
   Provides an interactive **Command-Line Interface** (CLI) to load, validate, solve, and manipulate puzzles.  
   Commands include:
   - `:l filename` – Load a puzzle file.
   - `:v` – Validate the current solution.
   - `:s` – Solve the puzzle.
   - `:a r c v` – Add a value to a cell.
   - `:d r c` – Delete a value from a cell.

   **Read this last** to understand how the interactive user interface works.

7. **`samples/`**  
   Contains sample puzzle files, such as:
   - `sudoku-small.pz`
   - `magicsquare.pz`
   - `kakuro-small.pz`

8. **`Main.hs`**  
   The project entry point. It runs the **CLI** for interactive usage.

---

## **Dependencies**
This project depends on the following libraries:
- Base: Standard Haskell libraries, including Prelude, Control.Applicative, Control.Monad, Data.Char, and System.IO.
- Containers: Provides efficient data structures, including Map and Set operations.
- QuickCheck: For property-based testing.
- HUnit: For unit testing.
- PrettyPrint: For rendering puzzles and solutions in a readable forma
---

## **Compilation and execution**

1. **Build the project**  
   Run the following command in the project root directory to build the project:

