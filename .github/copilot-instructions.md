# Copilot Instructions for logic-tasks

This is `logic-tasks`, a Haskell library that provides logic-related tasks for educational purposes. The repository generates various propositional logic exercises, including syntax tasks (formula composition, decomposition, normal forms) and semantics tasks (truth tables, resolution).

**Always reference these instructions first and fall back to search or Bash commands only when you encounter unexpected information that does not match the info here.**

## 🤖 Automated Copilot Setup

This repository includes an automated setup workflow (`.github/workflows/copilot-setup-steps.yml`) that prepares the environment for Copilot operations. The workflow automatically:

- **Installs TeX Live**: Required packages for LaTeX rendering (forest, preview, dvisvgm)
- **Verifies LaTeX installation**: Ensures pdflatex is working
- **Sets up Haskell Stack**: Configures the build environment
- **Installs HLint**: Enables code quality checking
- **Pre-builds dependencies**: Downloads and builds Haskell project dependencies

System dependencies and Haskell dependencies are already available. You can immediately proceed with builds and tests.

## 🚨 CRITICAL WARNINGS

### 🔴 NEVER COMMIT FILES THAT VIOLATE .EDITORCONFIG

**ABSOLUTE REQUIREMENT**: Every file you create or modify MUST comply with `.editorconfig` rules:

- **NO TRAILING WHITESPACE**
- **FINAL NEWLINE REQUIRED**

**BEFORE ANY COMMIT**: Run `./scripts/check-editorconfig.sh` to validate compliance:

```bash
./scripts/check-editorconfig.sh
```

**If violations found**: Use these commands to fix them immediately:

```bash
# For individual files:
sed -i 's/[[:space:]]*$//' filename  # Remove trailing whitespace
echo >> filename                     # Add final newline
```

Then run EditorConfig validation again to confirm fixes:

```bash
./scripts/check-editorconfig.sh
```

**IF `./scripts/check-editorconfig.sh` FAILS**:

- **DO NOT COMMIT**
- **DO NOT USE `report_progress`**
- **FIX ALL VIOLATIONS FIRST**

### 🔴 NEVER COMMIT CODE THAT DOESN'T BUILD

**ABSOLUTE REQUIREMENT**: Every commit MUST successfully build with `stack test --no-run-tests logic-tasks`.

**BEFORE ANY COMMIT**: Run `stack test --no-run-tests logic-tasks` to validate the code compiles:

```bash
stack test --no-run-tests logic-tasks
```

**If build fails**: Fix all compilation errors before committing:

- Review the error messages carefully
- Fix all type errors, missing imports, and syntax issues
- Re-run `stack test --no-run-tests logic-tasks` until it succeeds
- Only then proceed with committing

**IF `stack test --no-run-tests logic-tasks` FAILS**:

- **DO NOT COMMIT**
- **DO NOT USE `report_progress`**
- **FIX ALL BUILD ERRORS FIRST**

**Build times**: Remember that builds can take 30-45 minutes. Set appropriate timeout values (60+ minutes) and never cancel builds.

### ⏰ NEVER CANCEL BUILDS OR TESTS

- **Haskell builds**: Can take 30+ minutes on first run (set timeout to 60+ minutes)
- **Test suites**: Can take 10-20 minutes (set timeout to 30+ minutes)
- Builds resume from cache when interrupted - canceling wastes progress

### 🔴 ALWAYS RUN LINTERS BEFORE COMMITTING

**BEFORE ANY COMMIT**, run:

```bash
hlint src/ test/
```

**If violations found**, fix them immediately. **DO NOT COMMIT** or use `report_progress` until checks pass.

## Build tool

This project uses **Stack** as its build tool. The compiler (GHC) is automatically installed.

## Building the Project

### Quick Start

```bash
# Build the library
stack --no-terminal build

# Run tests
stack test
```

### Full Build with All Checks

```bash
# Build with tests, coverage, benchmarks, and documentation
stack --no-terminal test --coverage --bench --no-run-benchmarks --haddock --no-haddock-deps
```

**Build Timeouts**: Set initial_wait to at least 60 for builds and 30 for tests to avoid premature timeout.

## Testing

### Run All Tests

```bash
stack test
```

### Run Specific Test Module

```bash
# Match specific test module by name
stack test --test-arguments="-m Fill"

# Match all semantic tests
stack test --test-arguments="-m Semantics"

# Run with verbose output
stack test --test-arguments="--verbose"
```

**Finding Test Names**:

To see available test names and their hierarchy:

```bash
# List all test specs (shows full tree)
stack test --test-arguments="--dry-run"

# Filter and view specific category
stack test --test-arguments="-m Modelling.ActivityDiagram --dry-run"
```

**Best Practices**:

- **Simple substring matching works**: `SelectAS` will match `Modelling.ActivityDiagram.SelectAS`
- **Use `--dry-run` to verify**: Always test your pattern with `--dry-run` first to see what will run
- **Quote patterns with spaces**: Use `-m \"is valid\"` with escaped quotes for multi-word patterns
- **Be specific to avoid over-matching**: `SelectAS` is better than just `Select` which might match multiple modules
- **Substring matching is powerful**: `Modelling.CdOd` matches all class/object diagram tests

### Interactive Testing with testModule

You can test individual modules using the `testModule` function in the REPL:

```bash
stack repl
```

Then in GHCi:

```haskell
-- Example: Test the Fill module (truth table gap-filling)
testModule (Just AutoLeijen) German (genFillInst dFillConf) LogicTasks.Semantics.Fill.description LogicTasks.Semantics.Fill.partialGrade LogicTasks.Semantics.Fill.completeGrade parser
```

## Project Structure

### Main Source Directories

- `src/` - Main library source code
  - `LogicTasks/` - Core logic task modules
    - `Semantics/` - Semantic tasks (truth tables, resolution)
      - `Fill.hs` - Truth table gap-filling
      - `Pick.hs` - Truth table selection
      - `Decide.hs` - Truth table error finding
      - `Max.hs`, `Min.hs` - **deprecated**
      - `Step.hs`, `Resolve.hs`, `Prolog.hs` - Resolution tasks
    - `Syntax/` - Syntax tasks (formulas, normal forms)
      - `ComposeFormula.hs`, `DecomposeFormula.hs` - Formula composition
      - `IllegalCnfs.hs`, `IllegalDnfs.hs`, `IllegalFormulas.hs` - Invalid formula detection
      - `SimplestFormula.hs` - Bracket removal
      - `SubTreeSet.hs` - Subformula identification
      - `TreeToFormula.hs` - Tree to formula conversion
  - `Tasks/` - Task configurations and quiz implementations
  - `Formula/` - Formula parsing, printing, and manipulation
  - `Trees/` - Tree structure handling
- `test/` - Test specifications using Hspec
- `examples/` - Example applications
- `flex/` - Flex-Tasks integration

## Code Style and Quality

Run HLint on the codebase:

```bash
hlint src/ test/
```

## Haskell Development Best Practices

### Code Reuse and Refactoring

**Always look for refactoring opportunities**: When adding functions, check for opportunities to increase code reuse:

- Between the new function and preexisting ones
- Between multiple functions being added in the same pull request

### Unsafe Functions and Partiality

**Avoid `fromJust` in most cases**:

- Use pattern matching on `Maybe` instead: `case maybeValue of Just x -> ...; Nothing -> ...`
- Use `fromMaybe` with a default value: `fromMaybe defaultValue maybeValue`
- Use `maybe` to handle `Nothing` cases gracefully
- Only use `fromJust` when you have a very strong proof that the value is always `Just`, and document why

### List Operations

**Prefer `nubOrd` or `nubSort` over `nub`**:

- Use `nubOrd` for better performance
- Use `nubSort` when you also want the result sorted
- These require an `Ord` constraint but are much more efficient

**Prefer `map` over `fmap` or `<$>` for lists**:

- When working with lists specifically, use `map` instead of `fmap` or `<$>`
- Reserve `fmap` and `<$>` for non-list functors (Maybe, Either, IO, etc.)
- Example: Use `map (+1) [1,2,3]` instead of `fmap (+1) [1,2,3]` or `(+1) <$> [1,2,3]`

**Avoid writing explicit recursions on lists**:

- Only write explicit recursion when the logic truly doesn't fit existing abstractions
- Usually, list comprehensions or existing higher-order functions (`map`, `filter`, `fold`, etc.) are a better fit

### Function Definitions

**Use anonymous functions where appropriate**:

- Sometimes not introducing a name is a good way of not introducing a bad name
- Use lambda functions (`\x -> ...`) for simple, inline transformations
- Prefer named functions when they have clear, meaningful names
- Avoid deeply nested anonymous functions that hurt readability

**Consider inlining single-use definitions**:

- If a named entity (like a `let`-introduced value or top-level function) has:
  - A very short definition, AND
  - Is used only exactly once in the rest of the code
- Then it is sometimes better to simply inline it directly
- Balance this with readability - don't inline if it makes code harder to understand

### Naming Conventions

**CRITICAL**: All function and variable names (also local variable names) in Haskell code MUST be spell-checkable and avoid abbreviations.

**Rules for naming**:

- **NO abbreviations**: Never use abbreviated names like `len`, `subseq`, `cfg`, `inst`, `idx`, `tmp`, `cnt`, `num`, `str`, `val`, etc.
- **Use full, descriptive names**: Instead of `len`, use `length` or `theLength`; instead of `subseq`, use `subsequence`
- **CamelCase for clarity**: Use camelCase to combine words clearly (e.g., `theLength`, `currentIndex`, `temporaryValue`)
- **Prefer clarity over brevity**: `numberOfElements` is better than `numElems` or `nElems`
- **Spell check friendly**: All names should pass spell checking with standard dictionaries or be obvious compound words

**Examples of good naming**:

```haskell
-- Good: Full, descriptive names
checkDifferentNamesInstance :: DifferentNamesInstance -> Maybe String
differentNamesEvaluation :: OutputCapable m => DifferentNamesInstance -> [(Name, Name)] -> Rated m
defaultDifferentNamesConfig :: DifferentNamesConfig
randomiseLayout :: RandomiseLayout a => a -> Int -> IO a

-- Good: Clear compound words with CamelCase
theLength :: Int
currentIndex :: Int
temporaryValue :: String
numberOfElements :: Int
```

**Examples of bad naming (DO NOT USE)**:

```haskell
-- Bad: Abbreviated names
len :: Int              -- Use: length, theLength, listLength
subseq :: [a] -> [a]    -- Use: subsequence
cfg :: Config           -- Use: config, configuration
inst :: Instance        -- Use: instance, theInstance
idx :: Int              -- Use: index, currentIndex
tmp :: String           -- Use: temporary, temporaryValue
cnt :: Int              -- Use: count, counter
num :: Int              -- Use: number, numberOfItems
```

**Exceptions**:

- Standard Haskell library functions and types (e.g., `putStrLn`, `Seq`)
- Standard Haskell conventions (e.g., `xs`, `x`, single-letter type variables)
- Loop variables in very short, localized contexts (e.g., `i`, `j`, `k` in list comprehensions)
- Widely accepted mathematical notation in domain-specific contexts (e.g., `n` for count in mathematical functions)
- Standard abbreviations from the problem domain (e.g., `cd` for "class diagram", `od` for "object diagram" when these are established terms in the codebase)

## Development Guidelines

### Making Changes

1. **Minimal modifications**: Make the smallest possible changes to achieve your goal
2. **Don't break existing code**: Ensure all tests pass before and after your changes
3. **Follow existing patterns**: Match the coding style of surrounding code
4. **Update documentation**: If you change behavior, update relevant documentation
5. **Test your changes**: Add or update tests for any new functionality
6. **Run linters**: Use HLint to catch code quality issues early

### Working with REPL

```bash
# Start REPL with library loaded
stack repl

# Test a specific module interactively
testModule (Just AutoLeijen) German generatorFunction description partialGrade completeGrade parser
```

## Troubleshooting

### Test Failures

- Check if failures are related to your changes or preexisting
- Review test output carefully for error messages
- Run specific failing tests: `stack test --test-arguments="-m TestName"`
- Use `--verbose` flag for more detailed output
