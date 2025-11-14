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

### 🔴 NEVER COMMIT FILES THAT VIOLATE EditorConfig

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

### ⏰ NEVER CANCEL BUILDS OR TESTS

- **Haskell builds**: Can take 30-45 minutes on first run (set timeout to 60+ minutes)
- **Test suites**: Can take 10-20 minutes (set timeout to 30+ minutes)
- Builds resume from cache when interrupted - canceling wastes progress

### 🔴 ALWAYS RUN LINTERS BEFORE COMMITTING

**BEFORE ANY COMMIT**, run:

```bash
hlint src/ test/
```

**If violations found**, fix them immediately.

### ⚠️ COMMIT CHECKLIST

**Before using `report_progress` or committing changes**:

1. ✅ Run `./scripts/check-editorconfig.sh` - must pass
2. ✅ Run `stack test --no-run-tests logic-tasks` - must build successfully
3. ✅ Run `hlint src/ test/` - must have no violations

**If any check fails**: **DO NOT COMMIT** or use `report_progress` - fix all issues first.

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

# List all test specs (shows full tree)
stack test --test-arguments="--dry-run"
```

**Best Practices**:

- **Simple substring matching works**: `Fill` will match tests in `FillSpec`
- **Use `--dry-run` to verify**: Always test your pattern with `--dry-run` first to see what will run
- **Quote patterns with spaces**: Use `-m \"truth table\"` with escaped quotes for multi-word patterns
- **Be specific to avoid over-matching**: `Fill` is better than just `F` which might match multiple modules
- **Substring matching is powerful**: `Semantics` matches all semantics-related tests

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
-- Good: Full, descriptive names from logic-tasks
genFillInst :: FillConfig -> Gen FillInst
description :: OutputCapable m => Bool -> FillInst -> LangM m
partialGrade :: OutputCapable m => FillInst -> [TruthValue] -> LangM m
completeGrade :: (OutputCapable m, Alternative m, Monad m) => FillInst -> [TruthValue] -> Rated m

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
