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
- Between multiple functions being added in the same PR

### Unsafe Functions and Partiality

**Avoid `fromJust` in most cases**:

- Use pattern matching on `Maybe`: `case maybeValue of Just x -> ...; Nothing -> ...`
- Use `fromMaybe` with a default: `fromMaybe defaultValue maybeValue`
- Use `maybe` to handle `Nothing` cases gracefully
- Only use `fromJust` when you have a very strong proof that the value is always `Just`, and document why

### List Operations

**Prefer `nubOrd` or `nubSort` over `nub`**:

- Use `nubOrd` for better performance (O(n log n) vs O(n²))
- Use `nubSort` when you also want the result sorted
- These require an `Ord` constraint but are much more efficient

**Avoid writing explicit recursion on lists**:

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
