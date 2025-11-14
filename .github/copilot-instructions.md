# Copilot Instructions for logic-tasks

## Repository Overview

This is `logic-tasks`, a Haskell library that provides logic-related tasks for educational purposes. The repository generates various propositional logic exercises, including:

- **Syntax tasks**: Formula composition, decomposition, normal forms, subformulas
- **Semantics tasks**: Truth tables, resolution, maxterms, minterms

The library is designed for integration with the Autotool system and provides both direct task generation and quiz-based interfaces.

## Build System

This project uses **Stack** as its build tool:

- **Resolver**: `nightly-2025-10-07`
- **Build Tool**: Stack 3.x
- **Compiler**: GHC (installed automatically by Stack)

## Prerequisites

### Required System Dependencies

- **TeX Live**: Required for LaTeX rendering of formulas and diagrams
  - Packages: `scheme-basic`, `forest`, `preview`, `dvisvgm`
- **Stack**: Haskell build tool
- **HLint**: Code quality checker (installed via workflow)

## Building the Project

### Quick Start

```bash
# Install dependencies (first time or after package.yaml changes)
stack --no-terminal --install-ghc test --bench --only-dependencies

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

### Development Workflow

```bash
# Start interactive REPL
stack repl

# Run HLint on source code
hlint src/ test/

# Clean build artifacts
stack clean

# Full clean (including Stack cache)
stack clean --full
```

## Testing

### Run All Tests

```bash
stack test
```

### Run Tests with Coverage

```bash
stack test --coverage
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

-- Change language to English
testModule (Just AutoLeijen) English (genFillInst dFillConf) LogicTasks.Semantics.Fill.description LogicTasks.Semantics.Fill.partialGrade LogicTasks.Semantics.Fill.completeGrade parser
```

## Project Structure

### Main Source Directories

- `src/` - Main library source code
  - `LogicTasks/` - Core logic task modules
    - `Semantics/` - Semantic tasks (truth tables, resolution)
      - `Fill.hs` - Truth table gap-filling
      - `Pick.hs` - Truth table selection
      - `Decide.hs` - Truth table error finding
      - `Max.hs`, `Min.hs` - Maxterm/Minterm generation
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

### Key Files

- `package.yaml` - Package configuration with dependencies and build settings (**Edit this, not .cabal**)
- `logic-tasks.cabal` - Generated Cabal file (**DO NOT EDIT** - auto-generated from package.yaml)
- `stack.yaml` - Stack resolver and extra dependencies
- `.hlint.yaml` - HLint configuration for linting

## Code Style and Quality

### Compiler Flags

The project uses strict compiler settings:

- `-Wall` - All warnings enabled
- `-Werror` - Warnings treated as errors (except for specific cases)
- `-Widentities` - Warn about identities
- `-fdefer-typed-holes` - Defer typed holes to runtime
- `-fno-warn-unused-do-bind` - Allow unused do-bind

### Linting

Run HLint on the codebase:

```bash
hlint src/ test/
```

Configuration is in `.hlint.yaml`. HLint is automatically run in CI.

### Cabal File Consistency

The `.cabal` file is **auto-generated** from `package.yaml`. **DO NOT EDIT** `.cabal` directly.

To regenerate after changing `package.yaml`:

```bash
stack build --dry-run
```

**Always commit both** `package.yaml` and the generated `.cabal` file together.

## Haskell Development Best Practices

### Code Reuse and Refactoring

**Always look for refactoring opportunities**: When adding functions, check for opportunities to increase code reuse:

- Between the new function and pre-existing ones
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

## CI/CD Workflows

The repository has several CI workflows:

- **Haskell CI** (`haskell.yml`) - Main build and test workflow
- **HLint** (`hlint.yml`) - Haskell linter
- **Consistency** (`consistency.yml`) - Checks `.cabal` file consistency
- **Spelling** (`spelling.yml`) - Spell checking
- **XrefCheck** (`xrefcheck.yml`) - Link checking
- **Super-Linter** (`linter.yml`) - Multiple linters including YAML

### Caching Strategy

The CI uses Stack caching to speed up builds:

- **Cache paths**: `~/.stack` (Stack home), `.stack-work` (project build artifacts)
- **Cache key pattern**: `${{ runner.os }}-stack-home-${{ hashFiles('stack.yaml') }}-${{ hashFiles('package.yaml') }}`
- **Restore keys**: Allow partial cache matches when files change

The Copilot setup workflow uses the same caching strategy to benefit from shared cache with the main CI.

## Development Guidelines

### Making Changes

1. **Minimal modifications**: Make the smallest possible changes to achieve your goal
2. **Don't break existing code**: Ensure all tests pass before and after your changes
3. **Follow existing patterns**: Match the coding style of surrounding code
4. **Update documentation**: If you change behavior, update relevant documentation
5. **Test your changes**: Add or update tests for any new functionality
6. **Run linters**: Use HLint to catch code quality issues early

### Adding Dependencies

1. Add new dependencies to `package.yaml` under the `dependencies` section
2. Run `stack build --dry-run` to update the `.cabal` file
3. Commit both `package.yaml` and the generated `.cabal` file together

### Common Development Tasks

#### Working with REPL

```bash
# Start REPL with library loaded
stack repl

# Test a specific module interactively
testModule (Just AutoLeijen) German generatorFunction description partialGrade completeGrade parser
```

#### Building Documentation

```bash
# Generate Haddock documentation
stack haddock

# Generate and open in browser
stack haddock --open
```

#### Running Benchmarks

```bash
stack bench --no-run-benchmarks
```

#### Cleaning Build Artifacts

```bash
# Clean project build artifacts
stack clean

# Full clean including Stack cache
stack clean --full
```

## External Dependencies

The project depends on several external Git repositories defined in `stack.yaml`:

- `output-blocks` - Output formatting
- `flex-tasks` - Flex task integration
- `autotool-capabilities` - Autotool integration

These are managed automatically by Stack and cloned as needed during builds.

## Troubleshooting

### Build Fails with Missing Packages

- Ensure TeX Live is installed with all required packages: `scheme-basic`, `forest`, `preview`, `dvisvgm`
- Run `stack clean` and rebuild
- Check that system dependencies are available: `pdflatex --version`

### Test Failures

- Check if failures are related to your changes or pre-existing
- Review test output carefully for error messages
- Run specific failing tests: `stack test --test-arguments="-m TestName"`
- Use `--verbose` flag for more detailed output

### Cache Issues

- If build behaves unexpectedly: `stack clean --full`
- Clear Stack cache completely: `rm -rf ~/.stack`
- Clear project build cache: `rm -rf .stack-work`

### Dependency Resolution Issues

- Update Stack: `stack upgrade`
- Clean resolver cache: `rm -rf ~/.stack/pantry`
- Try rebuilding from scratch: `stack clean --full && stack build`

## Resources

- [Stack Documentation](https://docs.haskellstack.org/)
- [Haskell Language](https://www.haskell.org/)
- [HLint Manual](https://github.com/ndmitchell/hlint)
- [Project README](../README.md)
