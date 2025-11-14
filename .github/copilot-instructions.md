# Copilot Instructions for logic-tasks

## Repository Overview

This is `logic-tasks`, a Haskell library that provides logic-related tasks for educational purposes. The repository is used to generate various propositional logic exercises, including syntax tasks (formula composition, decomposition, normal forms) and semantics tasks (truth tables, resolution).

## Build System

This project uses **Stack** as its build tool with the following resolver:
- **Resolver**: `nightly-2025-10-07`
- **Build Tool**: Stack 3.x

## Prerequisites

### Required System Dependencies
- **TeX Live**: Required for LaTeX rendering
  - Packages: `scheme-basic`, `forest`, `preview`, `dvisvgm`
- **Stack**: Haskell build tool
- **GHC**: Installed automatically by Stack

## Building the Project

### Install Dependencies
```bash
stack --no-terminal --install-ghc test --bench --only-dependencies
```

### Build the Project
```bash
stack --no-terminal build
```

### Build with Tests and Coverage
```bash
stack --no-terminal test --coverage --bench --no-run-benchmarks --haddock --no-haddock-deps
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

### Interactive Testing
You can test individual modules using the `testModule` function in the REPL:
```bash
stack repl
```

Then in GHCi:
```haskell
testModule (Just AutoLeijen) German (genFillInst dFillConf) LogicTasks.Semantics.Fill.description LogicTasks.Semantics.Fill.partialGrade LogicTasks.Semantics.Fill.completeGrade parser
```

## Project Structure

### Main Source Directories
- `src/` - Main library source code
  - `LogicTasks/` - Core logic task modules
    - `Semantics/` - Semantic tasks (truth tables, resolution)
    - `Syntax/` - Syntax tasks (formulas, normal forms)
  - `Tasks/` - Task configurations and quiz implementations
  - `Formula/` - Formula parsing, printing, and manipulation
  - `Trees/` - Tree structure handling
- `test/` - Test specifications using Hspec
- `examples/` - Example applications
- `flex/` - Flex-Tasks integration

### Key Files
- `package.yaml` - Package configuration with dependencies and build settings
- `stack.yaml` - Stack resolver and extra dependencies
- `logic-tasks.cabal` - Generated Cabal file (do not edit directly)
- `.hlint.yaml` - HLint configuration for linting

## Code Style and Quality

### Compiler Flags
The project uses strict compiler settings:
- `-Wall` - All warnings enabled
- `-Werror` - Warnings treated as errors (except for specific cases)
- `-Widentities` - Warn about identities
- `-fdefer-typed-holes` - Defer typed holes to runtime

### Linting
Run HLint on the codebase:
```bash
hlint src/ test/
```

### Cabal File Consistency
The `.cabal` file is generated from `package.yaml`. To regenerate:
```bash
stack build --dry-run
```

Always ensure the generated `.cabal` file is committed after changes to `package.yaml`.

## CI/CD Workflows

The repository has several CI workflows:
- **Haskell CI** (`haskell.yml`) - Main build and test workflow
- **HLint** (`hlint.yml`) - Haskell linter
- **Consistency** (`consistency.yml`) - Checks `.cabal` file consistency
- **Spelling** (`spelling.yml`) - Spell checking
- **XrefCheck** (`xrefcheck.yml`) - Link checking

### Caching Strategy
The CI uses Stack caching to speed up builds:
- Cache key: `${{ matrix.os }}-${{ matrix.plan.build }}-stack-home-${{ hashFiles('stack.yaml') }}-${{ hashFiles('package.yaml') }}`
- Cache path: `~/.stack`

This same caching strategy should be used in any additional workflows to benefit from shared cache.

## Development Guidelines

### Making Changes
1. **Minimal modifications**: Make the smallest possible changes to achieve your goal
2. **Don't break existing code**: Ensure all tests pass before and after your changes
3. **Follow existing patterns**: Match the coding style of surrounding code
4. **Update documentation**: If you change behavior, update relevant documentation
5. **Test your changes**: Add or update tests for any new functionality

### Adding Dependencies
- Add new dependencies to `package.yaml` under the `dependencies` section
- Run `stack build --dry-run` to update the `.cabal` file
- Commit both `package.yaml` and the generated `.cabal` file

### Common Tasks

#### Testing a Specific Module
```haskell
-- In stack repl
testModule (Just AutoLeijen) German generatorFunction description partialGrade completeGrade parser
```

#### Building Documentation
```bash
stack haddock
```

#### Running Benchmarks
```bash
stack bench --no-run-benchmarks
```

## External Dependencies

The project depends on several external Git repositories:
- `output-blocks` - Output formatting
- `flex-tasks` - Flex task integration
- `autotool-capabilities` - Autotool integration

These are defined in `stack.yaml` and managed by Stack automatically.

## Troubleshooting

### Build Fails with Missing Packages
- Ensure TeX Live is installed with all required packages
- Run `stack clean` and rebuild

### Test Failures
- Check if failures are related to your changes
- Review test output carefully
- Use `stack test --test-arguments="--match PATTERN"` to run specific tests

### Cache Issues
- If build behaves unexpectedly, try: `stack clean --full`
- Clear Stack cache: `rm -rf ~/.stack`

## Resources

- [Stack Documentation](https://docs.haskellstack.org/)
- [Haskell Language](https://www.haskell.org/)
- [Project README](../README.md)
