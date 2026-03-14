# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

ParaSail is a pointer-free, pervasively-parallel programming language and compiler/interpreter system. The repository also contains related language variants: Ada202x, Sparkel, Javallel, and Parython. The entire implementation is written in Ada 2012/2022 using GNAT.

## Build Commands

**Prerequisites**: `gnat`, `gprbuild`, `tcsh/csh`, `libreadline-dev`

```bash
# Build everything (ParaSail + all language variants + docs + local install)
make

# Build only ParaSail interpreter
make build

# Build a specific language variant
make build_ada202x
make build_sparkel
make build_javallel
make build_parython

# Optimized build (default is debug with -O0)
make DEBUG=off

# Build with OpenMP parallelism
make OMP=on

# Install to local install/ directory
make local-install

# Build documentation PDFs
make doc
```

Individual language builds use `gprbuild` directly against `.gpr` files in `build/`:
```bash
gprbuild -p -g -O0 -gnato -gnata -gnatE -P build/parasail
```

## Running Tests

Tests require `install/bin` on your PATH:
```bash
export PATH="$PWD/install/bin:$PATH"

# Run all tests for all languages
cd testsuite && ./support/runalltests.sh

# Run all ParaSail tests
cd testsuite/ParaSail && ../support/runtests.sh

# Run specific tests
cd testsuite/ParaSail && ../support/runtests.sh clock for_website

# Run with compiler (not interpreter)
cd testsuite/ParaSail && ../support/runtests.sh -c

# Run a single test with compiler
cd testsuite/ParaSail && ../support/runtests.sh -c apply_op
```

Test output diffs are written to `./runtests.out`. To update expected output after a valid change:
```bash
cp tmp/tmp.out apply_op/test.out
```

Each test in `testsuite/ParaSail/<testname>/` contains `test.sh` (execution script) and optionally `test.out` (expected output).

## Architecture

### Compilation Pipeline

```
Source (.psl/.psi/.a2x/etc.)
    → Parser (YACC/Flex in parser/ or *_parser/)
    → AST (PSC.Trees hierarchy)
    → Semantic Analysis (semantics/)
    → PSVM Code Generation (semantics/)
    → PSVM Interpreter (interpreter/)
```

### Key Directories

| Directory | Purpose |
|-----------|---------|
| `parser/` | ParaSail lexer/YACC grammar and main entry point |
| `parser/gen/` | Auto-generated Ada from YACC/Flex (do not edit manually) |
| `semantics/` | Type system, name resolution, semantic analysis, PSVM code generation (~75K lines) |
| `interpreter/` | ParaSail Virtual Machine (PSVM) instruction set and execution engine (~32K lines) |
| `lib/` | Standard library written in ParaSail/PSI |
| `build/` | GNAT project files (`.gpr`) and build configuration |
| `testsuite/` | Regression tests organized by language (ParaSail, Ada202x, Sparkel, etc.) |
| `ada202x_parser/`, `sparkel_parser/`, `javallel_parser/`, `parython_parser/` | Language-variant parsers |
| `design/` | Architecture design documents describing the execution model |
| `documentation/` | Reference manuals, PDFs, white papers |
| `share/tools/` | Editor integrations (vim, emacs), vi tags scripts |

### Core Ada Packages

- **`PSC.Trees`** (`semantics/psc-trees.ads`) — Root AST node hierarchy for all language constructs
- **`PSC.Interpreter`** (`interpreter/psc-interpreter.ads`) — PSVM instruction set (variant record), storage regions, type descriptors, execution engine
- **`PSC.Interpreter.Builtins`** (`interpreter/psc-interpreter-builtins.adb`) — Built-in I/O, math, container, and string operations (~119K lines)
- **`PSC.Trees.Semantics.Static`** — Type checking and name resolution
- **`PSC.Trees.Semantics.Dynamic`** — PSVM code generation

### Entry Points

Each language has its own main program:
- `parser/parasail_main.adb` → ParaSail interpreter
- `ada202x_parser/ada202x_main.adb` → Ada202x interpreter
- Similar pattern for Sparkel, Javallel, Parython

### GNAT Project Structure

`build/shared.gpr` holds common compiler/linker settings. Language-specific `.gpr` files reference it and specify their `Source_Dirs` spanning `parser/`, `parser/gen/`, `semantics/`, and `interpreter/`.

### Language Variants

All variants (Ada202x, Sparkel, Javallel, Parython) share the same `semantics/` and `interpreter/` packages but have their own parsers in separate `*_parser/` directories. The `lib/` directory contains the standard library in ParaSail syntax (`.psl`/`.psi` files).

## CI

GitHub Actions (`.github/workflows/parasail-make-all.yml`) runs `make` on every push to `main` on `ubuntu-latest`.
