# MiniTriangle Compiler

By Harry Bridgen

## Overview

This is a Haskell implementation of a full compiler and virtual machine for a small programming language called MiniTriangle. It was developed as part of the "Compilers" module at the University of Nottingham, for which I received a mark of 100%.

The project compiles MiniTriangle source files (`.mt`) to an intermediate assembly language (TAM), then executes them using a custom TAM virtual machine.

## Project Structure

The following components were entirely developed by me:

-   **Parser** — A hand-written parser for MiniTriangle, including expression parsing with full precedence and associativity handling.
-   **TypeChecker** — Performs static semantic analysis with detailed error reporting.
-   **Compiler** — Translates the MiniTriangle AST into TAM instructions.
-   **TAM (Triangle Abstract Machine)** — A custom stack-based virtual machine that executes the compiled code.
-   **State & StateIO** — Custom implementations of monadic state and state+IO threading.
-   **Main** — Command-line entry point with support for both compiling and running `.mt` and `.tam` files.

## Usage

Compile using GHC:

```bash
ghc Main.hs -o mtc
```

Run with:

```bash
./mtc program.mt     # Compiles .mt source file to .tam and saves output
./mtc program.tam    # Executes .tam file using the TAM VM
```

## Features

-   Full parser and grammar for a statically-typed, expression-oriented language
-   Type inference and checking with user-friendly diagnostics
-   Compilation to a simple but expressive TAM instruction set
-   Stack-based virtual machine with support for functions, recursion, and I/O
-   Modular and testable design for each stage of compilation

## Author

Harry Bridgen  
[github.com/harrybridgen](https://github.com/harrybridgen)

## Credits

-   University of Nottingham — Coursework specification and MiniTriangle language definition
