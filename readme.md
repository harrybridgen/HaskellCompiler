# MiniTriangle Compiler

By Harry Bridgen

---

## Overview

This is a Haskell implementation of a full compiler and virtual machine for a small programming language called MiniTriangle. It was developed as part of the "Compilers" module at the University of Nottingham, for which I received a mark of 100%.

The project compiles MiniTriangle source files (`.mt`) to an intermediate assembly language (TAM), then executes them using a custom TAM virtual machine.

---

## Project Structure

The following components were entirely developed by me:

-   **Parser** - A hand-written parser for MiniTriangle, including expression parsing with full precedence and associativity handling.
-   **TypeChecker** - Performs static semantic analysis with detailed error reporting.
-   **Compiler** - Translates the MiniTriangle AST into TAM instructions.
-   **TAM (Triangle Abstract Machine)** - A custom stack-based virtual machine that executes the compiled code.
-   **State & StateIO** - Custom implementations of monadic state and state+IO threading.
-   **Main** - Command-line entry point with support for both compiling and running `.mt` and `.tam` files.
-   **Grammar** - Contains declarations for the grammar of the MiniTriangle language
-   **State** - Contains declaration and functions for the state monad
-   **StateIO** - Contains declaration and functions for the stateIO monad

---

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

---

## Features

-   Full parser and grammar for a statically-typed, expression-oriented language
-   Type inference and checking with user-friendly diagnostics
-   Compilation to a simple but expressive TAM instruction set
-   Stack-based virtual machine with support for functions, recursion, and I/O
-   Modular and testable design for each stage of compilation

---

## Grammar
The grammar for the MiniTriangle language is as follows:
```bash
-- ----------------------------------------------
-- expr        ::= term | term + expr | term - expr
-- mExpr       ::= expr | expr * mExpr | expr / mExpr
-- term        ::= int | bool | identifier | -term | (expr) | identifier (exprs)
-- exprs       ::= expr | expr , exprs
-- ----------------------------------------------
-- program     ::= let declarations in command
-- declaration ::= var identifier : type
--               | var identifier : type := expr
--               | fun identifier (vardecls) : type := expr
-- vardecl     ::= identifier : type
-- vardecls    ::= vardecl | vardecl , vardecls
-- type        ::= Integer | Boolean
-- ----------------------------------------------
-- command     ::= identifier := expr
--               | if expr then command else command
--               | while expr do command
--               | getint identifier
--               | printint expr
--               | begin commands end
-- commands    ::= command | command ; commands
-- ----------------------------------------------
```

---

## Author

Harry Bridgen  
[github.com/harrybridgen](https://github.com/harrybridgen)
