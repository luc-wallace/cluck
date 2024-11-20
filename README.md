# cluck

A compiler for a subset of C written in Haskell.

Inspired by [MicroC](https://blog.josephmorag.com/posts/mcc0/)

## Features

Current:

- Variable and function declarations
- Return, if and block statements
- Expressions and precedence
- Number, char and string literals
- Binary and unary operators via +, -, \*, /, %, ==, !=, ||, &&, !, >, >=, <, <=
- Detailed syntax errors
- Static analysis for bindings and types

Future:

- For loops
- Pointer types
- Structs
- Code generation

## Resources used

[C89 Draft](https://port70.net/%7Ensz/c/c89/c89-draft.html)

[Writing a C Compiler - Nora Sandler](https://norasandler.com/2017/11/29/Write-a-Compiler.html)

[Functional Parsing - Computerphile](https://www.youtube.com/watch?v=dDtZLm7HIJs)

[Megaparsec tutorial](https://markkarpov.com/tutorial/megaparsec.html)

[Compiling a functional language to LLVM - Daniel J. Harvey](https://danieljharvey.github.io/posts/2023-02-08-llvm-compiler-part-1.html)

[Micro C - Joseph Morag](https://blog.josephmorag.com/posts/mcc0/)

## Usage

Only compatible with GHC 8.8.

Requires LLVM 14 to be installed including Clang 14.

```
ghcup install cabal
```

```
cabal run cluck <input file>.c
```
