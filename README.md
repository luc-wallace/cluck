# cluck

A compiler for a small subset of C written in Haskell

## Features

Current:

- Variable and function declarations
- Return, if and block statements
- Expressions and precedence
- Number, char and string literals
- Binary and unary operators via +, -, \*, /, %, ==, !=, ||, &&, !, >, >=, <, <=
- Detailed syntax errors

Future:

- Function arguments
- For loops
- Pointer types
- Structs
- Semantic analysis
- Code generation

## Resources used

[C89 Draft](https://port70.net/%7Ensz/c/c89/c89-draft.html)

[Writing a C Compiler, Part 1](https://norasandler.com/2017/11/29/Write-a-Compiler.html)

[Functional Parsing - Computerphile](https://www.youtube.com/watch?v=dDtZLm7HIJs)

[Megaparsec tutorial](https://markkarpov.com/tutorial/megaparsec.html)

## Usage

Tested with GHC 9.4

```
ghcup install cabal
```

```
cabal run cluck <input file>.c
```
