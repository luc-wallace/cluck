# cluck

A compiler for a small subset of C written in Haskell

## Features

Current:

- Lexing
- AST & parsing types

Future:

- Full parsing
- Custom errors
- Code generation

## Usage

Tested with GHC 9.4

```
ghcup install cabal
```

```
cabal run cluc <input file>.c
```
