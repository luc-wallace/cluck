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

## Resources used

[C89 Draft](https://port70.net/%7Ensz/c/c89/c89-draft.html)
[Functional Parsing - Computerphile](https://www.youtube.com/watch?v=dDtZLm7HIJs)

## Usage

Tested with GHC 9.4

```
ghcup install cabal
```

```
cabal run cluck <input file>.c
```
