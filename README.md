# cluck

A compiler for a subset of C written in Haskell.

Inspired by [MicroC](https://blog.josephmorag.com/posts/mcc0/)

Generates LLVM IR, uses Clang as a backend.

## Features

Current:

- Variable and function declarations
- Return, if, for, while and do-while statements
- Expressions and precedence
- Number, char and float literals
- Binary and unary operators (most implemented)
- Detailed syntax errors
- Static analysis for bindings and types
- Code generation for expressions and functions
- Pointer types
- Type casts
- Arrays (1-dimensional)

Built-in functions (no includes required):

```c
void* malloc(int);
void free(void*);
int scanf(char*, void*);
int printf(char*, ...);
double sqrt(double);
double pow(double, double);
```

## Resources used

[C89 Draft](https://port70.net/%7Ensz/c/c89/c89-draft.html)

[Writing a C Compiler - Nora Sandler](https://norasandler.com/2017/11/29/Write-a-Compiler.html)

[Functional Parsing - Computerphile](https://www.youtube.com/watch?v=dDtZLm7HIJs)

[Megaparsec tutorial](https://markkarpov.com/tutorial/megaparsec.html)

[Compiling a functional language to LLVM - Daniel J. Harvey](https://danieljharvey.github.io/posts/2023-02-08-llvm-compiler-part-1.html)

[Micro C - Joseph Morag](https://blog.josephmorag.com/posts/mcc0/)

[Godbolt Compiler Explorer](https://godbolt.org/)

## Usage

Only compatible with GHC 8.8.

Requires LLVM 14 to be installed including Clang 14.

On Ubuntu:

```
sudo apt install llvm-14
```

```
ghcup install cabal
```

```
cabal build
```

Binary is located in ./dist-newstyle/build

```
./cluck <input file>.c
```

If no input file is provided, code can be entered directly via standard input.

Use Ctrl + D to submit once typed.

## Options

`-o` - Specify output file

`-A` - Output AST

`-H` - Generate header file

`-S` - Output LLVM IR

# Examples

The [examples](examples) folder contains some supported programs used to test the compiler's capabilities.

| Example                       | Description                                               |
| ----------------------------- | --------------------------------------------------------- |
| [primes.c](examples/primes.c) | An implementation of the Sieve of Eratosthenes algorithm. |
| [merge.c](examples/merge.c)   | An implementation of Merge sort.                          |