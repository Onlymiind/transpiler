## About

This is a work-in-progress implementation of custom compiled general-purpose language. For up-to-date examples check files in ```tests/functional/data``` directory (files with ```_fail``` postfix are examples of invalid programs, files with ```_expected``` postfix contain expected C output). Supported syntax is described in ```design_doc.md``` file.

## Current Status

The compiler in it's current state implements a significant part of basic programming laguage features, although the language remains quite simplistic (for now). The compiler also takes a number of shortcuts by transpiling source code into C instead of machine language (e.g. there is no optimization pass whatsoever).

Currently inmplemented:
- Basic static strict type system
- Arbitrary expressions
- Explicit type conversions
- Functions and function calls
- Statements and function return types
- Variables
- Basic type inference for variables
- If statements
- Loops
- Pointers
- Basic constant folding
- Runtime checks for null pointer dereference

## Building

```
git clone --recurse-submodules git@github.com:Onlymiind/transpiler.git
cd transpiler/src/v2
mkdir build && cd build
cmake ..
cmake --build .
```

## Usage

```
compiler <path-to-input-file> [path-to-output-file]
gcc <path-to-output-file>
```

## TODO next (roadmap):
- arrays and slices
- record types
- add more built-in types
- temporary basic built-in IO
- struct and array literals
- all other features to make this usable :)

