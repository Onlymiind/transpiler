## About

This is a work-in-progress implementation of custom compiled general-purpose language (some syntax sketches can be found in ```desing_doc.md```). For up-to-date examples check files in ```src/v2/tests/functional/data``` directory (files with ```_fail``` postfix are examples of invalid programs, files with ```_expected``` postfix contain expected C output).

## Current Status

A vertical slice of a future compiler can be found in ```src/v2``` directory. It implements a significant part of basic compiler functionality, although a large number of crucial features is (for now) missing. It also takes a number of shortcuts by transpiling source code into C instead of machine language (e.g. there is no optimization pass whatsoever).

All other files are part of "older" implementation and will soon be removed.

Currently inmplemented:
- Basic strict type system
- Arbitrary expressions
- Explicit type conversions
- Functions and function calls
- Statements and function return types
- Variables
- Basic type inference for local variables
- If statements
- Loops
- Pointers

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
- temporary basic built-in IO
- all other features to make this usable :)

