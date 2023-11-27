## About

This is a work-in-progress implementation of custom compiled general-purpose language (some syntax sketches can be found in ```desing_doc.md```). For up-to-date examples check files in ```src/v2/tests/functional/data``` directory.

## Current Status

A vertical slice of a future compiler is fully implemented (albeit for now quite incapable) and can be found in ```src/v2``` directory.
All other files are part of "older" implementation and are left as a reference.

Implemented:
- Basic strict type system
- Arbitrary arithmetic and logic expressions
- Explicit type conversions
- Basic functions and function calls
- Statements and function return types

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
- variables and parameterised functions
- if statements
- loops
- record types
- pointers, arrays and slices
- all other features to make this usable :)

