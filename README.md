## About

This is a work-in-progress implementation of custom compiled general-purpose language (some syntax sketches can be found in ```desing_doc.md```).

## Current Status

A vertical slice of a future compiler is fully implemented (albeit for now quite incapable) and can be found in ```src/v2``` directory.
All other files are part of "older" implementation and are left as a reference.

In it's current form the compiler can parse, check (according to basic strict type system) and transpile into C type casts and arbitrary arithmetic and logic expressions.

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
- parameterless functions
- scopes and statements
- variables and parameterised functions
- if statements
- loops
- record types

