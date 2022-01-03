# Debris Language
[![ci](https://github.com/Inky-developer/debris/workflows/ci/badge.svg)](https://github.com/Inky-developer/debris/actions)
[![dependency status](https://deps.rs/repo/github/Inky-developer/debris/status.svg)](https://deps.rs/repo/github/Inky-developer/debris)

Debris is a powerful language & compiler which aims to make the process of creating a datapack easier and quicker.
For a working prototype in python, take a look at [McScript](https://github.com/Inky-developer/mcscript).

There is an interactive online playground available: https://inky-developer.github.io/debris-playground/

## Status
This language is in a *very* early state. 

Documentation for the Project can be found [here](https://inky-developer.github.io/debris/debris_lang/)

Goal for version 0.1: Feature parity with the mcscript prototype
  - [x] Internal support for types
  - [x] Internal support for variables
  - [x] Support for modules
  - [x] Support for integers and arithmetic operations (+, -, *, /, %)
  - [x] Support for booleans and relations (==, !=, <, >, <=, >=)
  - [x] Support for conditions
  - [x] Control flow (return statements)
  - [x] Support for recursive loops
  - [x] Support for functions
  - [x] Support for structs
      - [x] Struct declaration and initialization
      - [x] Struct objects as function parameters
      - [x] ~~Custom implementation for struct update (`old_struct = new_struct`)~~
      - [x] Associated methods ~~and values~~ for structs
      - [ ] Interfaces
  - [x] Support for tuples
    - [x] tuple patterns for variable assignment
    - [x] tuple patterns for variable updates
    - [x] tuple methods, including `.length()` and `.get(0)`
    - [ ] variadic tuple patterns (`let (a, ..rest) = tuple;`)
    - [ ] variadic tuple arguments (`fn foo(array: (Int..)) {...}`)
    - [ ] iterating tuples (`comptime for i in (1, 2, 3) { print(i) }`)
  - [ ] Basic minecraft standard library
  - [ ] Context manipulators (execute as/at/positioned/...)
  - [ ] Syntax sugar
    - [x] In-place operators (+=, -=, *=, /=, %=)
    - [ ] While loop and for loop (requires interfaces)
    - [ ] attribute to declare ticking functions
  - [x] Basic optimization passes
  - [x] Further optimizations 
  - [x] Automated integration tests
  - [x] Internal refactoring to remove some recursive implementations
