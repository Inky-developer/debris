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
  - [x] Support for recursive loops
  - [x] Support for functions
  - [x] Control flow (return statements)
  - [x] Support for structs
      - [x] Struct declaration and initialization
      - [x] Struct objects as function parameters
      - [x] Associated methods ~~and values~~ for structs
  - [ ] Support for sum types
      - [ ] Declaring sum tyes
      - [ ] Matching on sum types
  - [x] Function expressions (`comptime my_func = fn() { ... }`)
  - [ ] Struct expressions (`comptime my_struct = struct { ... }`)
  - [ ] Interfaces
  - [x] Support for tuples
    - [x] tuple patterns for variable assignment
    - [x] tuple patterns for variable updates
    - [x] tuple methods, including `.length()`, `.get(0)`, `.added(element)` and `.join(separator)`
    - [ ] iterating tuples (`comptime for i in (1, 2, 3) { print(i) }`)
  - [ ] Basic minecraft standard library
  - [x] Builtin functionality for more control over the generated datapack
    - [x] `execute` function for inserting any command
    - [x] `export` function for generating a function at a specific path
    - [x] `on_tick` function for calling a function every tick
  - [x] Context manipulators (execute as/at/positioned/...)
  - [ ] Syntax sugar
    - [x] In-place operators (+=, -=, *=, /=, %=)
    - [x] While loops
    - [x] attribute to declare ticking functions
    - [ ] syntax sugar for for-loops (Blocked on interfaces?)
  - [x] Basic optimization passes
  - [ ] Further optimizations 
  - [x] Automated integration tests
  - [x] Internal refactoring to remove some recursive implementations
  - [x] Improved parser for more flexibility

## Editor Support
A basic vscode extension can be found at the latest run of the [extensions action](https://github.com/Inky-developer/debris/actions/workflows/editor_extensions.yml).
