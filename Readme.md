# Debris Language
[![ci](https://github.com/Inky-developer/debris/workflows/ci/badge.svg)](https://github.com/Inky-developer/debris/actions)

Debris is a powerful language & compiler which aims to make the process of creating a datapack easier and quicker.
For a working prototype in python, take a look at [McScript](https://github.com/Inky-developer/mcscript).

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
  - [ ] Support for recursive loops
  - [x] Support for functions
  - [ ] Support for structs
  - [ ] Basic optimization passes
  - [ ] Automated integration tests

## ToDo:
  - Implement simplest form of loops:
    Withing a block the keyword `recurse` jumps back to the top 
  - For now loops will be runtime-only, so all variables that are written
    to in a recursing block must be non-comptime
  - To detect that, mark recursing blocks in hir
  - Mir runs an extra pass on recursive blocks to filter written variables
    and promote them to runtime variants
  - To desugar while- and for loops to use the new `recurse` keyword, the control flow keyword `break` has to be implemented
  - Before that, implement the `return` keyword, which breaks execution in a function
  - How to implement control-flow keywords???
