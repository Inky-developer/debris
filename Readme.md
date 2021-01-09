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
  - Add error message for comptime value in runtime condition
  - Allow for else-if blocks 
  - Add Overflow checks for static int operations or let them overflow
