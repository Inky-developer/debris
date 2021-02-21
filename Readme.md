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
  - [x] Control flow (return statements)
  - [ ] Support for recursive loops
  - [x] Support for functions
  - [ ] Support for structs
  - [x] Basic optimization passes
  - [x] Automated integration tests

## ToDo:
  - Implement simplest form of loops
  - Prevent llir evaluating nodes mostly recursively which leads to performance
    problems - implement it like an interpreter
  - Prevent the datapack backend also working recursively which can lead
    to performance problems
  - Rethink the parse step, maybe use a hand-written parser
