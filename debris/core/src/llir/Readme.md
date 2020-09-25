# The Low-Level Intermediate Representation For The Debris Language

The LLIR has a syntax similar to minecraft commands but tries to be as version agnostic as possible, while not being less powerful than minecraft commands
The focus lies on mathematical operations, so a lot of computation is done one scoreboards
Nonetheless, the LLIR has extensive capabilities to work with nbt data in data storages or entities and to convert to and from nbt.
I am not sure yet how to handle the rest, for now the execute-node can be used to run any other command

All Operations on scoreboard values operate in-place, ie. they change the input value. This should be accounted for when generating the llir.

## Nodes

This are the various nodes that make up the LLIR:
* Function(u64, Vec\<Node\>)
* FastStore(scoreboard, value, id)
* FastStoreFromResult(scoreboard) { command }
* BinaryOperation(scoreboard_value, value, operation)
* Conditional(conditional)
* Branch(Vec\<Conditional\>) { Function, Option\<Function\> }
* Execute(String)
