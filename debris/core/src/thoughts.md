# Thoughts
## How should objects work?
### One integer with either static or dynamic value or two integers?
One of the biggest questions I have is how to handle dynamic vs static values. 
For example an integer may be known at compile time (not const though, since the value may change) while another integer might not be known at comptime at all.
I think in other languages all variables only have dynamic values but may be optimized out.
This could work by giving each integer an id and additionally a range where the value could be.
In my opinion this is quite dirty. Also loops could be problematic since the static value might have to be cleared before the loop.
Alternatively, I could create one type for static integers and one type for dynamic integers.
This solution is more effort, since I would also have to define operations between static and dynamic integers, but it feels a lot nicer than a lot of if-else branches.
This might also be a problem for function signatures, because it is common to accept any int and not care whether its static or dynamic. 
To solve this, a alias, `int` could be added. I don't know how exactly it should work, though. For the beginning, I will only treat it as an alias for the user,
while the program knows the exact underlying type at comptime.

## Overview
The Debris compiler has multiple intermediate representations. This chapter will give an overview of this example program:

    let a = 2 + 3 * 4
    let half = a / 2
    print(half)

### Parsing the code
The first step is to parse this source text into a high-level intermediate representation. Not much processing other than parsing will happen, however some constructs like if-expressions will be desugared.
The resulting HIR is equivalent to:

```
main_function {
    declare a := Plus(int(2), Times(int(3), int(4)))
    declare half := Divide(var(a), int(2))
    call print [var(half)]
    inner_objects []
}
```

The (here empty) list of inner objects contains all nested functions, structs and namespaces, so they can be resolved using an `Accessor`.

### Creating the MIR
The MIR, Mid-level intermediate representation, contains information about types and is generally a bit more low-level.
The only objects that exist are constructed from code literals, but none are computed

```
context 0 {
    def 0.0 int { 2 }
    def 0.1 temp int { 3 }
    def 0.2 temp int { 4 }
    call int.~mult [temp.1, temp.2]
    def 0.3 int result
    call int.~add [temp.0, temp.1]
    def 0.4 int result

    // set 
    call int.~copy [0.1, 0.0]
    def temp.0 int { 2 }
    call int.~div [0.1, temp.1] # 3

    call print [0.1] # 4
}
inner_objects := []
```

As you can see, the mir contains information about types and associated functions. This example is not optimized, however in the actual implementation the compiler would detect in a later pass that 0.0 is only read to be copied to 0.1 and optimize the need for this copy out. The function calls are not evaluated until later

### Creating the LLIR
The Low-level intermediate representation is quite similar to the minecraft syntax. Also, all type information are now erased

```
function main {
    store_to_scoreboard 0.0 val(2)
    store_to_scoreboard 0.1 val(3)
    store_to_scoreboard 0.2 val(4)
    scoreboard_operation multiply 0.1 score(0.2)
    scoreboard_operation add 0.0 score(0.1)

    store_to_scoreboard 0.1 score(0.0)
    store_to_scoreboard 0.2 val(2)
    scoreboard_operation divide 0.1 score(0.2)

    display_message chat [ { score(0.1) } ]
}
```
