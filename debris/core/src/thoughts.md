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
