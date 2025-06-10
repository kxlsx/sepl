## TODO:
# BIN
- cli
- better repl errors
- serde?
- more commands for the repl (undefine, restore)
# LIB
- clean up + add more builtins
- add ints
- tests
- figure out lib interface
- stack overflow handling, iterative evaluation (manual stack)
- faster hashmaps (dont need to be DOS secure)
- a bit more docs
- readme
- true, false  & nil don't need to exist (they can be symbols)
- clean up code
- eval args before calling (before Procedure::eval), this will clean up code 
- check docs
- add tests for lists
- (last (x y z w))
- fix ((lambda x x) x)  infinite recursion
- add = (would work for symbols)
