Wh1le
=====

Implementation of the while language (work in progress) and the available expression analysis for a homework. I thought I'd rather write the thing down instead of doing the analysis by hand.

What's there:

- While-Syntax (with some implicits but kind of unwieldy yet)
- While analysis functions, the analysis algorithm and instances:
  - Available Expression
  - Live Variables
  - Very Busy Expression
  - Reaching Definition
- Tests

TODO:

- Proper documentation
- Lexer and Parser OR proper embedding into Scala
- Tail calls for functions like "flow" / using scala functors where possible.
