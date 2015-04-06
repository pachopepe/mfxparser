# mfxparser
A precedence climbing mixfix parser

This is a mixfix parser work on.

I included parsing of calculational expressions for quantifiers, containers and substitution and also conjunctive operators. 
(http://www.cs.utexas.edu/users/EWD/transcriptions/EWD13xx/EWD1300.html)

The precedence parser is based on Parsing expressions by precedence climbing
* http://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing/ 
* http://www.engr.mun.ca/~theo/Misc/exp_parsing.htm
* http://antlr.org/papers/Clarke-expr-parsing-1986.pdf

I added backtracking by expression length to solve ambiguities; by example, if we have to prefix operators:
`if_then_` and `if_then_else`, and the expression `if e0 then if e1 then e2 else e3`, it is analysed as
the expression `if e0 (if e1 then e2 else e3)`

Some examples are on the Examples.hs file

I develop this parser to construct as a beginning work to construct a calculational style prover assistant.


