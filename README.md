# mfxparser
I use the precedence climbing parser technique combined with monadic parsing to develop a mixfix parser.

With monadic parsing we can deal with some ambiguity and overlaps, preferring the longest match, 
by example, if we have to prefix operators: `if_then_` and `if_then_else`, 
and the expression `if e0 then if e1 then e2 else e3`, it is analysed as
 `if e0 (if e1 then e2 else e3)`.

I also include parsing of calculational syntax for quantifiers, containers, substitution 
and also conjunctive operators. 
(http://www.cs.utexas.edu/users/EWD/transcriptions/EWD13xx/EWD1300.html)

Mixfix parsing have the ability to extend a language with prefix, infix an postfix operators and are used in languages such as Agda, Maude, Coq and others.

Precedence parser is based on Parsing expressions by precedence climbing
* http://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing/ 
* http://www.engr.mun.ca/~theo/Misc/exp_parsing.htm
* http://antlr.org/papers/Clarke-expr-parsing-1986.pdf

Some examples are on the Examples.hs file

I develop this parser as a beginning work to construct a calculational style proof assistant.
