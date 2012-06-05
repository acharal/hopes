# HOPES: Higher-Order Prolog with Extensional Semantics

June 1, 2011

## Introduction

Hopes is a prototype interpreter for a higher-order PROLOG-like
language. The syntax of the language extends that of PROLOG by
supporting higher-order constructs (such as higher-order predicate
variables, partial application and lambda terms). In particular,
the syntax allows clauses (and queries) that contain uninstantiated
predicate variables. The interpreter implements a higher-order
top-down SLD-resolution proof procedure described in [1]. In the
case of uninstantiated predicate variables, the proof procedure
will systematically (and in a sophisticated way) investigate all
finite instantiations of these variables. In other words, Hopes
has all the advantages of a higher-order system but continues
to keep the flavor of classical Prolog programming.

## Compilation

In order to compile hopes you must have installed the GHC compiler
version 6 or higher and the cabal system. You can download GHC from
http://www.haskell.org/ghc/

There is a Makefile for convenience, and you can type "make" to
the console to compile the program.

Microsoft Windows compilations are not yet supported but this should
be no problem as far as GHC and cabal are installed in the system.


## Getting Started

In pl/examples directory there are some hopes examples to getting
started. To load an example you must type

    -? :l pl/examples/file.pl


## References

[1]: http://www.springerlink.com/content/f127ru366p77vux3/ "Extensional Higher-Order Logic Programming, Angelos Charalambidis, Konstantinos Handjopoulos, Panos Rondogiannis, William W. Wadge, Logics in Artificial Intelligence, Lecture Notes in Computer Science 2010"
