# HOPES: Higher-Order PROLOG with Extensional Semantics

HOPES is a prototype interpreter for a higher-order PROLOG-like
language. The syntax of the language extends that of PROLOG by
supporting higher-order constructs (such as higher-order predicate
variables, partial application and lambda terms). In particular,
the syntax allows clauses (and queries) that contain uninstantiated
predicate variables. The interpreter implements a higher-order
top-down SLD-resolution proof procedure described in [1].

## Introduction 

For example, in HOPES one can express popular functional operators
such as `map`:
```prolog
map(R,[],[]).
map(R,[X|Xs],[Y|Ys]) :- R(X,Y), map(R,Xs,Ys).
```
Apart from using functional operators in a logic programming context,
we can also express naturally relational operators and graph operators:
```prolog
join(R,Q)(X) :- R(X), Q(X).
union(R,Q)(X) :- R(X).
union(R,Q)(X) :- Q(X).
singleton(Y)(X) :- X = Y.
diff(R,Q)(X) :- R(X), not(Q(X)). 
tc(G)(X,Y) :- G(X,Y).
tc(G)(X,Y) :- G(X,Z), tc(G)(Z,Y).
```
The definition of higher-order predicates together with the use of partial
applications can lead to a different programming style that blends  the 
functional and the logic programming. The following HOPES snipset shows how
we can define `except` (i.e. the predicate that succeeds for all elements of `R` except `Y`) 
by reusing (possibly) partially applied operators.
```prolog
except(R,Y)(X) :- diff(R, singleton(Y)).
```

In the case of uninstantiated predicate variables, the proof procedure
will systematically (and in a sophisticated way) investigate all
finite instantiations of these variables. In other words, HOPES
has all the advantages of a higher-order system but continues
to keep the flavor of classical Prolog programming.

## Getting Started

### Building HOPES

In order to build HOPES you must have installed the GHC compiler
version 6 or higher and the cabal system. You can download GHC from
http://www.haskell.org/ghc/

There is a Makefile for convenience, and you can type `make` to
the console to compile the program.

Microsoft Windows compilations are not yet supported but this should
be no problem as far as GHC and cabal are installed in the system.

### Running some examples

In pl/examples directory there are some hopes examples to getting
started. To load an example you must type
```
-? :l pl/examples/file.pl
```

[1]: http://dx.doi.org/10.1145/2499937.2499942 "Extensional Higher-Order Logic Programming, Angelos Charalambidis, Konstantinos Handjopoulos, Panos Rondogiannis, William W. Wadge, ACM Transactions on Computational Logic (TOCL), Volume 14 Issue 3, August 2013  Article No. 21"
