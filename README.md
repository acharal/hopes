# HOPES: Higher-Order PROLOG with Extensional Semantics

[![GitHub license](https://img.shields.io/badge/license-GPLv2-blue.svg)](https://raw.githubusercontent.com/acharal/hopes/master/COPYING)
[![Build Status](https://travis-ci.org/acharal/hopes.svg?branch=master)](https://travis-ci.org/acharal/hopes)

HOPES is a prototype interpreter for a higher-order PROLOG-like language. 

The syntax of the language extends that of PROLOG by supporting higher-order
constructs (such as higher-order predicate variables, partial application and
lambda terms). In particular, the syntax allows clauses (and queries) that
contain uninstantiated predicate variables. The interpreter implements a
higher-order top-down SLD-resolution proof procedure described in [CKRW13]
together with the semantics of the language. 

HOPES has all the advantages of a higher-order system but continues to keep  the
flavor of classical PROLOG programming.


## Introduction 

In HOPES one can express popular functional operators such as `map`:
```prolog
map(R,[],[]).
map(R,[X|Xs],[Y|Ys]) :- R(X,Y), map(R,Xs,Ys).
```
Apart from using functional operators in a logic programming context, we can
also express naturally relational operators and graph operators:
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
functional and the logic programming. The following HOPES snipset shows how we
can define `except` (i.e. the predicate that succeeds for all elements of `R`
except `Y`) by reusing (possibly) partially applied operators.
```prolog
except(R,Y)(X) :- diff(R, singleton(Y)).
```

As in classical PROLOG it is not required to use in a query some variables as
input and some variables as output. Along that lines, HOPES also support queries
that may have unbound predicate variables. For example, one can query
```
?- tc(G)(a,b).
```
to ask for potential graphs `G` whose transitive closure contains `(a,b)`. In
that case the interpreter will systematically (and in a sophisticated way)
investigate all finite instantiations of these variables. The answers will be
sets that represent the **extension** of `G` even if no such predicate is
currently defined in the program. For example, potential answers of the
aforementioned query will be:
```
G = { (a,b) } ;
G = { (a,X1), (X1,b) } ;
G = { (a,X1), (X1,X2), (X2,b) };
```

## Getting Started

### Building HOPES

In order to build HOPES you must should install [haskell stack](https://docs.haskellstack.org/en/stable/README/).
```bash
$ curl -sSL https://get.haskellstack.org/ | sh
$ stack build
```

#### Note for Windows
Microsoft Windows compilations are not yet tested but there should
be no problem as far as GHC and cabal are installed in the system.

### Running some examples

In pl/examples directory there are some hopes examples to getting
started. To load an example you must type
```
-? :l pl/examples/file.pl
```

[CKRW13]: http://dx.doi.org/10.1145/2499937.2499942 "Extensional Higher-Order Logic Programming, Angelos Charalambidis, Konstantinos Handjopoulos, Panos Rondogiannis, William W. Wadge, ACM Transactions on Computational Logic (TOCL), Volume 14 Issue 3, August 2013  Article No. 21"
