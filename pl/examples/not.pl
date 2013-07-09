
eq(X, X).


singleton(X, Y) :- eq(X, Y).

fail :- eq(c1,d1).

not(X) :- X, !, fail.


neq(X, Y) :- not(eq(X,Y)).


