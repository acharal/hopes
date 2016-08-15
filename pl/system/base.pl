

eq(X,X).

% X = Y :- eq(X, Y).

X = X.

X \= Y :- not(X = Y).

% fail :- !.

true.
true(_).
true(_,_).
true(_,_,_).
true(_,_,_,_).

not(G) :- G, !, fail.

\+ G :- not(G).

fail :- a = b.

