closure(R, X, Y) :- R(X,Y).
closure(R, X, Y) :- R(X, Z), closure(R, Z, Y).


q(R) :- closure(R, a, b).
