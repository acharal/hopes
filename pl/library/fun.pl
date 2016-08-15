
compose(F, G, X, Y) :- G(X, Z), F(Z, Y).

converse(F, X, Y, Z) :- F(Y,X,Z).

flip(F, X, Y) :- F(Y, X).
