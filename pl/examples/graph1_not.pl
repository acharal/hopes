
% not prelude 

eq(X, X).

singleton(X, Y) :- eq(X, Y).

fail :- eq(c1,d1).

not(X) :- X, !, fail.


neq(X, Y) :- not(eq(X,Y)).

nonempty(P) :- P(X).
nonempty2(P) :- P(X,Y).
empty(P) :- not(nonempty(P)).
empty2(P) :- not(nonempty2(P)).

union(R, G, Y) :- R(Y), !.
union(R, G, Y) :- G(Y).

minus(R, X, Z) :- R(Z), neq(X, Z).
minus2(R, X, Y, Z, K) :- R(Z, K), neq(Z, X), neq(K, Y).

equalrel(P, Q) :- empty(P), empty(Q).
equalrel(P, Q) :- P(X), Q(X), equal(minus(P,X), minus(Q, X)).


connected_bfs(G, R, Y) :- R(Y).
connected_bfs(G, R, Y) :- R(X), connected_bfs(G, union(minus(R,X), G(X)), Y).

connected(G, X, Y) :- connected_bfs(G, singleton(X), Y).


check(X, Y, R) :- R(X), not(R(Y)).
check(X, Y, R) :- R(Y), not(R(X)).

two_colorable(G, R) :- empty2(G).
two_colorable(G, R) :- G(X, Y), check(X, Y, R), two_colorable(minus2(G, X, Y), R).

graph(a,b).
graph(a,d).
graph(b,e).
