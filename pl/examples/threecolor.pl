eq(X, X).

neq(X, Y) :- not(eq(X,Y)).

nonempty(R)  :- R(X).
nonempty2(R) :- R(X, Y).

empty(R)  :- not(nonempty(R)).
empty2(R) :- not(nonempty2(R)).

minus(R, X, Z) :- R(Z), neq(X, Z).
minus2(R, X, Y, Z, K) :- R(Z, K), neq(Z,X), neq(K,Y).

threecolor(Graph,R,G,B) :- empty2(Graph).
threecolor(Graph,R,G,B) :-
       Graph(X,Y),
       has_color(X,Y,R,G,B),
       has_color(Y,X,R,G,B),
       threecolor(minus2(Graph,X,Y),R,G,B).

has_color(X,Y,R,G,B) :- R(X),not(R(Y)),not(G(X)),not(B(X)).
has_color(X,Y,R,G,B) :- B(X),not(B(Y)),not(R(X)),not(G(X)).
has_color(X,Y,R,G,B) :- G(X),not(G(Y)),not(B(X)),not(R(X)).

two_colorable(G, R) :- empty2(G).
two_colorable(G, R) :- 
       G(X, Y), 
       check(X, Y, R), 
       two_colorable(minus2(G, X, Y), R).

check(X, Y, R) :- R(X), not(R(Y)).
check(X, Y, R) :- R(Y), not(R(X)).


graph(a,b).
graph(a,d).
graph(b,e).
