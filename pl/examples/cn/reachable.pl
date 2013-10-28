edge(a, b).
edge(c, d).
edge(d, c).


reachable(a).
reachable(X) :- reachable(Y), edge(Y, X).

unreachable(X) :- not(reachable(X)).

