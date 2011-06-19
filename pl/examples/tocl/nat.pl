p(Q) :- Q(0), Q(s(0)).
nat(0).
nat(s(X)):-nat(X).
