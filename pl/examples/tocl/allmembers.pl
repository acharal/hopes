allmembers([], P).
allmembers([H|T], P) :- P(H), allmembers(T, P).
