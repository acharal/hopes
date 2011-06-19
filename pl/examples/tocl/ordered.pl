
ordered(R, []).
ordered(R, [X]).
ordered(R, [X, Y|T]) :- R(X, Y), ordered(R, [Y|T]).
