% Pls9 examples etc.

a.
a(X) :- X = 0.
b1(X) :- X(a).
b2(X) :- X(pred a/0).
b3(X) :- X(pred a/1).


map(_)([],[]).
map(R)([X|Xs],[Y|Ys]) :-
    R(X,Y), map(R)(Xs,Ys).

foo(X) :- bar(X).
bar(X) :- foo(s(X)).

apply(X,Y) :- X(Y).
isZero(X) :- apply( (\~(Y) => Y=0), X).

applyCur(X)(Y) :-  X(Y).
isZeroCur(X) :- applyCur((\~(Y) => Y=0))(X).


foo2(X) :- bar2(X).
bar2(X) :- foo2(X).
baz2(X) :- foo2(s(X)), bar2(X).

closure(R)(X,Y) :- R(X,Y).
closure(R)(X,Y) :-
    R(X,Z), closure(R)(Z,Y).

