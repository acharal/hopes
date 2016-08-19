

:- op(1200,  fx, '?-').     % goal
:- op(1100, xfy, ';').      % disjunction
:- op(1050, xfy, '->').     % if-then
%:- op(1000, xfy, ',').     % conjunction
:- op( 700, xfx, '=').      % unification
:- op( 700, xfx, '\\=').    % not unifiable
:- op( 100, xfx, '@').
:- op(  50, xfx, ':').      % module specification of a predicate Module:Pred
:- op( 600,  fy, '\\+').    % negation as failure

X = X.


X \\= Y :- not(X = Y).

true.
true(_).
true(_,_).
true(_,_,_).
true(_,_,_,_).

not(G) :- G, !, fail.

\+ G :- not(G).

fail :- a = b.
