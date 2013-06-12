% This file contains some --Placeholder-- implementations for
% some standard Prolog predicates, useful only to infer 
% their types.
% 
% Under Consideration!

true.
fail.

% Term unification
'='(0,0).
unify_with_occurs_check(0,0).
'\\='(0,0).

% Type testing
var(_).
atom(0).
integer(0).
float(0).
atomic(0).
compound(0).
nonvar(_).
number(0).

% Term comparison
functor(0,0,0).
arg(0,0,0).
'=..'(0,0).

% Arithmetic evaluation
is(0,0).

% Arithmetic comparison
'=:='(0,0).
'=\\='(0,0).
'<'(0,0).
'=<'(0,0).
'>'(0,0).
'>='(0,0).


