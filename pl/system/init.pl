
:- include('op.pl').
:- include('types.pl').
:- include('base.pl').
:- include('../library/arith.pl').

include([]).
include([X|Xs]) :- includeOne(X), include(Xs).


consult(X) :- includeOne(X).
