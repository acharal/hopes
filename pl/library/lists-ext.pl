

/*
:- module(lists-ext,
	[ member/2,			    % ?X, ?List
      append/2,		    	% +ListOfLists, -List
      append/3,			    % ?A, ?B, ?AB
	  prefix/2,			    % ?Part, ?Whole
      suffix/2,             % ?Part, ?Whole
      headtail/3,           % ?List, ?Head, ?Tail
      map/3,                % ?Relation, ?List1, ?List2 
      applist/2,            % ?Relation, ?List
      foldl/4,
      foldl1/3,
      foldr/4,
      foldr1/3,
      last/2,
	  same_length/2,		% ?List1, ?List2
	  permutation/2,		% ?List, ?Permutation
      filter/4,
      zip/3
	]).
*/


%% map(?Relation, ?List1, ?List2)
%
%  Maps the elements in List1 to List2 wrt to mapping Relation
map(_, [], []).
map(R, [A|B], [X|Y]) :- R(A, X), map(R, B, Y).
% map(R, Xs, Ys) :- foldr( (\X->\Y->\Z-> R(X,X1), headtail(Z,X1,Y)) ), [], Xs, Ys).


%% foldl(?Relation, +Initial, +List, -Result)
% 
%  List=[X0,...Xn], F(Initial,X0,Y1), F(Y1,X1,Y2), ..., F(Yn,Xn,Result).
foldl(_, Z, [], Z).
foldl(F, Y0, [X|Xs], Z) :- F(Y0, X, Y1), foldl(F, Y1, Xs, Z).

%% foldl1(?Relation, +Initial, +List, -Result)
% 
%  like foldl but without starting value
foldl1(F, [X1|Xs], Z) :- foldl(F, X1, Xs, Z).

%% foldr(?Relation, +Initial, +List, -Result)
%
% List=[X0,...Xn], F(X0, Yn, Result), F(X1, Yn-1, Yn), ... F(Xn, Y1, Y0).
foldr(_, Z, [], Z).
foldr(F, Y0, [X|Xs], Z) :- F(X, Y2, Z), foldr(F, Y0, Xs, Y2).

%foldr1

%% applist(?Relation, ?List)
%
% List = [X0,...Xn], F(X0,X1), F(X1,X2), ..., F(Xn-1, Xn).
applist(F, [X1,X2|Xs]) :- F(X1,X2), applist(F,[X2|Xs]).

headtail([X|Xs], X, Xs).

%% append(?List1, ?List2, ?List1AndList2)
%
%  List1AndList2 is the concatenation of List1 and List2
append(L1, L2, L12) :- foldl(headtail, L12, L1, L2).


%% append(?ListOfLists, ?List)
%
%  Concatenate a list of lists. Is true if ListOfLists is a list of lists, and List is the concatenation of these lists.
append(ListOfLists, List) :- foldl(append, [], ListOfLists, List).

%% prefix(?Part, ?Whole)
%
%  True iff Part is a leading substring of Whole.
prefix(L1, L2) :- append(L1, _, L2).


%% suffix(?Part, ?Whole) 
%
%  True iff Part is a trailing substring of Whole.
suffix(L1, L2) :- append(_, L1, L2).


%%  member(?Elem, ?List)
%
%   True if Elem is a  member   of  List.
member(X, Xs) :- suffix( [X|_], Xs). 

%% last(?List, ?Last)
%
%  Succeeds when Last is the last element of List. This predicate is semidet if 
%  List is a list and multi if List is a partial list.
last(Xs, X) :- suffix([X],Xs).

%%	same_length(?List1, ?List2)
%
%	Is true when List1 and List2 are   lists with the same number of
%	elements. The predicate is deterministic if  at least one of the
%	arguments is a proper list.  It   is  non-deterministic  if both
%	arguments are partial lists.
same_length(Xs,Ys) :- map(true, Xs,Ys)

%%	permutation(?Xs, ?Ys) is nondet.
%
%	True when Xs is a permutation of Ys. This can solve for Ys given
%	Xs or Xs given Ys, or  even   enumerate  Xs and Ys together.
permutation(Xs, Ys) :- same_length(Xs, Ys), foldl(delete, L, P, []).


filter(_, [], []).
filter(P, [X|Xs], Zs) :- P(X) -> Zs=[X|Ys] ; Zs = Ys, filter(P, Xs, Ys).


zip([], [], []).
zip([A|As], [B|Bs], [(A,B)|AsBs]) :- zip(As,Bs,AsBs).

