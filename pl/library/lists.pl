
/*
:- module(lists,
	[ member/2,			    % ?X, ?List
      append/2,			    % +ListOfLists, -List
      append/3,			    % ?A, ?B, ?AB
	  prefix/2,			    % ?Part, ?Whole
	  select/3,			    % ?X, ?List, ?Rest
	  selectchk/3,			% ?X, ?List, ?Rest
	  select/4,			    % ?X, ?XList, ?Y, ?YList
	  selectchk/4,			% ?X, ?XList, ?Y, ?YList
	  nextto/3,			    % ?X, ?Y, ?List
	  delete/3,			    % ?List, ?X, ?Rest
	  nth0/3,			    % ?N, ?List, ?Elem
	  nth1/3,			    % ?N, ?List, ?Elem
	  nth0/4,			    % ?N, ?List, ?Elem, ?Rest
	  nth1/4,			    % ?N, ?List, ?Elem, ?Rest
	  last/2,			    % +List, -Element
	  proper_length/2,		% @List, -Length
	  same_length/2,		% ?List1, ?List2
	  reverse/2,			% +List, -Reversed
	  permutation/2,		% ?List, ?Permutation
	  flatten/2,			% +Nested, -Flat

	  % Ordered operations
	  max_member/2,			% -Max, +List
	  min_member/2,			% -Min, +List

	  % Lists of numbers
	  sum_list/2,			% +List, -Sum
	  max_list/2,			% +List, -Max
	  min_list/2,			% +List, -Min
	  numlist/3,			% +Low, +High, -List

      % set manipulation
	  is_set/1,			    % +List
	  list_to_set/2,		% +List, -Set
	  intersection/3,		% +List1, +List2, -Intersection
	  union/3,			    % +List1, +List2, -Union
	  subset/2,		     	% +SubSet, +Set
	  subtract/3			% +Set, +Delete, -Remaining
	]).
*/


%% append(?List1, ?List2, ?List1AndList2)
%
%  List1AndList2 is the concatenation of List1 and List2
append([], X, X).
append([X|Z1], Y, [X|Z2]) :- append(Z1, Y, Z2).
% append(L1, L2, L12) :- foldl( headtail, L12, L1, L2)

%% append(?ListOfLists, ?List)
%
%  Concatenate a list of lists. Is true if ListOfLists is a list of lists, 
%  and List is the concatenation of these lists.
append(L, L).
append([L1|LR], L) :- append(LR, LL), append(L1, LL, L).

%%  member(?Elem, ?List)
%
%   True if Elem is a  member   of  List. 
member(X, [X]).
member(X, [_|Y]) :- member(X, Y).

% member(X, L) :- exists(eq(X), L). where eq(X,X).

%% prefix(?Part, ?Whole)
%
%  True iff Part is a leading substring of Whole.
prefix(L1, L2) :- append(L1, _, L2).


%% suffix(?Part, ?Whole)
%
%  True iff Part is a trailing substring of Whole.
suffix(L1, L2) :- append(_, L1, L2).

%%	select(?Elem, ?List1, ?List2)
%
%	Is true when List1, with Elem removed, results in List2.
select(X, L, L12) :-
    append(L1, [X|L2], L),
    append(L1, L2, L12).

%% nextto(?X, ?Y, ?List)
%
%  True if Y follows X in List.
nextto(X, Y, [X,Y|_]).
nextto(X, Y, [_|List]) :- nextto(X, Y, List).
    

%% delete(+List1, @Elem, -List2)
%
%  Delete matching elements from a list. True when List2 is a list with all elements 
%  from List1 except for those that unify with Elem. Matching Elem with elements of 
%  List1 is uses \+ Elem \= H, which implies that Elem is not changed.

delete(L1, X, L2) :- select(X, L1, L2).

%% last(?List, ?Last)
%
%  Succeeds when Last is the last element of List. This predicate is semidet if 
%  List is a list and multi if List is a partial list.
last(Xs, X) :- suffix([X],Xs).

%%	reverse(?List1, ?List2)
%
%	Is true when the elements of List2 are in reverse order compared to
%	List1.
reverse(List, Reversed) :- rev_acc( List, [], Reversed).

rev_acc([], Reversed, Reversed).
rev_acc([X|Xs], List, Reversed) :- rev_acc(Xs, [X|List], Reversed).


%% flatten(?List1, ?List2)
%
%  Is true if List2 is a non-nested version of List1.
flatten(List1, List2) :- append(List1, List2).

