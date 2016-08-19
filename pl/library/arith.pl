
% arithmetics

:- op( 700, xfx, 'is'),     % arithmetic evaluation
   op( 500, yfx, '+'),      % addition
   op( 500, yfx, '-'),      % substraction
   op( 400, yfx, '*'),      % multiplication
   op( 400, yfx, '/'),      % Pred/Arity - floating point division
   op( 400, yfx, '//'),     % integer division
   op( 400, yfx, 'rem'),    % remainder
   op( 400, yfx, 'mod'),    % modulo
   op( 200,  fy, '-'),      % sign reversal
   op( 200,  fy, '+').      % ???

% comparisons

:- op( 700, xfx, '=:='),    % equal
   op( 700, xfx, '=\\='),   % not equal
   op( 700, xfx, '<'),      % less
   op( 700, xfx, '=<'),     % less equal
   op( 700, xfx, '>'),      % greater
   op( 700, xfx, '>=').     % greater equal


X =:= Y  :- compare(X =:= Y).
X =\\= Y :- compare(X =\\= Y).
X < Y    :- compare(X < Y).
X =< Y   :- compare(X =< Y).
X > Y    :- compare(X > Y).
X >= Y   :- compare(X >= Y).

plus(X, Y, Z) :- Z is X + Y.

inc(A, A1) :- plus(1, A, A1).

add1(_, A, As) :- inc(A, A1).

length(As, L) :- foldr(add1, 0, As, L).

sum_list(As, S) :- foldr(plus, 0, As, S).
