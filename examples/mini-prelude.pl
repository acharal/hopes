%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                             %
%   H O P E   P R E L U D E   %
%                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% equality on zero-order terms

eq( X, X ).




%
% Natural Numbers
%


nat( 0 ).
nat( s( X ) ) :- nat( X ).

even( 0 ).
even( s( s( X ) ) ) :- even( X ).

odd( 1 ).
odd( s( s( X ) ) ) :- odd( X ).

leq( X, X ).
leq( X, s( Y ) ) :- leq( X, Y ).

order( X, 0, 0, X ).
order( 0, s( X ), 0, s( X ) ).
order( s( X1 ), s( X2 ), s( Y1 ), s( Y2 ) ) :- order( X1, X2, Y1, Y2 ).

max( X1, X2, X ) :- order( X1, X2, _, X ).

min( X1, X2, X ) :- order( X1, X2, X, _ ).

add( 0, X, X ).
add( s( X1 ), X2, s( X ) ) :- add( X1, X2, X ).

inc( X, Y ) :- add( 1, X, Y ).

substract( X, Y, Z ) :- add( Y, Z, X ).

multiply( 0, X, 0 ).
multiply( s( X ), 0, 0 ).
multiply( s( X ), s( Y ), W ) :- add( Z, s( Y ), W ), multiply( X, s( Y ), Z ).


% an kapoio apo ta dyo einai gnwsto kai to prod agnwsto kanoume to agnwsto enumerate me nat
% an kai ta dyo einai gnwsta ypologizoume
% an kapoio apo ta dyo einai gnwsto kai to prod gnwsto kanoume to agnwsto enumerate me nat, alla an den yparxei 8a kanei loop (enumerate me nat mexri to prod)
% an kanena apo ta dyo den einai gnwsto kai to prod gnwsto? (enumerate me nat mexri to prod)
% an kanena apo ta dyo den einai gnwsto kai to prod agnwsto? (enumerate me nat)







%
% Basic First-Order List Predicates
%


length( [], 0 ).
length( [ X | L ], s( N ) ) :- length( L, N ).

append( [], L, L ).
append( [ X | L1 ], L2, [ X | L12 ] ) :- append( L1, L2, L12 ).

splitAt( 0, L, [], L ).
splitAt( s( N ), [ X | L12 ], [ X | L1 ], L2 ) :- splitAt( N, L12, L1, L2 ).

reverse( L, R ) :- rev_app( L, [], R ).
rev_app( [], R, R ).
rev_app( [ X | Xs ], L, R ) :- rev_app( Xs, [ X | L ], R ).




%
% Basic Higher-Order List Predicates
%

all( R, [] ).
all( R, [ X | Xs ] ) :- R( X ), all( R, Xs ).

map( R, [], [] ).
map( R, [ X | Xs ], [ Y | Ys ] ) :- R( X, Y ), map( R, Xs, Ys ).

%
% applist( F, X0, [ X1, X2, ..., Xn ] )
%
% F( X0, X1 )
% F( X1, X2 )
% ..
% F( Xn-1, Xn )
%

applist( _, Z, [] ).
applist( F, Z, [ X | Xs ] ) :- F( Z, X ), applist( F, X, Xs ).

%
% foldl( F, Z1, [ X1, X2, ..., Xn ], Z )
%
% F( Z1, X1, Z2 )
% F( Z2, X2, Z3 )
% ..
% F( Zn, Xn, Z )
%

foldl( _, Z, [], Z ).
foldl( F, Y0, [ X | Xs ], Z ) :- F( Y0, X, Y1 ), foldl( F, Y1, Xs, Z ).



%
% Various List Predicates
%

prefix( L1, L12 ) :- append( L1, _, L12 ).

suffix( L2, L12 ) :- append( _, L2, L12 ).

member( X, Xs ) :- suffix( [ X | _ ], Xs ).

last( X, Xs ) :- suffix( [ X ], Xs ).

select( X, L, L12 ) :- append( L1, [ X | L2 ], L ), append( L1, L2, L12 ).

take( N, L12, L1 ) :- splitAt( N, L12, L1, _ ).

drop( N, L12, L2 ) :- splitAt( N, L12, _, L2 ).

nth( s( N ), Xs, X ) :- drop( N, Xs, [ X | _ ] ).

repeat( X, Xs ) :- applist( eq, X, Xs ).

any( R, Xs ) :- member( X, Xs ), R( X ).

foldr( F, Y0, L, Yn ) :- reverse( L, Xs ), foldl( F, Y0, Xs, Yn ). % de prepei na doulevei

headtail( [ X | Xs ], X, Xs ).

suffix_headtail( L, X, Xs ) :- suffix( [ X | Xs ], L ).

append2( L1, L2, L12 ) :- foldl( headtail, L12, L1, L2 ).

sublist( S, L ) :- foldl( suffix_headtail, L, S, _ ).

combs( L, N, C ) :- sublist( C, L ), length( C, N ). % combs( L, 0, C ) loop

concat( Ls, L ) :- foldl( append, [], Ls, L ). % concat( L, [] ) loop

% applist( leq, X, [ Y | 0 ] ) loop

combs2( _, 0, [] ).
combs2( L1, s( N ), [ X | C ] ) :- suffix( [ X | L ], L1 ), combs2( L, N, C ).




true2( X, Y ).
samelength( L1, L2 ) :- map( true2, L1, L2 ).


del( L1, X, L2 ) :- select( X, L1, L2 ).
permutation( L, P ) :- samelength( L, P ), foldl( del, L, P, [] ).



headtails( [], [], [] ).
headtails( [ [ H | T ] | Ls ], [ H | Hs ], [ T | Ts ] ) :- headtails( Ls, Hs, Ts ).




%
% Various Examples
%


hanoi( 0, S, M, D, [] ).
hanoi( s( N ), S, M, D, L ) :- hanoi( N, S, D, M, L1 ), append( L1, [ move( S, D ) ], L2 ), hanoi( N, M, S, D, L3 ), append( L2, L3, L ).



inbt( X, btree( X, L, R ) ).
inbt( X, btree( Y, L, R ) ) :- inbt( X, L ).
inbt( X, btree( Y, L, R ) ) :- inbt( X, R ).




pnats( N, L ) :- length( L, N ), applist( inc, 0, L ).




nqueens( N, L ) :-
	length( L, N ),
	add( N, N, s( K ) ),
	map( pnats, [ N, K ], [ C, D ] ),
	nqueens_search( L, C, C, D, D ).



nqueens_search( [], Dx, [], Xs, Ys ).
nqueens_search( [ Q | Qs ], Dx, Columns, Xs, Ys ) :-
	select( Q, Columns, Columns1 ),
	nth( Q, Dx, X ), select( X, Xs, Xs1 ),
	reverse( Dx, Dy ),
	nth( Q, Dy, Y ), select( Y, Ys, Ys1 ),
	map( inc, Dx, Dx1 ),
	nqueens_search( Qs, Dx1, Columns1, Xs1, Ys1 ).





inclist( L1, L2 ) :- map( inc, L1, L2 ).





nonzero( s( X ) ).

cont( Ys, Xs, N ) :- samelength( Xs, [ _ | L ] ), all( nonzero, L ), append( [ C | L ], [ D ], Ys ), foldl( add, 0, Xs, S ), foldl( substract, N, Ys, S ).





p( X ) :- p( X ).
p( 0 ).


empty( X ) :- empty( X ).


closure(R, X, Y) :- R(X, Y).
closure(R, X, Y) :- R(X, Z), closure(R, Z, Y).

ordered(R, []).
ordered(R, [X]).
ordered(R, [X, Y| Z]):- R(X,Y), ordered(R, [Y | Z]).
