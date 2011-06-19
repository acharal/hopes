/*
    Part of HOPES

    Copyright (C) 2006-2009 Constantinos Hadjopoulos <std01105@di.uoa.gr>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; see the file COPYING.  If not, write to
    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301, USA.

*/

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

