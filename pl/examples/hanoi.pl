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


hanoi( 0, S, M, D, [] ).
hanoi( s( N ), S, M, D, L ) :-
    hanoi( N, S, D, M, L1 ), 
    append( L1, [ move( S, D ) ], L2 ), 
    hanoi( N, M, S, D, L3 ), 
    append( L2, L3, L ).
