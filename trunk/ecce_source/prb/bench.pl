
time(Goal,Time) :- 
	statistics(runtime,[Global1,_]),
	call(Goal),
	statistics(runtime,[Global2,_TimeSinceLastStat]),
	Time is Global2 - Global1.


main :-
% 	statistics(runtime,[Global1,_]),
% 	test_index,
% 	statistics(runtime,[Global2,_]),
% 	X is Global2 - Global1,
	time( test_index , X ),
	display( 'Indexating: ' ) , display( X ) , nl,

% 	statistics(runtime,[G1,_]),
% 	test_no_index,
% 	statistics(runtime,[G2,_]),
% 	Y is G2 - G1,
	time( test_no_index , Y ),
	display( 'No Indexating: ' ) , display( Y ) , nl.



test_index  :-
	test_index_2( 2000000 ).

test_index_2( 0 ).
test_index_2( N ) :-
	N1 is N - 1,
	test_index_2( N1 ),
	index( 3  , _ ),
	index( 6  , _ ),
	index( 1  , _ ),
	index( 3  , _ ),
	index( 16 , _ ),
	index( 20 , _ ).
	

test_no_index  :-
	test_no_index_2( 2000000 ).

test_no_index_2( 0 ).
test_no_index_2( N ) :-
	N1 is N - 1,
	test_no_index_2( N1 ),
	no_index( _ , 3 ),
	no_index( _ , 6 ),
	no_index( _ , 1 ),
	no_index( _ , 3 ),
	no_index( _ , 16 ),
	no_index( _ , 20 ).
	


index(  1  , X ) :- var( X ) , !.
index(  2  , X ) :- var( X ) , !.
index(  3  , X ) :- var( X ) , !.
index(  4  , X ) :- var( X ) , !.
index(  5  , X ) :- var( X ) , !.
index(  6  , X ) :- var( X ) , !.
index(  7  , X ) :- var( X ) , !.
index(  8  , X ) :- var( X ) , !.
index(  9  , X ) :- var( X ) , !.
index( 10  , X ) :- var( X ) , !.
index( 11  , X ) :- var( X ) , !.
index( 12  , X ) :- var( X ) , !.
index( 13  , X ) :- var( X ) , !.
index( 14  , X ) :- var( X ) , !.
index( 15  , X ) :- var( X ) , !.
index( 16  , X ) :- var( X ) , !.
index( 17  , X ) :- var( X ) , !.
index( 18  , X ) :- var( X ) , !.
index( 19  , X ) :- var( X ) , !.
index( 20  , X ) :- var( X ) , !.


no_index(  X  , 1 ) :- var( X ) , !.
no_index(  X  , 2 ) :- var( X ) , !.
no_index(  X  , 3 ) :- var( X ) , !.
no_index(  X  , 4 ) :- var( X ) , !.
no_index(  X  , 5 ) :- var( X ) , !.
no_index(  X  , 6 ) :- var( X ) , !.
no_index(  X  , 7 ) :- var( X ) , !.
no_index(  X  , 8 ) :- var( X ) , !.
no_index(  X  , 9 ) :- var( X ) , !.
no_index( X  , 10 ) :- var( X ) , !.
no_index( X  , 11 ) :- var( X ) , !.
no_index( X  , 12 ) :- var( X ) , !.
no_index( X  , 13 ) :- var( X ) , !.
no_index( X  , 14 ) :- var( X ) , !.
no_index( X  , 15 ) :- var( X ) , !.
no_index( X  , 16 ) :- var( X ) , !.
no_index( X  , 17 ) :- var( X ) , !.
no_index( X  , 18 ) :- var( X ) , !.
no_index( X  , 19 ) :- var( X ) , !.
no_index( X  , 20 ) :- var( X ) , !.
