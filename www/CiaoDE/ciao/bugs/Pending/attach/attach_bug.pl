% This module has been designed for meausering the time cost of putting an
% attribute to a variable with or without global attributes activated.

:- module( attach_bug , _ , _ ).

:- use_module( library( prolog_sys ) ).


put_simple_attr( X , T ) :-
	_X =[1,2,3,4,5,6,7,8,9,9999,0],
	statistics( runtime , [Start , _ ] ),
	attach_attribute(_ , X),
	attach_attribute(_ , X),
	attach_attribute(_ , X),
	attach_attribute(_ , X),
	attach_attribute(_ , X),
	attach_attribute(_ , X),
	attach_attribute(_ , X),
	attach_attribute(_ , X),
	attach_attribute(_ , X),
	statistics( runtime , [End , _ ] ),
	T is End - Start.



exec_ntimes( _P , N ) :-
	N > 0,
	!,
	put_simple_attr( pppppppppp(aaaaaaa) , _ ),
%	call( P ),
	N1 is N - 1,
	exec_ntimes( _P , N1 ).

exec_ntimes( _ , 0 ).

main :-
	exec_ntimes( _ , 1000000 ).
