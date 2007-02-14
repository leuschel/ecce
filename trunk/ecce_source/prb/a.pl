:- module( a , _ ).

:- use_module(m1).
:- use_module(m2).
%:- include( 'b/b' ).

main :-
	display( hello ).
