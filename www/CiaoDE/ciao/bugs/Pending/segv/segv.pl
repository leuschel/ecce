% To test: 
% ciaoc segv.pl; ./segv

:- module(_,_,[]).

main :-	r.

:- concurrent t/0.

r :-
	display(t),
	asserta_fact(t),
	retract_fact(t),
	r.
