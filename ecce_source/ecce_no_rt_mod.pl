:- module( _ , [remove_prepost/2], [] ).

remove_prepost( X , Y ) :-
	X =.. [Call,Y],
	member( Call , [pp_mnf, 
	                pp_cll,
			prepost_call,
			prepost_mnf_call,
			mnf_call] ).

remove_prepost( not(X) , \+ X ).
remove_prepost( bd_findall(X,G,L) , findall(X,G,L) ) :- !.
remove_prepost( if_inf(_X) , fail ) :- !.

