:- module(index_tools,[non_indexed_unfolding/2,non_indexed_instantiation/2,indexed_varlist/1,indexed_atom/1]).


:- use_package( .('ecce_no_rt') ).

/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,1996,1997 */
/* --------------------------------------------- */

/* TO GET THE LATEST VERSION OF THE ECCE SYSTEM GO TO:
	http://www.cs.kuleuven.ac.be/~michael */
/* BUG REPORTS, QUESTIONS AND COMMENTS CAN BE SENT TO:
	michael@cs.kuleuven.ac.be */

:- use_module(bimtools).
:- use_module(code_generator).


/* file: index-tools.pro */

non_indexed_unfolding(SelLiteral,Varlist) :-
	copy(Varlist,OrigVarlist),
	claus(_Nr,SelLiteral,_Body),
	non_indexed_instantiation(Varlist,OrigVarlist),!.


non_indexed_instantiation([X|_T],[OrigX|_OrigT]) :-
	nonvar(OrigX), nonvar(X),
	not(indexed_atom(X)),
	not(variant_of(X,OrigX)).
	  /* -> we have an instantiation which will not
			 be captured by indexing */
non_indexed_instantiation([_X|T],[_OrigX|OrigT]) :-
	non_indexed_instantiation(T,OrigT).


indexed_varlist([]).
indexed_varlist([X|T]) :-
	indexed_atom(X),
	indexed_varlist(T).



indexed_atom(X) :- var(X),!.
indexed_atom(X) :- atomic(X),!.
indexed_atom(X) :-
	nonvar(X), X=.. [_Func|Args],
	is_list_of_free_variables(Args).
