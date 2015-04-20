:- module('partition.naive',['partition.naive:partition_goal'/3]).

%:- dynamic partition_goal/3.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').

:- use_package( .('../ecce_no_rt2') ).

'partition.naive:partition_goal'([],_Nrs,[]).
'partition.naive:partition_goal'([Lit|T],[SelNr|TN],[split_goal([DynLit],built_in(SelNr))|ST]) :-
	is_built_in_literal(Lit),
	extract_positive_atom_from_literal(Lit,PosAtom),!,
	make_fully_dynamic(PosAtom,DynLit),
	'partition.naive:partition_goal'(T,TN,ST).
'partition.naive:partition_goal'([Lit|T],[SelNr|TN],[split_goal([DynLit],neg(SelNr))|ST]) :-
	is_negative_literal(Lit,_),
	extract_positive_atom_from_literal(Lit,PosAtom),!,
	make_fully_dynamic(PosAtom,DynLit),
	'partition.naive:partition_goal'(T,TN,ST).
'partition.naive:partition_goal'([Lit|T],[SelNr|TN],[split_goal([DynLit],[SelNr])|ST]) :-
	not(is_built_in_literal(Lit)),
	not(is_negative_literal(Lit,Atom)),!,
	make_fully_dynamic(Lit,DynLit),
	'partition.naive:partition_goal'(T,TN,ST).
'partition.naive:partition_goal'([Lit|T],[SelNr|TN],ST) :-
	'partition.naive:partition_goal'(T,TN,ST).



make_fully_dynamic(X,Z) :- /* make non-ground args fully dynamic */
	X=..[Func|XA],
	l_make_fully_dynamic(XA,ZA),!,
	Z=..[Func|ZA].
make_fully_dynamic(X,_Z) :- print('*** error make_fully_dynamic failed'),nl.

l_make_fully_dynamic([],[]).
l_make_fully_dynamic([X|XA],[X|ZA]) :-
	ground(X),!,l_make_fully_dynamic(XA,ZA).
l_make_fully_dynamic([X|XA],[_|ZA]) :-
	l_make_fully_dynamic(XA,ZA).
