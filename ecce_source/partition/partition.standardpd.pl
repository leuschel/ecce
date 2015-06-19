:- module('partition.standardpd',['partition.standardpd:partition_goal'/3]).


:- use_package( .('../ecce_no_rt2') ).

%:- dynamic partition_goal/3.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').


'partition.standardpd:partition_goal'([],_Nrs,[]).
'partition.standardpd:partition_goal'([Lit|T],[SelNr|TN],[split_goal([PosAtom],built_in(SelNr))|ST]) :-
	is_built_in_literal(Lit),
	extract_positive_atom_from_literal(Lit,PosAtom),!,
	'partition.standardpd:partition_goal'(T,TN,ST).
'partition.standardpd:partition_goal'([Lit|T],[SelNr|TN],[split_goal([PosAtom],neg(SelNr))|ST]) :-
	is_negative_literal(Lit,_),
	extract_positive_atom_from_literal(Lit,PosAtom),!,
	'partition.standardpd:partition_goal'(T,TN,ST).
'partition.standardpd:partition_goal'([Lit|T],[SelNr|TN],[split_goal([Lit],[SelNr])|ST]) :-
	\+(is_built_in_literal(Lit)),
	\+(is_negative_literal(Lit,Atom)),!,
	'partition.standardpd:partition_goal'(T,TN,ST).
'partition.standardpd:partition_goal'([Lit|T],[SelNr|TN],ST) :-
	'partition.standardpd:partition_goal'(T,TN,ST).

