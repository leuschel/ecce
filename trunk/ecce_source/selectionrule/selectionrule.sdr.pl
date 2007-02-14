:- module('selectionrule.sdr',['selectionrule.sdr:select_positive_literal'/5]).

:- use_package( .('../ecce_no_rt2') ).

%:- dynamic select_positive_literal/5.

:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(library(lists)).

:- use_module('../bimtools').
:- use_module('../calc_chtree').


/* Synchronised Descent Rule as defined by Proietti/Pettorossi */

'selectionrule.sdr:select_positive_literal'(Goal,TopGoalVarlist,UnfHist,NrOfSel,SelLiteral) :-
	/* Try to find determinate literals */
	member_nr(SelLiteral,Goal,NrOfSel),
	not(is_negative_literal(SelLiteral,Atom)),
	not(is_built_in_literal(SelLiteral)),
	'selectionrule.sdr:ok_to_unfold'(Goal,NrOfSel,UnfHist).
'selectionrule.sdr:select_positive_literal'(Goal,TopGoalVarlist,[],1,SelLiteral) :-
	member_nr(SelLiteral,Goal,1).

'selectionrule.sdr:ok_to_unfold'(Goal,NrOfSel,[]) :-
	maximal_sdr(Goal,NrOfSel).

maximal_sdr(Goal,NrOfSel) :-
	pp_mnf(split_list(Goal,NrOfSel,Left,Sel,Right)),
	not(not_maximal_sdr(Sel,Left)),
	not(not_maximal_sdr(Sel,Right)).

not_maximal_sdr(Sel,List) :-
	member(Atom,List),
	greater_sdr(Atom,Sel).


greater_sdr(T,U) :-
	varlist(T,TV),
	varlist(U,UV),
	shared_variable(X,TV,UV),
	depth_sdr(X,T,0,DT),
	depth_sdr(X,U,0,DU),
	DT > DU.

shared_variable(X,TV,UV) :-
	member(V,TV),
	member(U,UV),
	V == U,
	X = V.

depth_sdr(X,T,Depth,Depth) :-
	var(T), X==T.
depth_sdr(X,T,InD,Depth) :-
	nonvar(T),
	T =.. [Pred|Args],
	InD1 is InD + 1,
	l_depth_sdr(X,Args,InD1,Depth).

l_depth_sdr(X,Args,InD,Depth) :-
	findall(ADepth,
		(member(A,Args), depth_sdr(X,A,InD,ADepth)),
		Ds),
	Ds \== [],
	get_maximum(Ds,0,Depth).

get_maximum([],MaxSoFar,MaxSoFar).
get_maximum([H|T],MaxSoFar,Max) :-
	((H>MaxSoFar)
	 -> get_maximum(T,H,Max)
	 ;  get_maximum(T,MaxSoFar,Max)
	).
