/* @(#)indexer.pl	1.6 06 Oct 1993

Since this file is only used by term_expansion/2, it should be loaded with

:- load_files(indexer(indexer), when(compile_time),if(changed)]).

The ":- index" directive supplied by this file is described in the
README file in this directory.

This module is intended for use in indexing compiled tables of prolog
facts. If you wish to index dynamic facts, see dynamic_indexer.pl.

This software is free: please copy it and/or modify it. Do with it what
you like.  It is a gift from KnowledgeWare Inc, and was written by Tom
Howland.

*/

:- module(indexer_tr,[ expand_index/3 ], []).

:- use_module(library('indexer/low')).
:- use_module(library('indexer/hash')).
:- use_module(library(aggregates),[findall/3]).
:- use_module(library(dynamic),[assert/1]).
:- use_module(library(lists),[append/3]).

:- use_module(library(write)).

:- data clause_count/4, indexed/4.
:- dynamic '** hash index **'/4.

expand_index((:-index(Specs)), Clauses, Mod) :-
    index_clauses(Specs, _, [], Head, NewHead, NewName, NewArity,
		  FirstArg, Body, Hash, Hash),
    functor(Head, Name, Arity),
    asserta_fact(clause_count(Name, Arity, Mod, 0)),
    asserta_fact(indexed(Head, Mod, FirstArg, NewHead)),
    parse_index_specs(Specs, Head, Mod, Mod, FirstArg, Body, _, _, _, Dis,
		      Com, _, _, _, _),
    append(Dis, [(:-discontiguous NewName/NewArity), (Head:-Body,NewHead)],
	   Clauses),
    assert_each(Com).
expand_index((:-_), _, _Mod) :- !, fail.
expand_index(Rule, [NewRule|Clauses], Mod) :-
    head(Rule, Head),
    functor(Head, Name, Arity),
    indexed(Head, Mod, ClNum, NewHead),
    retract_fact(clause_count(Name, Arity, Mod, ClNum)),
    X is ClNum+1,
    asserta_fact(clause_count(Name, Arity, Mod, X)),
    new_rule(Rule, NewHead, NewRule),
    findall(Ming, '** hash index **'(Head, Mod, ClNum, Ming), Clauses),
    display(Clauses), nl.

head((X:-_), X) :- !.
head(X, X).

new_rule((_:-Y), N, (N:-Y)) :- !.
new_rule(_, N, N).

assert_each([]).
assert_each([H|T]) :- assert(H), assert_each(T).
/* debug version:
assert_each([H0|T]) :- 
	debug(H0,H),
	\+ \+ ( numbervars(H,0,_), writeq(assert(H)) ), nl,
	assert(H), assert_each(T).

debug((H:-B),(H:-display(H),nl,B)):- !.
debug(H,H).
*/
