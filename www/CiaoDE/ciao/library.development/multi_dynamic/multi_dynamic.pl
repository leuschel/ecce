:- module(multi_dynamic, [
	multi_new/1,

        multi_asserta_fact/2, 
	multi_asserta_fact/3, 
	multi_assertz_fact/2, 
	multi_assertz_fact/3,

        multi_current_fact/2, 
	multi_current_fact/3, 
	multi_retract_fact/2, 
	multi_retractall_fact/2,

        multi_set_fact/2, 
	multi_erase/2,

        multi_asserta/3, 
	multi_asserta/4, 
	multi_assertz/3, 
	multi_assertz/4, 

        multi_retract/3, 
	multi_retractall/2, 
	multi_abolish/2,

        multi_clause/3, 
	multi_clause/4, 
	multi_current_predicate/2,

        multi_dynamic/2, 
	multi_data/2,

	multi_call/2
        ],

        [assertions, isomodes]).

:- use_module(library(dynamic)).
:- use_module(library(prolog_sys), [new_atom/1]).

multi_new(Db) :-
	new_atom(Db).

multi_data(Db, Spec) :-
	indirect_spec(Db, Spec, Spec2),
	data(Spec2).

multi_dynamic(Db, Spec) :-
	indirect_spec(Db, Spec, Spec2),
	dynamic(Spec2).

multi_current_fact(Db, Fact) :-
	indirect_fact(Db, Fact, Fact2),
	current_fact(Fact2).

multi_current_fact(Db, Fact, Ref) :-
	indirect_fact(Db, Fact, Fact2),
	current_fact(Fact2, Ref).

multi_set_fact(Db, Fact) :-
	indirect_fact(Db, Fact, Fact2),
	set_fact(Fact2).

multi_erase(_Db, Ref) :-
	erase(Ref).

multi_asserta_fact(Db, Fact) :-
	indirect_fact(Db, Fact, Fact2),
	asserta_fact(Fact2).

multi_asserta_fact(Db, Fact, Ref) :-
	indirect_fact(Db, Fact, Fact2),
	asserta_fact(Fact2, Ref).

multi_assertz_fact(Db, Fact) :-
	indirect_fact(Db, Fact, Fact2),
	assertz_fact(Fact2).

multi_assertz_fact(Db, Fact, Ref) :-
	indirect_fact(Db, Fact, Fact2),
	assertz_fact(Fact2, Ref).

multi_retract_fact(Db, Fact) :-
	indirect_fact(Db, Fact, Fact2),
	retract_fact(Fact2).

multi_retractall_fact(Db, Fact) :-
	indirect_fact(Db, Fact, Fact2),
	retractall_fact(Fact2).

:- meta_predicate multi_asserta(?, ?, goal).

multi_asserta(Db, Head, Body) :-
	indirect_fact(Db, Head, Head2),
	cons_clause(Head2, Body, Clause),
	asserta(Clause).

:- meta_predicate multi_asserta(?, ?, goal, ?).

multi_asserta(Db, Head, Body, Ref) :-
	indirect_fact(Db, Head, Head2),
	cons_clause(Head2, Body, Clause),
	asserta(Clause, Ref).

:- meta_predicate multi_assertz(?, ?, goal).

multi_assertz(Db, Head, Body) :-
	indirect_fact(Db, Head, Head2),
	cons_clause(Head2, Body, Clause),
	assertz(Clause).

:- meta_predicate multi_assertz(?, ?, goal, ?).

multi_assertz(Db, Head, Body, Ref) :-
	indirect_fact(Db, Head, Head2),
	cons_clause(Head2, Body, Clause),
	assertz(Clause, Ref).

:- meta_predicate multi_retract(?, ?, goal).

multi_retract(Db, Head, Body) :-
	indirect_fact(Db, Head, Head2),
	retract((Head2 :- Body)).

multi_retractall(Db, Head) :-
	indirect_fact(Db, Head, Head2),
	retractall(Head2).

multi_abolish(Db, Spec) :-
	indirect_spec(Db, Spec, Spec2),
	abolish(Spec2).


multi_clause(Db, Head, Body) :-
	indirect_fact(Db, Head, Head2),
	clause(Head2, Body).

multi_clause(Db, Head, Body, Ref) :-
	indirect_fact(Db, Head, Head2),
	clause(Head2, Body, Ref).

multi_current_predicate(Db, Spec) :-
	indirect_spec(Db, Spec, Spec2),
	current_predicate(Spec2).

% Todo: multi_call(Db, g((''(X) :- X = 3))) ????
multi_call(Db, Fact) :-
	indirect_fact(Db, Fact, Fact2),
	Fact2.

% ---------------------------------------------------------------------------

indirect_spec(Db, Spec, Spec2) :-
	Spec = N/A,
	atom_concat(Db, N, N2),
	Spec2 = N2/A.

indirect_fact(Db, Fact, Fact2) :-
	Fact =.. [N|As],
	atom_concat(Db, N, N2),
	Fact2 =.. [N2|As].

% a hack
%cons_clause(Head, Body, (Head :- Body)).

cons_clause(Head, '$:'(Body), '$:'(':-'(Head2, Body))) :-
	Head =.. [N|As],
	atom_concat(multi_dynamic, ':', Q),
	atom_concat(Q, N, N2),
	Head2 =.. [N2|As].
