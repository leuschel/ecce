:- module(lazytr, [lazy_sentence_translation/3], []).

:- use_module(library(terms), [copy_args/3, atom_concat/2]).
:- use_module(library(lists), [reverse/2]).
:- use_module(library(aggregates)).

:- data lazy/4.
:- data newp/3.
:- data done/2.

make_lazy(Module, Predicate, Freeze, New) :-
        asserta_fact(lazy(Module, Predicate, Freeze, New)).

make_newp(Module, Head, Body) :-
        asserta_fact(newp(Module, Head, Body)).

make_done(Module, Head) :-
        asserta_fact(done(Module, Head)).

lazy_sentence_translation(0, _, Module) :-
	!,
        retractall_fact(lazy(Module, _, _, _)),
	retractall_fact(newp(Module, _, _)),
	retractall_fact(done(Module, _)).
lazy_sentence_translation(end_of_file, List, _) :-
	findall(
		   (Predicate :- Body),
		   newp(_, Predicate, Body),
		   List1
	       ),
	reverse([end_of_file|List1], List),
        retractall_fact(lazy(Module, _, _, _)),
	retractall_fact(newp(Module, _, _)),
	retractall_fact(done(Module, _)),
	!.
lazy_sentence_translation((?- _), _, _) :-
	!,
	fail.
% lazy_sentence_translation((:- lazy(function(Specification))), _, Module) :-
% 	Specification = Name/Args,
% 	NewArgs is Args + 1,
% 	NewSpec = Name/NewArgs,
% 	lazy_sentence_translation((:- lazy(NewSpec)), _, Module).
lazy_sentence_translation((:- lazy(Specification)), _, Module) :-
	!,
	(
	    Specification = Name/Args, functor(Predicate, Name, Args) ->
	    (
		atom_concat([Name, '_$$lazy$$'], LazyName),
		Predicate =.. [_|Arguments],
		New =.. [LazyName|Arguments],
		functor(Head, Name, Args),
		make_lazy(Module, Predicate, Head, New)
	    )
	;
	    inform_user(['Invalid lazy specification: ', Specification])
	).
lazy_sentence_translation((:- _), _, _) :-
	!,
	fail.
lazy_sentence_translation((Head :- Body), (NewHead :- NewBody), Module) :-
	!,
        lazy_sentence_translation_analyze(Module, Head, Body,
	                                  NewHead, NewBody).
lazy_sentence_translation(Head, (NewHead :- NewBody), Module) :-
        lazy_sentence_translation_analyze(Module, Head, true,
	                                  NewHead, NewBody).

lazy_sentence_translation_analyze(Module, Head, Body, NewHead, NewBody) :-
	lazy(Module, Head, Freeze, New),
	!,
	functor(Head, Name, A),
	arg(A, Head, Value),
 	(
	    (   % ground
		( ground(Value) ; done(Module, Name) ) -> 
		  make_newp(Module, New, Body)
	    )
	;
	    (   % rest
		functor(New, LazyName, _),
		Freeze =.. [_|Arguments],
		FreezePart =.. [LazyName|Arguments],
		arg(A, Freeze, Result),
		NewHead = Freeze,
		NewBody = (freeze(Result, (Module:FreezePart))),
		make_done(Module, Name),
		make_newp(Module, New, Body)
	    )
	).
