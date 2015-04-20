:- module(macro__tr, [macro_translate/3], []).

% todo: document!!!
% 
% AUTHOR: jfran
%
% DESCRIPTION: Language extension to enable or disable certain parts 
% of code depending on the definition of 'values'.
% NOTE: All code (even that not included) must be syntactically correct.
% WARNING: Do not use it to select code for different architectures or
% your bootstraps won't be portable! (you will make bytecode depends on
% architecture)
%
% Little example:
%:- define(testing).
%:- if(testing ; unstable).
%:- else.
%:- fi.

% ---------------------------------------------------------------------------
% State of the translation

% Defined values
:- data defined/2.
% Stack of inserting values (the top says whether we are inserting 
% sentences or not)
:- data inserting/2.

% Clean all transformation state for this module
clean(Mod) :-
        retractall_fact(defined(_,Mod)),
        retractall_fact(inserting(_,Mod)).

% Get top stack value
get_inserting(Inserting0, Mod) :-
	% Check only first solution
	current_fact(inserting(Inserting, Mod)), !,
	Inserting = Inserting0. 
get_inserting(true, _) :- !. % insert by default

% Push a new value in the stack
push_inserting(Inserting, Mod) :-
	asserta_fact(inserting(Inserting, Mod)).

% Pop a value from the stack
pop_inserting(Inserting0, Mod) :-
	retract_fact(inserting(Inserting, Mod)), !,
	Inserting = Inserting0.

% ---------------------------------------------------------------------------
% Translation process

macro_translate(0, [], _Mod) :- !.
macro_translate(end_of_file, end_of_file, Mod) :- !,
	% todo: missing error messages if conditions are left open!
	clean(Mod).
macro_translate((:- if(Cond)), [], Mod) :- !,
	cond_begin(Cond, Mod).
macro_translate((:- else), [], Mod) :- !,
	cond_else(Mod).
macro_translate((:- fi), [], Mod) :- !,
	cond_end(Mod).
macro_translate(_, [], Mod) :-
	get_inserting(false, Mod), !.
% Ok, if we are here it means that we are inserting
macro_translate((:- define(Name)), [], Mod) :- !,
	define(Name, Mod).

cond_begin(Cond, Mod) :-
	( get_inserting(true, Mod),
	  eval_cond(Cond, Mod) ->
	    Inserting = true
	; Inserting = false
	),
	push_inserting(Inserting, Mod).

cond_else(Mod) :-
	pop_inserting(Inserting0, Mod),
        ( ( get_inserting(false, Mod) % parent is ignoring
	  ; Inserting0 = true % or cond was true
	  ) ->
	    Inserting = false
	; Inserting = true
	),
	push_inserting(Inserting, Mod).

cond_end(Mod) :-
	pop_inserting(_Inserting, Mod).

eval_cond(X, Mod) :- atom(X), !,
	defined(X, Mod).
eval_cond((X;Y), Mod) :-
	( eval_cond(X, Mod) -> true ; eval_cond(Y, Mod) ).
eval_cond((X,Y), Mod) :-
	eval_cond(X, Mod), eval_cond(Y, Mod).
eval_cond((\+ X), Mod) :-
	\+ eval_cond(X, Mod).

define(Name, Mod) :-
	current_fact(defined(Name, Mod)), !.
define(Name, Mod) :-
	asserta_fact(defined(Name, Mod)).
