:- module(catcher_rt,[catcher_handler/6],[]).
:- use_module(library(terms),[atom_concat/2]).
:- use_module(library(write)).

:- set_prolog_flag(write_strings, on).

catcher_handler(Module,Functor,Arity,Pred,Message,Error) :- !,
	atom_number(ArityA,Arity),
	atom_concat(['{NOTE: Raised exception at ',
	Module,':',Functor,'/',ArityA, ' ', Message, ': '],M),
	display(M),
	write(Pred),display('}\n'),
	throw(Error).
