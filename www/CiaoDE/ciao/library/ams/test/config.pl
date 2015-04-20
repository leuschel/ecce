:- module(config,[active_module/3,precompile/1,separate_ciaoc_args/3],[]).

:- use_module(library('compiler/c_itf')).
:- use_module(library(compiler),[make_po/1]).
:- use_module(library(messages)).

:- data active/3.

ams_mode(private).
ams_mode(shared(App)):- atom(App).
ams_mode(public(Id)):- atom(Id).

active_module(M,Host,Mode):-
	atom(M),
	atom(Host),
	ground(Mode),
	ams_mode(Mode), !,
	asserta_fact(active(M,Host,Mode)).
active_module(M,Host,Mode):-
	error_message("Incorrect arguments: ~q",[active_module(M,Host,Mode)]).

%% application(App):-
%% 	atom(App), !,
%% 	( app(App0)
%% 	-> ( App0=App
%% 	   -> true
%% 	    ; error_message("Different apps declared: ~q ~q",[App,App0])
%% 	   )
%% 	; asserta_fact(app(App))
%% 	).
%% application(App):-
%% 	error_message("Incorrect argument: ~q",[application(App)]).

precompile(Files):-
	precompile_each(Files),
	cleanup_c_itf_data.
%% precompile(_Files):-
%% 	error_message("No app declared").

precompile_each([]).
precompile_each([File|Files]):-
	process_files_from(File,ams,any,precompile_,false,false,active_),
	precompile_each(Files).

active_(Base):-
	module_from_base(Base,M),
	active(M,_,_).

precompile_(Base):-
	active_(Base), !,
%	etc...
	atom_concat(Base,'_server',Server),
	make_po(Server).
precompile_(_Base).

% No good:
separate_ciaoc_args([X],[],[X]).
separate_ciaoc_args([_|Xs],A,B):-
	separate_ciaoc_args(Xs,A,B).
