% package for AMS configuration files

:- use_module(library('compiler/c_itf'),[opt_suffix/2]).
:- use_module(library('compiler/c_itf')).
:- use_module(library(compiler),[make_po/1]).
:- use_module(library(messages)).
%:- set_prolog_flag(unknown,fail).  % does not work!
:- initialization('$precompile').

'$precompile':-
	current_prolog_flag(argv,Args),
	separate_ciaoc_args(Args,_MainArgs,PrecompArgs),
%	( main(MainArgs) -> true ; main ),
	main,
	precompile(PrecompArgs),
	opt_suffix(_,'_client'),
	display(done), nl. % {Program ended with failure}
% fails because of calling process_files_from

:- data active/3.
:- data app/1.

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

precompile(Files):-
	precompile_each(Files),
	cleanup_c_itf_data.

precompile_each([]).
precompile_each([File|Files]):-
% this makes ciaoc fail!!
	process_files_from(File,ams,any,precompile_,false,false,active_),
	precompile_each(Files).

active_(Base):-
	module_from_base(Base,M),
	active(M,_,_).

precompile_(Base):-
	active_(Base),
%	etc...
	atom_concat(Base,'_server',Server),
	make_po(Server).

% No good:
separate_ciaoc_args([X],[],[X]).
separate_ciaoc_args([_|Xs],A,B):-
	separate_ciaoc_args(Xs,A,B).
