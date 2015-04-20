/*             Copyright (C)1990-2002 UPM-CLIP				*/

/* THIS NEEDS TO BE REVISED */

%% :- use_module(library('assertions/assertions.pl')).
%% 
%% %----------------------------------------------------------------------------
%% :- comment(title,"CIAO syntax support for Prolog systems (e.g., SICStus").
%% %----------------------------------------------------------------------------
%% 
%% :- comment(module,"This module provides suitable definitions for
%%    loading CIAO files under Prolog Systems.").
%% 
%% :- comment(bug,"This is very incomplete. It needs to be unified with
%%    the file used in the CIAO-2.3 compiler.").
%% 
%% :- comment(version(0*5#0,1997/9/12), "Separated engine/compiler 
%%    dependent stuff into this file. (Manuel Hermenegildo)").

%----------------------------------------------------------------------------
% Syntax support for CIAO's operators
%----------------------------------------------------------------------------

:- op(950,xfy,[(&)]).

:- op(950,xfy,[(\&)]).

:- op(950,xf,[(&)]).

:- op(1000,xfy,[(&&)]).

%----------------------------------------------------------------------------

%% Needed locally.
local_append([],Ys,Ys).
local_append([E|Xs],Ys,[E|Zs]):- local_append(Xs,Ys,Zs).

%----------------------------------------------------------------------------
% Emulate CIAO's additional module primitives
%----------------------------------------------------------------------------

% New directives will simply be ignored.

ccomp(M,Args) :- 
	local_append("[CIAO-SICSTUS compatibility: ",M,T1),
	local_append(T1,"]~n",F),
	format(user_error,F,Args).

term_expansion(( :- new_declaration(D) ),
               ( :- ( D=F/N,
		      functor(Skel,F,N),
		      %% user:ccomp("Asserting ~w",[Skel]),
		      assert(Skel) ))).

term_expansion(:-(use_module(X,Y)), 
	      (:-(user:load_ciao_module(X,Y)))).

term_expansion(:-(use_module(X)), 
	      (:-(user:load_ciao_module(X)))).

load_ciao_module(M) :-
	not_in_iso(M),
	!,
	ccomp("Loading ciao module ~w",[M]),
	use_module(M).
load_ciao_module(M) :- 
	ccomp("Ignoring ciao (iso) module ~w",[M]).

load_ciao_module(M,I) :-
	not_in_iso(M),
	!,
	ccomp("Loading ciao module ~w (explicit imports ~w)",[M,I]),
	use_module(M,I).
load_ciao_module(M,_I) :- 
	ccomp("Ignoring ciao (iso) module ~w (explicit imports)",[M]).

term_expansion( ( :- module(M,E,S) ), 
	        ( [:-(module(M,E)), :-(load_syntax_modules(S,M))] )).

%% Adds library( ) wrapper, ignores 
load_syntax_modules([],_CM) :- 
	!.
load_syntax_modules([M|Ms],CM) :- 
	!, 
	load_syntax_module(M,CM),
	load_syntax_modules(Ms,CM).
load_syntax_modules(M,CM) :- 
	!, 
	load_syntax_module(M,CM).

load_syntax_module(M,CM) :-
	not_in_iso(M),
	!,
	ccomp("Loading ciao syntax module ~w into ~w",[M,CM]),
	consult(CM:library(M)).
load_syntax_module(M,_CM) :- 
	ccomp("Ignoring ciao syntax (iso) module ~w",[M]).

not_in_iso(M) :- nonvar(M), \+ in_iso(M).

in_iso(dcg). 
in_iso(dcg_expansion). 
in_iso(aggregates). 
in_iso(format). 
in_iso(streams). 
in_iso(write). 
in_iso(ttyout). 
in_iso(dynamic). 
in_iso(library(X)) :- in_iso(X).

term_expansion(:-(data(X)),  
	       :-(user:ccomp("Defining data as dynamic: ~w",[X]),dynamic(X))).

term_expansion(?-(syntax(M)),  
	       ?-(user:load_syntax_module(M,user))).
term_expansion(:-(syntax(M)),  
	       :-(user:load_syntax_module(M,user))).

term_expansion(?-(include(X)), 
	       ?-(user:ccomp("Loading included file(s) ~w",[X]),
	          consult(X))).
term_expansion(:-(include(X)), 
	       :-(user:ccomp("Loading included file(s) ~w",[X]),
	          consult(X))).
term_expansion(:-(use_file(File,When)),:-(user:file_use(When,File))).

:- meta_predicate(file_use(?,:)).

file_use(compatibility,M:File):-
	compatibility(S),
	name(File,StringF),
	name(S,StringS),
	"_"=[X],
	local_append(StringF,[X|StringS],StringFile),
	name(TheFile,StringFile),
	use_module(M:TheFile).

set_prolog_flag(X,Y) :- prolog_flag(X,_,Y).

%----------------------------------------------------------------------------
% Emulate CIAO's 'make'
%----------------------------------------------------------------------------

make_exec(List,System):-
	prolog_flag(compiling,_,fastcode),
%%	compile(List),
	% Otherwise SICStus insists on loading the CIAO .ql files...
	consult(List),
%	save(System),
	start_program.

start_program :-
	main,
	halt.
start_program :-
	format(user_error,"[Program ended with failure]~n",[]),
	halt.
