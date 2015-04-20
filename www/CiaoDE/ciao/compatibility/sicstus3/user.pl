/*             Copyright (C)1990-2002 UPM-CLIP				*/

%----------------------------------------------------------------------------
% Support for misc CIAO functionality in std Prolog systems (e.g., SICStus).
%----------------------------------------------------------------------------

%----------------------------------------------------------------------------
% Syntax support for CIAO's operators
%----------------------------------------------------------------------------

:- op(1190,fx,[(typedef),(type)]).

:- op(1180,xfx,[(::=)]).

:- op(1150,fx,[(data)]).

:- op(950,xfy,[(&)]).

:- op(950,xfy,[(\&)]).

:- op(950,xf,[(&)]).

:- op(1000,xfy,[(&&)]).

%----------------------------------------------------------------------------
% Needed locally.
%----------------------------------------------------------------------------

local_append([],Ys,Ys).
local_append([E|Xs],Ys,[E|Zs]):- local_append(Xs,Ys,Zs).

ccomp(M,Args) :- 
	local_append("[CIAO-SICSTUS compatibility: ",M,T1),
	local_append(T1,"]~n",F),
	format(user_error,F,Args).

%----------------------------------------------------------------------------
% Set paths for sub-libraries
%----------------------------------------------------------------------------

add_ciao_lib_dir(DS,SL) :- 
	local_append(DS,SL,NDS), 
	name(ND,NDS),
	asserta(user:library_directory(ND)).

:- ciao_library_directory(D),           % last in sicstus lib
   name(D,DS),
   add_ciao_lib_dir(DS,"/contrib"),
   add_ciao_lib_dir(DS,"/library"),
   add_ciao_lib_dir(DS,"/lib"),
   add_ciao_lib_dir(DS,""),             % second in ciao libs
   compat_library_directory(DC),
   name(DC,DCS),
   add_ciao_lib_dir(DCS,"").            % first look in compat

:- multifile file_search_path/2.
:- dynamic file_search_path/2.

% file_search_path(engine,E) :- comp_engine_directory(E).
file_search_path(engine,E) :- library_directory(E).

%----------------------------------------------------------------------------
% Emulate CIAO's directives
%----------------------------------------------------------------------------

:- dynamic term_expansion/2.

term_expansion(( :- add_sentence_trans(F/2) ),
	       ( :- Expansion=..[F,A,B],
		    assertz( (term_expansion(A,B):-Expansion) ) )).

term_expansion(( :- meta_predicate(error_protect(goal)) ),
	       ( :- meta_predicate(error_protect(:)) )).
term_expansion(( :- meta_predicate(ctrlc_clean(goal)) ),
	       ( :- meta_predicate(ctrlc_clean(:)) )).
term_expansion(( :- meta_predicate(trace(goal)) ),
	       ( :- meta_predicate(trace(:)) )).
/* Does not work
term_expansion(( :- meta_predicate(Spec0) ),
	       ( :- ( user:translate_meta_spec(Spec0,Spec1),
		      meta_predicate(Spec1) ) )).
*/

term_expansion(( :- new_declaration(D,_) ),
               ( :- ( D=F/N,
		      functor(Skel,F,N),
		      %% user:ccomp("Asserting ~w",[Skel]),
		      assert(Skel) ))).

term_expansion(( :- new_declaration(D) ),
               ( :- ( D=F/N,
		      functor(Skel,F,N),
		      %% user:ccomp("Asserting ~w",[Skel]),
		      assert(Skel) ))).

translate_meta_spec((Spec0,Specs0),(Spec1,Specs1)):- !,
	translate_meta_spec(Spec0,Spec1),
	translate_meta_spec(Specs0,Specs1).
translate_meta_spec(Spec0,Spec1):-
	Spec0=..[F|Args0],
	translate_meta_args(Args0,Args1),
	Spec1=..[F|Args1],
	ccomp("Translated meta-predicate: ~w",[Spec1]).

translate_meta_args([Arg0|Args0],[Arg1|Args1]):-
	translate_meta_arg(Arg0,Arg1),
	translate_meta_args(Args0,Args1).
translate_meta_args([],[]).

translate_meta_arg(goal,(:)):- !.
translate_meta_arg(A,A).

%----------------------------------------------------------------------------
% Emulate CIAO's additional module/file primitives
%----------------------------------------------------------------------------

term_expansion( ( :- module(M,E,S) ), 
	        ( [(:- module(M,E)),
		   (:- user:add_builtin_library(M)),
		   (:- user:load_syntax_modules(S,M))
		  ]  )).

term_expansion( ( :- module(M,E) ), 
	        ( [(:- module(M,E)),
		   (:- user:add_builtin_library(M))
		  ]  )).

add_builtin_library(builtin):- !.
add_builtin_library(M):- compile(M:library(builtin)).

%% Adds library( ) wrapper
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
	user:ccomp("Loading ciao syntax module ~w into ~w",[M,CM]),
	compile(CM:library(M)).

term_expansion(( :- data(X) ),  
	       ( [(:- user:ccomp("Defining data as dynamic: ~w",[X])),
		  (:- dynamic(X)) ] )). 

term_expansion(( :- syntax(M) ),  
	       ( :- user:load_syntax_modules(M,user) )).

term_expansion(( :- include(X) ), 
	       ( :- ( user:ccomp("Loading included file(s) ~w",[X]),
	             compile(X)) )).

term_expansion(( :- load_compilation_module(File) ),
	       ( :- translation:use_module(File) )).

%----------------------------------------------------------------------------
% ISO compliance
%----------------------------------------------------------------------------

term_expansion(( :- initialization(Goal) ),
	       ( :- ( user:ccomp("Executing initialization goal ~q",[Goal]),
		      Goal ) )).

%----------------------------------------------------------------------------
% CIAO's peculiarities
%----------------------------------------------------------------------------

% For some strange reason SICStus refuses to load write.pl into c_itf
% but it does it for metaterms ?????
% goal_expansion(write_term(T,L), c_itf, ciao_write_term(T,L)).
goal_expansion(write_term(T,L), c_itf, metaterms:ciao_write_term(T,L)).
goal_expansion(this_module(M), c_itf,  M = c_itf).

%----------------------------------------------------------------------------
% CIAO's 'make'
%----------------------------------------------------------------------------

make_exec(List,System):-
%% 	compile(List),
	consult(List),
	save(System),
	start_program.

start_program :-
	main,
	halt.
start_program :-
	format(user_error,"[Program ended with failure]~n",[]),
	halt.

makeql(X):- fcompile(X).

%----------------------------------------------------------------------------
% Finally, load the compatibility with the basic builtins
%----------------------------------------------------------------------------

:- use_module(builtin).
