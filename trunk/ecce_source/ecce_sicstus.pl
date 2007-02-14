/* ================================= */
/*   E C C E    for Sicstus Prolog   */
/* ================================= */


/* --------------------------------------------- */
/* (C) COPYRIGHT MICHAEL LEUSCHEL 1995,96,97,98  */
/* --------------------------------------------- */


/* load this file to start up the ECCE system in Sicstus Prolog */


/* Some term_expansions needed to make mecce-ciao run in sicstus again */

:- op( 1000 , fx , data ).


%term_expansion( :-(initialization( X )) , :-(X) ).

term_expansion( :-(use_module(library(dec10_io))), [] ).

term_expansion( :-(use_module(library(dynamic))), [] ).

term_expansion( :-(use_module(library(aggregates))), [] ).

term_expansion( :-(use_module(library(sort))), [] ).

term_expansion( :-(use_module(library(prolog_sys))), [] ).

term_expansion( :-(use_module(engine(internals))), [] ).

term_expansion( :-(use_module(engine(internals),_)), [] ).

term_expansion( :-(use_package(_)), [] ).

term_expansion( :-(meta_predicate(_)), [] ).
 
term_expansion( :-(set_prolog_flag(multi_arity_warnings,X)) , 
	        :-(set_prolog_flag(discontiguous_warnings,X)) ).

term_expansion( :-(data(X))    , 
	        :-(dynamic(X)) ).

term_expansion( :-(include('bimtools/ciao_specific.pl')), 
	        :-(ensure_loaded('bimtools/sicstus_specific.pl')) ).

term_expansion( :-(export(_)), [] ).

term_expansion( :-(use_module('bimtools/makeflat')) , :-(use_module(makeflat)) ).

term_expansion( :-(use_module('bimtools/makeiff')) , :-(use_module(makeiff)) ).

term_expansion( :-(module( _ )) , [] ).

term_expansion( :-(module( _ , _ )) , [] ).

term_expansion( :-(reexport( _ , _ )) , [] ).

%term_expansion( :-(use_module(X)) , :-(ensure_loaded(X)) ) :-
%	X \= library( _ ).



%term_expansion( :-(use_module( X )), :-(use_module( Y )) ) :-
%	X \= library( _ ),
%	ecce_source_directory(Dir),
%	atom_concat(Dir,X,Y).

%term_expansion( X , _ ) :-
%	display( X ) , nl , nl,
%	fail.


goal_expansion( retract_fact( X ) , _ , retract( X ) ).

goal_expansion( asserta_fact( X ) , _ ,  assert( X ) ).

goal_expansion( assertz_fact( X ) , _ ,  assert( X ) ).


:- use_module(library(system)).

ecce_source_directory(Res) :- environ('ECCE_SOURCE',R),
  string_concatenate(R,'/',Res).
ecce_benchmark_directory(Res) :- environ('ECCE_BENCHMARKS',R),
  string_concatenate(R,'/',Res).

/* --------------------- */
/* --------------------- */

:- use_module(library(lists)).
:- mode string_concatenate(i,i,o).
string_concatenate(X,Y,XY) :-
   name(X,Xs),name(Y,Ys),append(Xs,Ys,XYs),name(XY,XYs).

ensure_consulted(File) :- 
	ecce_source_directory(Dir),
	string_concatenate(Dir,File,CF),!,
	ensure_loaded(CF).

ecce_reconsult(File) :-
	ecce_source_directory(Dir),
	string_concatenate(Dir,File,CF),!,
	consult_without_redefine_warning(CF).

ecce_compile(File) :-
	ecce_source_directory(Dir),
	string_concatenate(Dir,File,CF),!,
	fcompile(CF).

ecce_use_module(File) :- 
	ecce_source_directory(Dir),
	string_concatenate(Dir,File,CF),!,
	use_module(CF).
ecce_use_module(File,A1,A2) :- 
	ecce_source_directory(Dir),
	string_concatenate(Dir,File,CF),!,
	use_module(CF,A1,A2).
	
	
	
:- use_module(library(charsio),[read_from_chars/2]).
convert_cli_into_atom(CLIGOAL,Atom) :-
  name(CLIGOAL,AsciiL),add_dot(AsciiL,AL2),read_from_chars(AL2,Atom).
  


ecce_recompile :- ecce_reconsult('ecce_compile').
	
:- ensure_consulted('bimtools/sicstus_specific.pl').

:- ensure_consulted('ecce_main.pl').

:- nl,nl,print('Type "ecce." and hit return to start the system'),nl.


main_sicstus :-
   prolog_flag(argv,ArgV),
   main(ArgV).
   
runtime_entry(start) :- main_sicstus.

%:- main_sicstus.

%:- ecce.


% RUNTIME ENTRY: main + get arguments...