:- op( 1000 , fx , data ).


term_expansion( :-(initialization( X )) , :-(X) ).

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

term_expansion( :-(use_module(X)) , :-(ensure_loaded(X)) ) :-
	X \= library( _ ).



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
