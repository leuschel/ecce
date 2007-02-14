:- module(modes,[find_applicable_mode_declaration/7,other_mode_declaration/2,get_potential_io_args/3]).

/* modes.pro */
:- set_prolog_flag(multi_arity_warnings,off).
:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).

:- use_module(bimtools).
% [claus_database,typechecker]
:- use_package( .('ecce_no_rt') ).

:- include( multi_meta ).

find_applicable_mode_declaration(H,InArgs,AnyArgs,OutArgs,DCall,DI,DO) :-
	mode_declaration(Nr,H,InArgs,AnyArgs,OutArgs),
	not(other_mode_declaration(Nr,H)),
	mode_declaration(Nr,DCall,DI,_,DO).
find_applicable_mode_declaration(H,InArgs,AnyArgs,OutArgs,DCall,DI,DO) :-
	mode_declaration(Nr,H,InArgs,AnyArgs,OutArgs),
	ground(InArgs),
	mode_declaration(Nr,DCall,DI,_,DO).
find_applicable_mode_declaration('is'(X,Y),[Y],[],[X],'is'(V,W),[W],[V]).
find_applicable_mode_declaration('=..'(X,Y),[X],[],[Y],'=..'(V,W),[V],[W]).
find_applicable_mode_declaration('=..'(X,Y),[Y],[],[X],'=..'(V,W),[W],[V]).
find_applicable_mode_declaration(arg(N,X,A),[N,X],[],[A],arg(M,V,B),[M,V],[B]).
find_applicable_mode_declaration(functor(C,N,F),[C],[],[N,F],
				 functor(C2,N2,F2),[C2],[N2,F2]).

other_mode_declaration(Nr,H) :-
	mode_declaration(Nr2,H,_InArgs,_AnyArgs,_OutArgs),
	not(Nr2=Nr).


pre_condition(get_potential_io_args(Call,_InArgs,_OutArgs)) :-
	term_is_of_type(Call,nonvar).
post_condition(get_potential_io_args(_Call,InArgs,OutArgs)) :-
	term_is_of_type(InArgs,nonvar),
	term_is_of_type(OutArgs,nonvar).
	
get_potential_io_args(Call,InArgs,OutArgs) :-
	(find_applicable_mode_declaration(Call,InArgs,_AnyArgs,OutArgs,
					 _DCall,_DI,_DO)
	-> (true)
	;  ( /* no info, assume all args can be in or out */
	    InArgs = [Call],
	    OutArgs = [Call])
	).
