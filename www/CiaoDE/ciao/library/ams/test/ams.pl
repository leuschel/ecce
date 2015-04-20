% package for AMS configuration files

:- use_module(library('ams/doc/config')). % does not work!
:- use_module(library('compiler/c_itf'),[opt_suffix/2]).
%:- use_module(library(compiler),[use_module/1]).
:- set_prolog_flag(unknown,fail).
:- initialization('$precompile').

'$precompile':-
%	use_module(library('ams/doc/config')),
	current_prolog_flag(argv,Args),
%% 	separate_ciaoc_args(Args,MainArgs,PrecompArgs),
%% 	( main(MainArgs) -> true ; main ),
	precompile(PrecompArgs),
	opt_suffix(_,'_client').
