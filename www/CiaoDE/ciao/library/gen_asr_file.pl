:- module(_, gaf/1, []).

:- use_module(library('assertions/assrt_lib'),
	[
            get_code_and_related_assertions/5,
            cleanup_code_and_related_assertions/0
        ]).


gaf(F):-
	cleanup_code_and_related_assertions, 
        absolute_file_name(F, FileName),
        get_code_and_related_assertions(FileName,_M,_Base,_Suffix,_Dir).
