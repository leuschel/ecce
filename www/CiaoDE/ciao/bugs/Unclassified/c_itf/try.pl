
:- use_package([]).

:- use_module(library('compiler/c_itf')).
:- use_module(library(ctrlcclean),[ctrlc_clean/1]).
:- use_module(library(errhandle),[error_protect/1]).  

main(Files):-
        cleanup_c_itf_data,
        pass_one(Files,Files).

pass_one([File|Files],Fs):-
        error_protect(ctrlc_clean(
                process_file(File, xrefs, any, 
                             recorda(Fs),
                             false, false, always)
                                 )),
        pass_one(Files,Fs).
pass_one([],_Fs).

always(_BaseName).

recorda(BaseName,Files):-
        defines_module(BaseName,M),
	display(module_read(M)), nl,
	debug(Files).

debug(Files):-
	imports(Module,Imported,F,A),
	member(Module,Files),
	member(Imported,Files),
	display(Module), display(-),
	display(F), display(/),
	display(A), display(-),
	display(Imported), nl,
	fail.
debug(_Files).

imports(Module,M,F,A):-
        imports_pred(ModuleBase,ImpFile,F,A,_DefType,_Meta,_EndFile),
        defines_module(ModuleBase,Module),
        base_name(ImpFile,ImpModuleBase),
        defines_module(ImpModuleBase,M).
