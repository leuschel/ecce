

main_sicstus :-
   prolog_flag(argv,ArgV),
   main(ArgV).
   
runtime_entry(start) :- main_sicstus.

%:- main_sicstus.

%:- ecce.


% RUNTIME ENTRY: main + get arguments...

save :- save_program('~/git_root/ecce/ecce_source/ecce.sav').
%:- ensure_consulted('ecce_main.pl').
% MV: I am bypassing ecce_sicstus.pl!!!
% MAL: this does not work !!!!
:- ensure_loaded('ecce_sicstus.pl').