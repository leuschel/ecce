%%-----------------------------------------------------------------------
%%  Test tcl_command 
%%-----------------------------------------------------------------------
:- use_module(tcl_command,[],[]).

:- export(test_command/2).

make_asssigned_command(a,4,Command).
test_command(Command, Result) :-
             tcl_new(Interp),
             tcl_eval(Interp, Command, Result).
            % tcl_delete(Interp).
