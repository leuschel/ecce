:- use_package([]).

:- use_module(library(system)).

popen_read(Command) :- popen(Command,read,S), close(S).
execnoout(Command) :-
        exec(Command,In,Out,Err),
        close(In),
        close(Out),
        close(Err).
