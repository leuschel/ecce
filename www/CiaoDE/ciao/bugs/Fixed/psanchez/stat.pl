
:- module(stat, [main/0]).

:- use_module(library(prolog_sys)).
:- use_module(aries).

main:-
        statistics(runtime, _), (w(L,C,G,N,"doctores", []), fail; true), statistics(runtime, Ts).

Ts = [173040,1820] ? 

yes
?- statistics(runtime, _), (w(L,C,G,N,"doctores", []), fail; true), statistics(runtime, Ts).

Ts = [174870,1830] ? 

yes
?- statistics(runtime, _), (w(L,C,G,N,"doctores", []), fail; true), statistics(runtime, Ts).

Ts = [176690,1819] ? 

yes
