:- module(client, [main/1]).

%% This is to understand the remote calls
:- use_package(remote).

%% This is to understand locally the concurrency-related calls
:- use_package(hlc).


:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(aggregates)).
:- use_module(library(sockets), [hostname_address/2]).


main([]):- fail.
main([Host]):-
%        server_trace(Host),
        display('Integers'), nl,
        integers(100, Host),
        integers(500, Host),
        integers(1000, Host),
        display('Variables'), nl,
        variables(100, Host),
        variables(500, Host),
        variables(1000, Host),
        server_stop(Host).



integers(N, Host):-
        list_of_integers(N, List),
        send_it(N, List, Host).

variables(N, Host):-
        length(List, N),
        send_it(N, List, Host).

send_it(N, List, Host):-
        send_thing(N, List, Host), fail.
send_it(N, List, Host):-
        send_thing(N, List, Host), fail.
send_it(N, List, Host):-
        send_thing(N, List, Host), fail.
send_it(N, List, Host):-
        send_thing(N, List, Host), fail.
send_it(N, List, Host):-
        send_thing(N, List, Host), fail.
send_it(N, List, Host):-
        send_thing(N, List, Host).
send_it(_N, _List, _Host).


list_of_integers(0, []).
list_of_integers(N, [Number|Ns]):-
        N > 0,
        Number is N + 255,
        N1 is N - 1,
        list_of_integers(N1, Ns).

send_thing(Size, Thing, Host):-
%        format("In send_thing/3 with size ~d~n", [Size]),
        statistics(walltime, _),
        received(Thing, Result) @ Host,
        statistics(walltime, [_, Time]),
%        format("In send_thing/3 after receiving~n", []),
        (
            Result = ok ->
            format("Time for size ~d: ~d~n", [Size, Time])
        ;
            format("Error acknowledging result of size ~d~n", [Size])
        ).
        

%received(_, ok).
