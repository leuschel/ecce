% :- module(client, [main/1]).

%% This is to understand the remote calls
%:- include(library('remote/ciao_client')).  
:- use_package(remote).
%% This is to understand locally the concurrency-related calls
:- include(library(hlc)).


:- use_module(library(format)).
:- use_module(library(aggregates)).
:- use_module(library(sockets), [hostname_address/2]).


main([]):-
        format(
"Usage: client <host_where_the_server_is_running>.
 The client performs several remote calls.  The companion program,
 server, must be running in some machine reachable through the net.
 The client will issue a command to the server in order to put it in
 \"trace mode\" so that a trace of the internal commands will be made.
~n", []).

main([Host]):-
        (
            hostname_address(Host, _Address) ->
            step0(Host),
            step1(Host),
            step2(Host),
            step3(Host),
            step4(Host),
            step5(Host),
            step6(Host)
        ;
            format(
"
Please check the host name you gave (~w) is correct.
Please make sure it is reachable as well (ping ~w).~n
",[Host, Host])
        ).

%% server_host(localhost).

queens_size(20).
small_queens_size(9).

%% @/2 calls remotely.  Note it is a blocking call.


step0(Host) :-
	p(X) @ Host,
	display(X), nl,
	fail.
step0(_).

step1(Host):-
 %%         format("~nPutting the server ~w in trace mode~n", [Host]),
 %%         server_trace(Host),
        queens_size(S),
        format("Sending a blocking queens(~d) goal~n", [S]),
        queens(S, Q) @ Host,       %% This is a blocking call
        format("Queens of size ~w returning ~w~n~n~n", [S,Q]).


 %% We make a remote call in a separate, local thread --- but the server,
 %% which is not threaded itself, would become blocked!  Note that the
 %% answer to p/1 waits for the answer to queens/2 to be received; this
 %% is due to the server executing a query at a time.

step2(Host):-
        queens_size(S),
        format("Sending a locally concurrent queens(~d) goal~n", [S]),
        queens(S, Q) @ Host &&> HandleQ,
        format("Sending a locally concurrent simple query~n", []),
        p(X) @ Host &&> HandleP,
        format("Waiting for answers...~n", []),
        HandleP <&&,
        format("p(X) returned X = ~w~n", [X]),
        HandleQ <&&,
        format("queens returned Q = ~w~n~n~n", [Q]).
        


 %% We can transform a non-threaded server immediately into a threaded one,
 %% without the need of transforming its code (provided it is reentrant).
 %% All we need is to force per-query threads in the server.

step3(Host):-
        queens_size(S),
        format("Sending a remotely concurrent queens(~d) goal~n", [S]),
        queens(S, Q) &&> HandleQ @ Host,
        format("Sending a blocking simple query~n", []),
        p(X) @ Host,   %% We could make a remote thread for this one as well
        format("p(X) returns X = ~w~n", [X]),
        HandleQ <&& @ Host,
        format("queens returns Q = ~w~n~n~n", [Q]).


stepN(Host):-
        queens_size(S),
        format("Sending a remotely concurrent queens(~d) goal~n", [S]),
        queens(S, Q) &&> HandleQ @ Host,
        format("Sending a remote simple query~n", []),
        p(X) &&> HandleP @ Host,  
        HandleQ <&& @ Host,
        format("queens returns Q = ~w~n~n~n", [Q]),
        HandleP <&& @ Host,
        format("p(X) returns X = ~w~n", [X]).


step4(Host):-
        small_queens_size(S),
        format("Sending a remotely concurrent findall of queens(~d) goal~n", [S]),
        findall(Q, queens(S, Q), AllQueens) &&> HandleQ @ Host,
        format("Waiting for answers...~n", []),
        HandleQ <&& @ Host,
        (
            member(Sol, AllQueens),
            format("Solution: ~w~n", [Sol]),
            fail
        ) ; 
        format("~n~n", []).
        



%% Note that we do need

:- use_package(clpq).

%% for the example below, although all the processing is done remotely.  
%% This is so because library(clpq) does not only define the syntax, but 
%% but also the unification of attributed variables.  When place_queens/2
%% returns, Qs has a local value (attributed variables) and a remote
%% value (a list of numbers, i.e., terms).  The attributed variable
%% handling is needed to perform the unification between the terms
%% and the variables.


step5(Host):-
        N = 13,
        format("Placing ~w queens remotely: sending constrains~n", [N]),
        constrain_values(N, N, Qs) @ Host &&> Handle,
        format("Placing ~w queens remotely: asking for a value assignement~n",
              [N]),
	Handle <&&, 
        place_queens(N, Qs) @ Host,
        format("~w queens placed: ~w~n~n", [N, Qs]).


%% Stop the remote server

step6(Host):-
        format("Stopping the server ~w~n~n~n", [Host]),
        server_stop(Host).
