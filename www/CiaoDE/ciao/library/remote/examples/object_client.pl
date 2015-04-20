% :- module(object_client, [main/1]).

:- use_package(objects).
:- use_package(remote).
:- use_module(library(sockets), [hostname_address/2]).
:- use_module(library(format)).

:- include(library(hlc)).


main([]):-
        format(
"Usage: client <host_where_the_server_is_running>.
 The client performs several remote calls.  The companion program,
 server, must be running in some machine reachable through the net.
 The client will issue a command to the server in order to put it in
 \"trace mode\" so that a trace of the internal commands will be made.
~n", []).

main([Host]):-
        (hostname_address(Host, _Address) ->
	 step1(Host)
%	 step2(Host),
%	 step3(Host),
%	 step4(Host, 0),
%	 step5(Host)
	
        ;
         format("Please check the host name you gave (~w) is correct.                           Please make sure it is reachable as well (ping ~w).~n",
	         [Host, Host])
	).

%% server_host(localhost).


step1(Host) :-
	format("Creating an instance of class \"object_server\" in host (~w)~n", [Host]),
	Server new stack @ Host,
	Server:top(X) @ Host,
%	findall(X, p(X), L) @ Host,
%	server_stop(Host).
	format("Top element in instance is (~w)~n", [X]).
%	format("Server at ~w stopped~n", [Host]).

step2(Host) :-
	p(X) @ Host &&> Hp,
	X = b,
	q(Y) @ Host &&> Hq,
	Y = bb,
	Hp <&&,
	Hq <&&,
%	display(p(X)),
	format("element p(~w) exists in host (~w)~n", [X, Host]),
	format("element q(~w) exists in host (~w)~n", [Y, Host]).

step3(Host) :-
	p(X) @ Host,
	format("element is (~w)~n", [X]),
	fail.
step3(_).

step4(Host, N) :-
	N < 1000, !, 
	r @ Host,
	N1 is N + 1,
	step4(Host, N1).
step4(_,N) :- 	format("Iteration number: ~w~n", [N]).
	
step5(Host) :-
	p(X) @ Host,
	X=b,
	format("Client 1: element is (~w)~n", [X]).
