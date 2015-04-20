:- module(server,[main/1],[]).
:- use_module(library('ams/amsrt'),[ams_service/1]).
:- use_module(library(lists),[append/3]).
:- use_module(library(filenames)).
:- use_module(library(read)).
:- use_module(library(sockets)). 
:- use_module(library('ams/socket_serve'),[serve_socket_mono/3]).
:- use_module(library(system)). 

:- data service/5.

debug(M):- display(user_error,M), nl(user_error).

connection_length(private,5).
connection_length(shared,10).
connection_length(public,100).

main([Host]):-
	ams_service(Port),
	bind_socket(Port,100,Socket),
	server:debug('Connected to port:'), server:debug(Port),
	serve_socket_mono(Socket,process(Host),true).

process(startup(Aid,Mod,Mid,Mode),_H,Stream):- !,
	server:debug(starting_up(Aid,Mid,Mode)),
	startup(Aid,Mod,Mid,Mode,Port),
	return(Port,Stream,_).
process(deamon(Mid,Pid,Port),_H,Stream):- !,
	server:debug(deamon_up(Mid,Pid,Port)),
	deamon_up(Mid,Pid,Port,Answer),
	return(Answer,Stream,_).
process(shutdown(Port),Host,Stream):- !,
	server:debug(shutting_down(Port)),
	shutdown(Port,Host,Answer),
	return(Answer,Stream,_).
process(Request,_H,_Stream):-
	error(['Request not recognized: ',Request]).

startup(Aid,_Mod,Mid,Mode,Port):-
	partner(Mode,Aid,Mid,Port), !,
	server:debug(partner(Mid,Port)),
	up(Mid,Mode,Aid,Port).
startup(_A,Mod0,_Mid,_M,a(Port)):-  % active module
	( Mod0=class(Mod) -> true ; Mod0=Mod ), % for classes
	atom_codes(Mod,MOD),
	service(MOD,public,_,Port,_), !,  % check that it is still running!
	server:debug(actmod(Mod,Port)),
	up(MOD,public,_Any,Port).
startup(Aid,Mod,Mid,Mode,Result):-
	server:debug(booting_server),
	exec('bash -c "strapper 2> strapper_log"',StdIn,StdOut),
	% boot(StdIn,StdOut,Aid,Mod,Mid,Mode,Result), !.
	connection_length(Mode,Length),
	current_output(OldOut),
	set_output(StdIn),
	display_term(Mod),
	display_term(Mid),
	display_term(Length),
	flush_output,
	set_output(OldOut),
	close(StdIn),
	server:debug(awaiting_answer),
	read(StdOut,Answer),
	server:debug(answer(Answer)),
	close(StdOut),
	!,
	( Answer = end_of_file
	-> Result = -3
	 ; ( number(Answer)
	   -> Result = Answer,
	      ( Answer @> 0
	      -> asserta_fact(service(Mid,Mode,Aid,Answer,1))
	       ; true
	      )
	    ; error(Answer),
	      Result = -3
	   )
	).
startup(_A,_Mo,_Mi,_Mode,-1).

up(Mid,Mode,Aid,Port):-
	retract_fact(service(Mid,Mode,Aid,Port,N)),
	N1 is N+1,
	asserta_fact(service(Mid,Mode,Aid,Port,N1)).

deamon_up(Mid,_Pid,_Port,Answer):-
	service(Mid,_Mode,_Aid,_P,_N), !,
	Answer = -1.
deamon_up(Mid,_Pid,Port,0):-
	asserta_fact(service(Mid,public,_Aid,Port,0)).

shutdown(a(Port),_H,Answer):-  % active module
	retract_fact(service(Mid,Mode,Aid,Port,N)),
	N > 0, !,
	N1 is N-1,
	Answer = ok,
	asserta_fact(service(Mid,Mode,Aid,Port,N1)).
shutdown(Port,Host,Answer):-
	retract_fact(service(Mid,Mode,Aid,Port,N)), !,
	N1 is N-1,
	down(N1,Mid,Mode,Aid,Host,Port,Answer).
shutdown(_P,_H,-6).

down(0,_Mi,_Mo,_A,Host,Port,Answer):-
	connect_to_socket(Host,Port,Stream), !,
	return(1,Stream,Result),
	( Result = 0
	-> Answer = ok
	 ; Answer = -5
	),
	close(Stream).
down(0,_Mi,_Mo,_A,_H,_P,-5).
down(N,Mid,Mode,Aid,_H,Port,ok):-
	N > 0,
	asserta_fact(service(Mid,Mode,Aid,Port,N)).

return(Answer,Stream,Result):-
	current_output(OldOut),
	( set_output(Stream)
	-> display_term(Answer),
	   flush_output,
	   set_output(OldOut),
	   Result = 0
	 ; error('Connection lost!'),
	   Result = -1
	),
	server:debug(return(Answer)).

partner(shared,Aid,Mid,Port):-
	service(Mid,shared,Aid,Port,_).
partner(public,_A,Mid,Port):-
	service(Mid,public,_,Port,_).
