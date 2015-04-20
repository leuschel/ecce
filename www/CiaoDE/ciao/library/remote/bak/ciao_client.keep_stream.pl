:- module(ciao_client, 
          [
              (@)/2, 
               server_stop/1,
               server_trace/1,
               server_notrace/1
          ], 
          [assertions]).

:- use_module(library('remote/read_and_write')).
:- use_module(library(sockets), [connect_to_socket/3]).
:- use_module(library('remote/socket_info'), [socket_port/1]).


@(Goal, Address):-
	translate_goal(Goal, GoalToSend),
	get_socket_port(Address, Host, Port),
	connect_to_socket(Host, Port, Stream), 
        runat(Stream, GoalToSend, Goal),
        close(Stream).



runat(Stream, GoalToSend, Goal) :- 
	remote_write(Stream, GoalToSend),
	remote_read(Stream, AnswerGoal),
	AnswerGoal = Goal. 


runat(Stream, _GoalToSend, Goal) :- 
	repeat,
	   remote_write(Stream, '$0'),  % ask for new solution
	   remote_read(Stream, Answer),
	   ( Answer='$0', !, % no more solutions
	     fail         % continue failing
	   ; Answer=Goal
	   ).


translate_goal(Goal, user_goal(Goal)).


server_stop(Address):-
        special_command(stop, Address).
server_trace(Address):-
        special_command(trace_on, Address).
server_notrace(Address):-
        special_command(trace_off, Address).



special_command(Command, Address):-
        get_socket_port(Address, Host, Port),
        connect_to_socket(Host, Port, Stream), 
        remote_write(Stream, Command),
        close(Stream).

get_socket_port(a(Host, Port), Host, Port) :- !.
get_socket_port(Host, Host, Port) :-
	socket_port(Port).
