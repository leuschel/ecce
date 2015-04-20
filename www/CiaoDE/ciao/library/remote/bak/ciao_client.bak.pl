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
	display(goal_at(Goal, Address)),nl,
	translate_goal(Goal, GoalToSend),
       display('goal translated'(GoalToSend)), nl,
       display('about to write'(Address, GoalToSend, Goal)), nl,
       runat(Address, GoalToSend, Goal).
%       close(Stream).

runat(Address, GoalToSend, Goal) :- 
	(Address = a(_,_) ->
 	 !
 	;
	 true), 
	display('I am coming this way!'), nl,
	get_socket_port(Address, Host, Port),
	connect_to_socket(Host, Port, Stream), 
	remote_write(Stream, GoalToSend),
	remote_read(Stream, AnswerGoal),
	display(first_result(AnswerGoal)), nl,
	AnswerGoal = Goal, %AnswerGoal \== '$0'),
	close(Stream).

runat(Address, _GoalToSend, Goal) :- 
	repeat,
	   get_socket_port(Address, Host, Port),
	   display(socket_port(Port)), nl,
	   connect_to_socket(Host, Port, Stream), 
	   display(asking_for_new_sol_at(Stream)), nl,
	   remote_write(Stream, '$0'),  % ask for new solution
	   display('asked for more'), nl,
	   remote_read(Stream, Answer),
	   display(new_solution_is(Answer)), nl,
	   close(Stream),
	   ( Answer='$0', !, % no more solutions
	     fail         % continue failing
	   ; Answer=Goal
	   ).

%        display('Unified!!!!'), nl

% runat(Stream, _GoalToSend, Goal) :-
% 	repeat,
% 	   remote_write(Stream, '$0'),  % ask for new solution
% 	   remote_read(Stream,Answer),
% 	   ( Answer='$0', !, % no more solutions
% 	     fail         % continue failing
% 	   ; Answer=Goal
% 	   ).
%        display('Stream closed, unifying'), nl,
%%         displayq(goal(Goal)), nl,
%%         displayq(goal(AnswerGoal)), nl,

%% translate_goal(use_module(Mod), use_module(Mod)):- !.
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
