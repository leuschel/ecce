:- module(ciao_client, 
          [
              (@)/2, 
               server_stop/1,
               server_trace/1,
               server_notrace/1,
	       runat/3
          ], 
          [assertions]).

:- use_module(library('remote/read_and_write')).
:- use_module(library(sockets), [connect_to_socket/3]).
:- use_module(library('remote/socket_info'), [socket_port/1]).

@(Goal, Host):-
        translate_goal(Goal, GoalToSend),
%        display('goal translated'), nl,
%        display('about to write'), nl,
	socket_port(Port),
        connect_to_socket(Host, Port, Stream), 
	runat(Stream, GoalToSend, Goal).
%        close(Stream).

runat(Stream, GoalToSend, Goal) :-
        remote_write(Stream, GoalToSend),
%        display('goal written, about to read'), nl,
	remote_read(Stream, AnswerGoal),
%        display('answer read'), nl,
        AnswerGoal = Goal. %AnswerGoal \== '$0'


runat(Stream, _GoalToSend, Goal) :-
	repeat,
	   remote_write(Stream, '$0'),  % ask for new solution
	   remote_read(Stream, Answer),
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


server_stop(Host):-
        special_command(stop, Host).
server_trace(Host):-
        special_command(trace_on, Host).
server_notrace(Host):-
        special_command(trace_off, Host).



special_command(Command, Host):-
        socket_port(Port),
        connect_to_socket(Host, Port, Stream), 
        remote_write(Stream, Command),
        close(Stream).
