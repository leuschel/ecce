:- module(ciao_client_rt, 
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


@(Goal, Host):-
        translate_goal(Goal, GoalToSend),
%        display('goal translated'), nl,
        socket_port(Port),
        connect_to_socket(Host, Port, Stream), 
%        display('about to write'), nl,
        remote_write(Stream, GoalToSend),
%        display('goal written, about to read'), nl,
        remote_read(Stream, AnswerGoal),
%        display('answer read'), nl,
        close(Stream),
%        display('Stream closed, unifying'), nl,
 %%         displayq(goal(Goal)), nl,
 %%         displayq(goal(AnswerGoal)), nl,
        AnswerGoal = Goal
%        display('Unified!!!!'), nl
 .


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

:- comment(version_maintenance,dir('../../version')).


%% Note that the "assertions" library needs to be included in order
%% to support ":- comment(...,...)." declarations such as these.
%% These version comment(s) can be moved elsewhere in the file.
%% Subsequent version comments will be placed above the last one
%% inserted.

:- comment(version(1*7+5,2000/07/25,18:06*22+'CEST'), "First version
of the Ciao Client utilities started. No backtracking at the moment.
(MCL)").

