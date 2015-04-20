:- module(ciao_server_rt, [serve/0], [assertions]).

:- use_module(user, [call_in_user/1]).
:- use_module(library(sockets)).
:- use_module(library('remote/socket_info'), [socket_port/1]).

:- use_module(library('remote/read_and_write')).


:- use_module(library(compiler),[use_module/1]).


%% :- use_package(debug).
%% :- use_package(trace).
%% :- use_package(nodebug).
%% :- entry serve/0.

:- data tracing/0.

serve:-
        get_socket(Socket),
        wait_for_commands(Socket).

wait_for_commands(Socket):-
        repeat,
        socket_accept(Socket, Stream),
        remote_read(Stream, Command),
        execute(Command, Stream, ToDo),
        ToDo = finish.
%%        socket_shutdown(Socket, How).


execute(What, Stream, error):-
        var(What), !,
        display(user_error, 'Error in server: empty command received!'),
        nl(user_error),
        close(Stream).

execute(stop, Stream, finish):-   %% In case the application goes crazy
        !,
        close(Stream). 

execute(use_module(X), Stream, continue):-
        !,
        %% do_trace(received(use_module(X))),
        use_module(X),
        send_answer_back(use_module(X), Stream).

execute(user_goal(Goal), Stream, continue):-
        !,
        do_trace(received(Goal)),
        (
            call_in_user(Goal) ->
            AnswerGoal = Goal
        ;
            AnswerGoal = fail
        ),
        send_answer_back(AnswerGoal, Stream).

execute(trace_on, Stream, continue) :-  
        !,
        asserta_fact(tracing),
%%      set_tracing,
        close(Stream).

execute(trace_off, Stream, continue):-  
        !,
        retractall_fact(tracing),
        %% unset_tracing,
        close(Stream).

execute(_Other, Stream, error):-
        display(user_error, 'Error in server: unknown command received!'),
        nl(user_error),
        close(Stream).

send_answer_back(Answer, Stream):-
        remote_write(Stream, Answer),
        do_trace(sent(Answer)),
        close(Stream).

do_trace(What):-
        current_fact(tracing) ->
        displayq(What),
        nl
 ;
        true.

 %% do_trace(What):-
 %%         (
 %%             current_fact(tracing) ->
 %%             basic_terms_read(Read),
 %%             basic_terms_written(Written),
 %%             nl,
 %%             display(Read),
 %%             display(' basic terms read so far'),
 %%             nl,
 %%             display(Written),
 %%             display(' basic terms written so far'),
 %%             nl,
 %%             displayq(What),
 %%             nl,
 %%             nl,
 %%             flush_output
 %%         ;
 %%             true
 %%         ).

num_of_connections(100).

get_socket(Socket):-
        num_of_connections(Queue),
        socket_port(Port),
        bind_socket(Port, Queue, Socket).  %% Already in "listen" state
