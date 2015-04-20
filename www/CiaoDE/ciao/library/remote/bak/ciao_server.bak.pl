:- module(ciao_server, [serve/0, serve_random_port/0], [assertions]).
:- use_module(library(sockets)).
:- use_module(library('remote/socket_info'), [socket_port/1]).

:- use_module(library('remote/read_and_write')).
:- use_module(library(write)).


:- use_module(library(compiler),[use_module/1]).

:- use_module(library(random), [random/3]).
:- use_module(library(system)).
:- use_module(library(aggregates)).

%% :- use_package(debug).
%% :- use_package(trace).
%% :- use_package(nodebug).
%% :- entry serve/0.

:- data tracing/0.

:- multifile call_in/1.

:- multifile is_boundto/1.
:- concurrent is_boundto/1.

%p(a).
%p(b).

serve:-
	get_socket(Socket),
 	wait_for_commands(Socket).
%	close(Stream).

serve_random_port:-
	get_socket_port(Socket, Port),
	asserta_fact(is_boundto(Port)),
 	wait_for_commands(Socket).
	

wait_for_commands(Socket) :- 
	display(w1(Socket)), nl,
	socket_accept(Socket, Stream),
	display(w2(Stream)), nl,
	remote_read(Stream, Command),
	display(command_to_run(Command)), nl,
	execute(Command, Stream, ToDo),
	display(executed(ToDo)),nl,
 	close(Stream),
 	display('stream not closed'),nl,
       (ToDo = finish ->
	 true
	;
	 wait_for_commands(Socket)).
%%%        socket_shutdown(Stream, read_write). %%

execute(What, Stream, error):-
        var(What), !,
        display(user_error, 'Error in server: empty command received!'),
        nl(user_error),
        close(Stream).

execute(stop, _Stream, finish):- 
	!.  %% In case the application goes crazy

execute(use_module(X), Stream, continue):-
        !,
        %% do_trace(received(use_module(X))),
        use_module(X),
        send_answer_back(use_module(X), Stream).

execute(user_goal(Goal), Stream, continue):- 
	!,
	display(about_calling(Goal)), nl,	
%        (   call(Goal),
       (call_in(Goal),
	 display(kk1), nl,
        AnswerGoal = Goal
	;
	 display(kk2), nl,
	 AnswerGoal = '$0'
	),
	display(a_g(AnswerGoal)), nl,
       display('Answer sending'(AnswerGoal, Stream)), nl,
       send_answer_back(AnswerGoal, Stream),
       display('Answer sent'), nl.

execute('$0', _Stream, continue):- 
	display('$0'), nl,
	!,
	display('Backtracking...'), nl,
	fail.

execute(trace_on, Stream, continue) :-  
        !,
%	 display('Tracing...'), nl,
        asserta_fact(tracing),
%%      set_tracing,
        close(Stream).

execute(trace_off, Stream, continue):-  
        !,
%	 display('Tracing off...'), nl,
        retractall_fact(tracing),
        %% unset_tracing,
        close(Stream).

execute(_Other, Stream, error):-
         display(user_error, 'Error in server: unknown command received!'),
         nl(user_error),
         close(Stream).

send_answer_back(Answer, Stream):-
        remote_write(Stream, Answer),
        do_trace(sent(Answer)).
%        close(Stream).

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
	socket_port(Port),
	asserta_fact(is_boundto(Port)),
	num_of_connections(Queue),
	bind_socket(Port, Queue, Socket),   %% Already in "listen" state
	display(binding(Port, Queue, Socket)), nl.

get_socket_port(Socket, Port):-
	num_of_connections(Queue),
	random(49816, 60000, AuxPort),
	(bind_socket(AuxPort, Queue, Socket) ->
	 Port=AuxPort
	;
	 get_socket_port(Socket, Port)).
	 

get_stream(Stream) :-
	socket_port(Port),
	current_host(Host),
	connect_to_socket(Host, Port, Stream).
