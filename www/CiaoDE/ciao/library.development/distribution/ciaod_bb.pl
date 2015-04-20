% ----------------------------------------------------------------------
%
% Blackboard for Distributed CIAO
% Based on library file linda/server.pl of SICStus 2.1
% (C) UPM-CLIP 1997
%
% ----------------------------------------------------------------------

:- use_module(library(read)).
%:- use_module(library(fastrw), [fast_read/1, fast_write/1]).
:- use_module(library(lists), [delete/3, member/2]).
:- use_module(library(queues)).
:- use_module(library('sockets/sockets')).
:- use_module(library(aggregates), [findall/3]).

% Tq = Th-Tt : queue of tuples
% Wq = Wh-Wt : queue of waiting streams

main :- prolog_flag(argv, Args),
        bootargs(Args).

bootargs([Port]) :- !,
	bind_socket(Port, 5, Socket),
        q_empty(Tq),
        q_empty(Wq),
	server1(Socket,[],Tq,Wq).
bootargs([]) :- !,
	bind_socket(Port, 5, Socket),
        display(Port), put_code(0'.), nl,
        flush_output,
        q_empty(Tq),
        q_empty(Wq),
	server1(Socket,[],Tq,Wq).
bootargs(_) :-
        inform_user([
          'Bad number of arguments: either none or port number']),
 	halt(-1).

server1(Socket, Streams0, Tq, Wq) :-
	wait_for_arrival(Socket,Streams0,ReadableStreams,Streams1),
	server2(ReadableStreams,Streams1,Streams2,Tq,Tq1,Wq,Wq1),
	server1(Socket,Streams2,Tq1,Wq1).

/* Waits for either one or all of the following:
	1) A connection is done to 'Socket'
        2) It is possible to read from a stream
*/
wait_for_arrival(Socket, Streams0, ReadableStreams, Streams) :-
	select_socket(Socket, NewStream, off, Streams0, ReadableStreams),
	new_stream_in_list(NewStream, Streams0, Streams).

new_stream_in_list(NewStream, Streams, Streams) :- 
	var(NewStream), !.
new_stream_in_list(NewStream, Streams0, [NewStream|Streams0]).
% :-
%	inform_user(['---------- Opened ',NewStream]).

server2([],S,S,Tq,Tq,Wq,Wq).
server2([InStream|SSs],SS0,SS,Tq,Tq2,Wq,Wq2) :-
	server_one_stream(InStream,SS0,SS1,Tq,Tq1,Wq,Wq1),
	server2(SSs,SS1,SS,Tq1,Tq2,Wq1,Wq2).

server_one_stream(InStream,SS0,SS1,Tq,Tq1,Wq,Wq1) :-
	set_input(InStream),
	set_output(InStream),
	get_code(Protocol),
	server_one_stream(Protocol,InStream,SS0,SS1,Tq,Tq1,Wq,Wq1).

server_one_stream(-1,S,SS0,SS,Tq,Tq,Wq,Wq) :- !,
        % end of file, that is, a broken connection
	set_input(user_input),
	set_output(user_output),
	close(S), /* this failed with a segmentation violation */
%	inform_user(['---------- Closed ',S]),
	delete(SS0,S,SS).
server_one_stream(Protocol,_Stream,SS,SS,Tq,Tq1,Wq,Wq1) :-
	get_code(Request),
%	trace_linda(before,Request,Ti),
	perform_request(Request,Protocol,Tq,Tq1,Wq,Wq1).
%	trace_linda(after,_,Ti).

%----------------------------------------
perform_request(0's,Protocol,Tq,Tq,Wq,Wq) :-  /* rd_noblock */ !,
	get_input(Protocol,Tuple),
        (   q_member(Tuple,Tq) ->
            to_client(Protocol,0's,Tuple)
        ;   to_client(0'f)
        ).
perform_request(0'r,Protocol,Tq,Tq,Wq,Wq1) :-  /* rd */ !,
	get_input(Protocol,Tuple),
        (   q_member(Tuple,Tq) ->
            to_client(Protocol,Tuple),
            Wq1 = Wq
        ;   current_output(Stream),
            q_insert(waiting(Tuple,Protocol,Stream,rd),Wq,Wq1)
        ).
perform_request(0'R,Protocol,Tq,Tq,Wq,Wq1) :- /* rd, conjunction */ !,
	get_input(Protocol,Tlist),
	(   member(T,Tlist), q_member(T,Tq) ->
	    to_client(Protocol,T),
            Wq1 = Wq
	;   current_output(Stream),
	    q_insert(waiting_list(Tlist,Protocol,Stream,rd),Wq, Wq1)
	).
perform_request(0'j,Protocol,Tq,Tq1,Wq,Wq) :-  /* in_noblock */ !,
	get_input(Protocol,Tuple),
        (   q_delete(Tuple,Tq,Tq1) ->
            to_client(Protocol,0's,Tuple)
        ;   to_client(0'f),
            Tq1 = Tq
        ).
perform_request(0'i,Protocol,Tq,Tq1,Wq,Wq1) :-  /* in */ !,
	get_input(Protocol,Tuple),
        (   q_delete(Tuple,Tq,Tq1) ->
            to_client(Protocol,Tuple),
            Wq1 = Wq
        ;   current_output(Stream),
            q_insert(waiting(Tuple,Protocol,Stream,in),Wq,Wq1),
            Tq1 = Tq
        ).
perform_request(0'I,Protocol,Tq,Tq1,Wq,Wq1) :- /* in, conjunction */ !,
	get_input(Protocol,Tlist),
	(   member(T,Tlist), q_delete(T,Tq,Tq1) ->
	    to_client(Protocol,T),
            Wq1 = Wq
	;   current_output(Stream),
	    q_insert(waiting_list(Tlist,Protocol,Stream,in),Wq,Wq1),
            Tq1 = Tq
	).
perform_request(0'o,Protocol,Tq,Tq1,Wh-Wt,Wh1-Wt) :- /* out */ !,
	get_input(Protocol,Tuple),
        wakeup(Tuple,Wh,Wh1,LastOp),
        (   LastOp = rd ->
            q_insert(Tuple,Tq,Tq1)
        ;   Tq1 = Tq
        ).
perform_request(0'p,Protocol,Tq,Tq,Wq,Wq) :-  /* ping */ !,
	get_input(Protocol,In),
	ping_answer(In,Out),
	to_client(Protocol,Out).
perform_request(0'f,Protocol,Tq,Tq,Wq,Wq) :- /* rd_findall */ !,
	get_input(Protocol,f(Template,Tuple,Bag)),
	findall(Template,q_member(Tuple,Tq), Bag),
	to_client(Protocol,Bag).
perform_request(0'h,_Protocol,Tq,Tq,Wq,Wq) :- /* halt */ !,
	to_client(0'h),
        halt.
% perform_request(0'c,Protocol,Tq,Tq,Wq,Wq) :- /* call */ !,
% 	get_input(Protocol,Goal),
%         (  call(Goal) ->
%            to_client(Protocol,0's, Goal)
%         ;  to_client(0'f)
%         ).
% perform_request(0't,Protocol,Tq,Tq,Wq,Wq) :- /* trace */ !,
% 	get_input(Protocol,Q),
% 	trace_answer(Q,Repl),
% 	to_client(Protocol,Repl).
perform_request(R, _Protocol,Tq,Tq,Wq,Wq) :-
	current_input(Stream),
	inform_user(['*** Unknown request from ',Stream,': ',R]).

ping_answer(ping,pong) :- !.
ping_answer(X, illegal(X)).

%-----------------------------------------------------------------------------
wakeup(_,H,H1,LastOp) :-
        var(H), !,
        H1 = H,
        LastOp = rd.
wakeup(Tuple,H,H1,LastOp) :-
        H = [waiting(Tuple,Protocol,Stream,Op)|H_], !,
        to_clientS(Protocol,Stream,Tuple),
        (   Op = in ->
            H1 = H_,
            LastOp = in
        ;   wakeup(Tuple,H_,H1,LastOp)
        ).
wakeup(Tuple,H,H1,LastOp) :-
        H = [waiting_list(Tlist,Protocol,Stream,Op)|H_],
        member(Tuple,Tlist), !,
        to_clientS(Protocol,Stream,Tuple),
        (   Op = in ->
            H1 = H_,
            LastOp = in
        ;   wakeup(Tuple,H_,H1,LastOp)
        ).
wakeup(Tuple,[E|H],[E|H1],LastOp) :-
        wakeup(Tuple,H,H1,LastOp).

%-----------------------------------------------------------------------------
get_input(0'f, Data) :- fast_read(Data).
get_input(0'p, Data) :- read(Data).
	
put_output(0'f, Data):- fast_write(Data).
put_output(0'p, Data):- displayq(Data), put_code(0'.), nl.

%-----------------------------------------------------------------------------
to_client(C) :-
        put_code(C),
        current_output(S),
	flush_output(S).
to_client(Protocol,C,Data) :- 
	put_code(C),
	put_output(Protocol,Data),
	current_output(S),
	flush_output(S).
to_client(Protocol,Data) :- 
	put_output(Protocol,Data),
	current_output(S),
	flush_output(S).

to_clientS(Protocol,Stream,Data) :- 
	current_output(S),
	set_output(Stream),
	put_output(Protocol,Data),
	flush_output(Stream),
	set_output(S).

%-----------------------------------------------------------------------------
% 
% :- data linda_trace/0.
% 
% trace_answer(Q,on) :- var(Q), linda_trace, !.
% trace_answer(Q,off) :- var(Q), !.
% trace_answer(on,on) :- linda_trace, !.
% trace_answer(on,on) :- !, assert(linda_trace).
% trace_answer(_,off) :- retractall(linda_trace).
% 
%-----------------------------------------------------------------------------
% trace_linda(before,R,Ti) :-
% 	linda_trace, !,
% 	put_code(user_output,R),
% 	tab(user_output,2),
% 	flush_output(user_output),
% 	statistics(runtime,[Ti,_]).
% trace_linda(after,_,Ti) :-
% 	linda_trace, !,
% 	statistics(runtime,[Tf,_]),
% 	T is Tf-Ti,
% 	inform_user(['(',T,' ms)']).
% trace_linda(_,_,0).
