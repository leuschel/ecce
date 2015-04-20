% ----------------------------------------------------------------------
%
% Linda communication for Distributed CIAO, for SICStus 2.1
% Based on library file linda/client.pl of SICStus 2.1
% (C) UPM-CLIP 1995
%
% ----------------------------------------------------------------------

 %% :- module(linda,[
 %% 	linda_client/1,
 %% 	close_client/0,
 %% 	in/1,
 %% 	in/2,
 %% 	in_noblock/1,
 %% 	out/1,
 %% 	rd/1,
 %% 	rd/2,
 %% 	rd_noblock/1,
 %%         rd_findall/3,
 %% 	linda_timeout/2,
 %%         halt_server/0,
 %%         open_client/2,
 %%         in_stream/2,
 %%         out_stream/2]).

:- ensure_loaded(library(lists)).

%% :- dynamic linda_stream/2.
:- set(time_out(20000)).

protocol(0'p).      /* 0'f = special format, 0'p = write_canonical */

linda_client(NewAddress) :-
        setting(linda_stream(Stream,OldAddress)),
	current_stream(_N,socket,Stream), !,
	(   NewAddress = OldAddress ->
	    true    % Seems ok, just ignore it
	;   format(user_error,
	           '{ERROR: linda_client/1: Already client to ~w}~n',
		   [OldAddress])
	).
linda_client(Address) :-
	unsetall(linda_stream(_,_)),%Might be a try to reestablish a broken conn.
	open_client(Address,Stream),
	set(linda_stream(Stream,Address)),
	ping(Answer),
	ping_answer_ok(Answer).

ping_answer_ok(pong) :- !.
ping_answer_ok(A) :-
	format(user_error,
	       '{ERROR: linda_client/1: strange answer from server: ~q}~n',
	       [A]).	

open_client(Host-Port, Stream):- connect_to_socket(Host, Port, Stream).

%-----------------------------------------------------------------------------

close_client :-
        unset(linda_stream(Stream,_)),
        close(Stream).

ping(Answer) :-	
	to_linda(0'p, ping),
	time_out_select,
	from_linda(Answer).

out(T) :- to_linda(0'o, T).

in(T) :-
        to_linda(0'i, T),
	from_linda(T).

in(Tuples,Tuple) :-
	Tuples = [_|_],
        to_linda(0'I, Tuples),
	from_linda(Tuple), !,
	member(Tuple,Tuples).  % for unification of Tuples with answer

in_noblock(T) :-
        to_linda(0'j, T),
	time_out_select,
	from_linda(0's, T).
	
rd_noblock(T) :-
	to_linda(0's, T),
	time_out_select,
	from_linda(0's, T).

rd(T) :-
	to_linda(0'r, T),
	from_linda(T).

rd(Tuples,Tuple) :-
	Tuples = [_|_],
        to_linda(0'R, Tuples),
	from_linda(Tuple), !,
	member(Tuple,Tuples).  % for unification of Tuples with answer

rd_findall(Template,Tuple,Bag) :- 
	to_linda(0'f, f(Template,Tuple,Bag)),
	time_out_select,
	from_linda(Bag).

linda_trace(OnOff) :-
	(var(OnOff) ; OnOff=on ; OnOff=off), !,
	to_linda(0't, OnOff),
	time_out_select,
	from_linda(OnOff).

linda_call(Goal) :-
	to_linda(0'c, Goal),
	from_linda(0's, Goal).

halt_server :- % makes also close_client
	setting(linda_stream(Stream,Address)),
	(   current_stream(_N,socket,Stream) ->
	    current_output(CU),
	    set_output(Stream),
	    protocol(P),
            put(P),
            put(0'h),
	    flush_output(Stream),
	    set_output(CU),
	    current_input(CI),
	    set_input(Stream),
	    get0(_),
	    set_input(CI),
            close_client
        ;   linda_client(Address) -> % Connection broken; could reestablish it
	    halt_server
        ;   format(user_error, 
	           '{ERROR: the connection with linda has been shut down, can''t reopen it!}',
		   []),
	    fail
        ).

in_stream(Stream,T) :-
        to_linda_stream(Stream, 0'i, T),
	from_linda_stream(Stream, T).

out_stream(Stream, T) :- to_linda_stream(Stream, 0'o, T).
        
%-----------------------------------------------------------------------------
to_linda(Code, Item) :-
	setting(linda_stream(Stream,Address)),
	(   current_stream(_N,socket,Stream) ->
	    current_output(CU),
	    set_output(Stream),
	    protocol(P),
	    write_out(P,Code,Item),
	    flush_output(Stream),
	    set_output(CU)
        ;   linda_client(Address) -> % Connection broken; could reestablish it
	    to_linda(Code, Item)
        ;   format(user_error, 
	           '{ERROR: the connection with linda has been shut down, can''t reopen it!}',
		   []),
	    fail
        ).

to_linda_stream(Stream, Code, Item) :-
	(   current_stream(_N,socket,Stream) ->
	    current_output(CU),
	    set_output(Stream),
	    protocol(P),
	    write_out(P,Code,Item),
	    flush_output(Stream),
	    set_output(CU)
        ;   format(user_error, 
	           '{ERROR: the connection has been shut down!}',
		   []),
	    fail
        ).

%-----------------------------------------------------------------------------
write_out(0'p,Code,Item) :- 
	put(0'p),
	put(Code), 
	write_canonical(Item),write('.'),nl.
write_out(0'f,Code,Item) :-
	put(0'f), 
	put(Code), 
	fast_write(Item).

read_in(0'p,Item) :- read(Item).
read_in(0'f,Item) :- fast_read(Item).
%-----------------------------------------------------------------------------
from_linda(Item) :-
	setting(linda_stream(Stream,_Address)),
	current_input(S),
	set_input(Stream),
	protocol(P),
	read_in(P,Item),
	set_input(S).

from_linda_stream(Stream, Item) :-
	current_input(S),
	set_input(Stream),
	protocol(P),
	read_in(P,Item),
	set_input(S).

from_linda(Code, Item) :-
	setting(linda_stream(Stream,_Address)),
	current_input(S),
	set_input(Stream),
	get0(Cl),
	(   Cl=Code ->
	    protocol(P),
	    read_in(P,Item),
	    set_input(S)
	;   set_input(S),
	    fail
	).

%-----------------------------------------------------------------------------
time_out_select :- 
        setting(time_out(TimeOut)), 
        time_out_select1(TimeOut).

time_out_select1(off) :- !.
time_out_select1(TimeOut) :-
        number(TimeOut),
	setting(linda_stream(Stream,_Address)),
	select_socket(_,_,TimeOut,[Stream],[_|_]), !.
time_out_select1(_) :-
	format(user_error,'{ERROR: wait for linda timed out}~n',[]),
	fail.
%-----------------------------------------------------------------------------

linda_timeout(Old, New) :-
        (   var(New) ->
            Old = New
        ;   check_timeout_arg(New),
            unset(time_out(Old)),
            set(time_out(New))
        ).

check_timeout_arg(Msecs) :- number(Msecs), Msecs >= 0.
check_timeout_arg(off).
