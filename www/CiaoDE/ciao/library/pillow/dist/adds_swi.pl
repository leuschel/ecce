
display_list([M|Ms]) :- !,
        display(M),
        display_list(Ms).
display_list([]) :- !.
display_list(M) :-
        display(M).

warning(Mess) :-
        current_output(OldOut),
        set_output(user_error),
        display_list(['WARNING: '|Mess]),
        set_output(OldOut).

write_string(Stream, S) :-
        current_output(OldOut),
        set_output(Stream),
        write_string(S),
        set_output(OldOut).

write_string([]).
write_string([C|Cs]) :- put_code(C), write_string(Cs).

get_line(Line) :-
        get_code(C),
        get_line_after(C, Cs),
        Line = Cs.

get_line_after(-1,[]) :- !. % EOF
get_line_after(10,[]) :- !. % Newline
get_line_after(13, R) :- !, % Return, delete if at end of line
        get_code(C),
        get_line_after(C, Cs),
        ( Cs = [] ->
              R = []
        ; R = [13|Cs]
        ).
get_line_after(C, [C|Cs]) :-
        get_code(C1),
        get_line_after(C1, Cs).

whitespace --> whitespace_char, whitespace0.

whitespace0 --> whitespace_char, whitespace0.
whitespace0 --> [].

whitespace_char --> [10]. % newline
whitespace_char --> [13]. % return
whitespace_char --> [32]. % space
whitespace_char --> [9].  % tab

string([]) --> "".
string([C|Cs]) -->
        [C],
        string(Cs).

getenvstr(Var,ValStr) :-
        getenv(Var,Val),
        atom_codes(Val,ValStr).

list_lookup(List, Functor, Key, Value) :-
	var(List), !,
        functor(Pair, Functor, 2),
        arg(1, Pair, Key),
        arg(2, Pair, Value),
	List=[Pair|_].
list_lookup([Pair|_], Functor, Key, Value) :-
        functor(Pair, Functor, 2),
        arg(1, Pair, Key0),
	Key0==Key, !,
        arg(2, Pair, Value).
list_lookup([_|List], Functor, Key, Value) :-
	list_lookup(List, Functor, Key, Value).

% ============================================================================
% http_transaction(+Host, +Port, +Request, +Timeout, -Response)
% type http_transaction(atom, integer, string, integer, string).
%
% Notes: Sends an HTTP Request to an HTTP server and returns the
%         resultant message in Response
% 
%        Fails on timeout (Timeout in seconds) 
% ============================================================================
% 
%%%%% this file allows fetch_url/3 for SWI-Prolog
%%%%% based on pillow_{ciao|sicstus21|sicstus3}
%%%%% changed by Frank Schilder (schilder@informatik.uni-hamburg.de)
%%%%% Date: 29th May 2000
%
:- use_module(library(socket)).

%%% needs the clib package:
%%% available from http://swi.psy.uva.nl/projects/SWI-Prolog/packages/clib/
% ============================================================================
%
%The clib package consists of various basically independant libraries
%dealing with some commonly encountered low-level operations that must
%be defined or are easier defined in C.
%
%library(unix)
%    Deals with Unix process management (fork(), pipe(), exec(), etc.). 
%
%library(socket)
%    Deals with handling tcp sockets (both server and client). This
%library also runs on MS-Windows, though the server functionality is
%    incomplete on this platform.  
%
%library(cgi)
%    Deals with fetching form-data when using SWI-Prolog as CGI
%application. Supports both GET and POST methods. Also supports
%    multipart/form-data MIME encoded form-data. 
%
% ============================================================================

http_transaction(Host, Port, Request, Timeout, Response) :-
        tcp_socket(Socket),
        tcp_connect(Socket, Host:Port),
        tcp_open_socket(Socket, ReadFd, WriteFd),
        write_string(WriteFd, Request),
        flush_output(WriteFd),
        wait_for_input([WriteFd],R,Timeout),
	R \== [],  % Fail if timeout
	read_to_close(ReadFd, Response),
        close(WriteFd),
	close(ReadFd).

read_to_close(S, L) :-
        get_code(S, C),
        read_to_close1(C, S, L).

read_to_close1(-1, _, []) :- !.
read_to_close1(C, S, [C|L]) :-
        get_code(S, C1),
        read_to_close1(C1, S, L).
