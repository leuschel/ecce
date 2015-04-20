
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
        environ(Var,Val),
        atom_codes(Val,ValStr).

:- use_module(library(lists)).

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

http_transaction(Host, Port, Request, Timeout, Response) :-
        socket('AF_INET', Socket),
        socket_connect(Socket, 'AF_INET'(Host,Port), Stream),
        write_string(Stream, Request),
        flush_output(Stream),
        stream_select([Stream],Timeout:0,R),
        R \== [],  % Fail if timeout
        stream_to_string(Stream,Response).

stream_to_string(Stream, String) :-
        current_input(OldIn),
        set_input(Stream),
        read_to_close(String),
        set_input(OldIn),
        close(Stream).

read_to_close(L) :-
        get_code(C),
        read_to_close1(C, L).

read_to_close1(-1, []) :- !.
read_to_close1(C, [C|L]) :-
        get_code(C1),
        read_to_close1(C1, L).
