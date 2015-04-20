
display_list([M|Ms]) :- !,
        display(M),
        display_list(Ms).
display_list([]) :- !.
display_list(M) :-
        display(M).

warning(Mess) :-
        get_stream(output, OldOut),
        get_stream(error, ErrSt),
        set_stream(output, ErrSt),
        display_list(['WARNING: '|Mess]),
        set_stream(output, OldOut).

write_string(Stream, S) :-
        get_stream(output, OldOut),
        set_stream(output, Stream),
        write_string(S),
        set_stream(output, OldOut).

write_string([]).
write_string([C|Cs]) :- put(C), write_string(Cs).

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

string([]) --> [].
string([C|Cs]) -->
        [C],
        string(Cs).


getenvstr(Var,ValStr) :-
        getenv(Var,Val),
        string_list(Val,ValStr).

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

:- use_module(library(sockets)).

http_transaction(Host, Port, Request, _Timeout, Response) :-
        socket(internet, stream, s), connect(s, Host/Port),
        write_string(s, Request),
        flush(s),
        stream_to_string(s,Response).

stream_to_string(Stream, List) :-
        get(Stream,C),
        read_to_close(C, Stream, List),
        close(Stream).

read_to_close(-1, _, []) :- !.
read_to_close(C, Stream, [C|L]) :-
        get(Stream, C1),
        read_to_close(C1, Stream, L).

/* ISO/Ciao compatibility */

get_code(C) :- get(C).

catch(Goal,Error,Handler) :-
        block(Goal, Error, Handler).

atom_codes(Atom, List) :-
        atom(Atom), !,
        atom_string(Atom,String),
        string_list(String,List).
atom_codes(Atom, List) :-
        string_list(String,List),
        atom_string(Atom,String).

number_codes(Number, List) :-
        number(Number), !,
        number_string(Number,String),
        string_list(String,List).
number_codes(Number, List) :-
        string_list(String,List),
        number_string(Number,String).

atom_concat(A, B, AB) :- concat_atoms(A, B, AB).

flush_output :- flush(output).

select(E, [E|Es], Es).
select(E, [X|Es], [X|L]) :- select(E, Es, L).
