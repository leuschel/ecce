:- module(listio, [file_list/2],[assertions]).

%% Nota importante: esto aun no funciona.

:- use_module(library(streams)).
:- use_module(engine(internals)).

file_list(File, List) :-
        open(File, read, Stream),
        '$undo_goal'(close_if_needed(Stream)),
        attach_attribute(List, '$listio'(List, Stream)).

:- multifile 
        verify_attribute/2.

verify_attribute('$listio'(L, S), Cs) :-
        get_undo(S, C),
        unify_listio(Cs, C, L, S).

unify_listio([], -1, L, _) :-
        detach_attribute(L),
        L = [].
unify_listio([C|Cs], C, L, S) :-
        detach_attribute(L),
        L = [C|L_],
        attach_attribute(L_, '$listio'(L_, S)),
        L_ = Cs.

:- data pending_char/2.

get_undo(S, C) :-
        S = '$stream'(_,N),
        get_a_char(N, S, Ch),
        '$undo_goal'(asserta_fact(pending_char(N,Ch))),
        C = Ch.

get_a_char( N,_S, C) :-
        retract_fact(pending_char(N,C)), !.
get_a_char(_N, S, C) :-
        get_code(S, C),
        close_if_eof(C, S).

close_if_eof(-1, S) :- !, close(S).
close_if_eof(_, _).

close_if_needed(S) :-
        current_stream(_, _, S) -> close(S) ; true.
