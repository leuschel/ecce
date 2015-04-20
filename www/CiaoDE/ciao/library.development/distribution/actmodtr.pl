actmodtr((:- use_active_module(M, Imports)), LocalDefs) :-
        define_remote_predicates(Imports, M, LocalDefs).

define_remote_predicates([], M, []).
define_remote_predicates([F/A|Ps], M, [Def|Defs]) :-
        define_remote_predicate(F, A, M, Def),
        define_remote_predicates(Ps, M, Defs).

define_remote_predicate(F, A, M, Def) :-
        functor(P, F, A),
        Def = (P :- module_address(M,Add), remote_call(Add, P)).

remote_call(Address, Q) :-
        remote_call_stream(Address,Stream),
        in_stream(Stream, '$rc'(N)),
        N1 is N+1,
        out_stream(Stream, '$rc'(N1)),
        out_stream(Stream, '$rq'(N, Q)),
        in_stream(Stream, '$ra'(N, As)),
%        close(Stream), % ?
        member(Q, As).

:- dynamic '$remote_stream'/2.

remote_call_stream(Address,Stream) :-
        '$remote_stream'(Address,Stream),
        current_stream(_N,socket,Stream), !.
remote_call_stream(Address,Stream) :-
        open_client(Address, Stream),
        assertz_fact('$remote_stream'(Address,Stream)).
