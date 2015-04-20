:- module(remove2, [rr/3]).
:- ensure_loaded(local).

rr(X, Y, trace(1, F1, F2)):-
    f(X, T, F1),
    f(T, Y, F2).

%-----------------------------------------------------------------

f(X, Y, Trace):-
    non_rigid_norm(X, list_length, 0, SizeX),
    non_rigid_norm(Y, list_length, 0, SizeY),
    f(X, Y, SizeX, SizeY, unbounded, Trace).

f(X, Y, D, E, Mode, Trace):-
    ((D >= 0 ; E >= 0) ->
            f1(X, Y, D, E, Mode, Trace)
     ;
            Mode = unbounded,
            rigid_norm(X, list_length, 0, SizeX, Mutex),
            rigid_norm(Y, list_length, 0, SizeY, Mutex),
            bounded_level_mapping_one('=', SizeX, Level, Mutex),
            bounded_level_mapping_one('=', SizeY, Level, Mutex),
            freeze(Level, f1(X, Y, Level, Level, bounded, Trace))
    ).

f1([], [], _, _, _, 2).
f1([A|T], Y, D, E, Mode, trace(3, H)):-
    h(A, T, Y, D, E, Mode, H).


h(A, [], [A], _, _, _, 4).
h(A, [B|S], Y, D, E, Mode, trace(5, G)):-
    g(A, B, [B|S], S, Y, D, E, Mode, G).


g(A, A, _, S, [A|Y], D, E, Mode, trace(6, F)):-
    D1 is D - 2,
    E1 is E - 1,
    f(S, Y, D1, E1, Mode, F).
g(A, B, T, _, [A|Y], D, E, Mode, trace(7, Neq, F)):-
    D1 is D - 1,
    E1 is E - 1,
    local:'\=='(A, B, Neq),
    f(T, Y, D1, E1, Mode, F).


%-----------------------------------------------------------------
%
% Norms and level mappings

non_rigid_norm(V, _, D, D):-
    var(V), !.
non_rigid_norm([], list_length, D, D).
non_rigid_norm([_|Y], list_length, D_in, D_out):-
    D_in1 is D_in + 1,
    non_rigid_norm(Y, list_length, D_in1, D_out).

:- block rigid_norm(-, ?, ?, ?, -), rigid_norm(?, ?, -, ?, -).

rigid_norm(_, _, _, _, Mutex):-
    ground(Mutex), !.
rigid_norm([], list_length, D, D, _).
rigid_norm([_|Y], list_length, D_in, D_out, Mutex):-
    D_in1 is D_in + 1,
    rigid_norm(Y, list_length, D_in1, D_out, Mutex).

:- block bounded_level_mapping_one(?, -, ?, -).

bounded_level_mapping_one(_, _, _, Mutex):-
    ground(Mutex), !.
bounded_level_mapping_one('=', Level, Level, 0).
