:- module(remove, []).
:- ensure_loaded(local).

rr(X, Y, trace(1, F1, F2)):-
    r(X, T, F1),
    r(T, Y, F2).

%-----------------------------------------------------------------

r(X, Y, Trace):-
    non_rigid_norm(X, list_length, 0, SizeX),
    non_rigid_norm(Y, list_length, 0, SizeY),
    r(X, Y, SizeX, SizeY, unbounded, Trace).

r(X, Y, D, E, Mode, Trace):-
    ((D >= 0 ; E >= 0) ->
            r1(X, Y, D, E, Mode, Trace)
     ;
            Mode = unbounded,
            rigid_norm(X, list_length, 0, SizeX, Mutex),
            rigid_norm(Y, list_length, 0, SizeY, Mutex),
            bounded_level_mapping_one('=', SizeX, Level, Mutex),
            bounded_level_mapping_one('=', SizeY, Level, Mutex),
            freeze(Level, r1(X, Y, Level, Level, bounded, Trace))
    ).

r1([], [], _, _, _, 2).
r1([X], [X], _, _, _, 3).
r1([X, X|T], [X|T1], D, E, Mode, trace(4, R)):-
    D1 is D - 2,
    E1 is E - 1,
    r(T, T1, D1, E1, Mode, R).
r1([X, Y|T], [X|T1], D, E, Mode, trace(5, Neq, R)):-
    D1 is D - 1,
    E1 is E - 1,
    local:'\=='(X, Y, Neq),
    r([Y|T], T1, D1, E1, Mode, R).

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
