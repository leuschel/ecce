

:- module(match, [match/3]).
:- ensure_loaded(local).


match(Pat, T, trace(1, Trace)):-
    match1(Pat, T, Pat, T, Trace).

match1(W, X, Y, Z, Trace):-
    non_rigid_norm(X, list_length, 0, SizeX),
    non_rigid_norm(Z, list_length, 0, SizeZ),
    Level is SizeX + (SizeZ * SizeZ),
    match1(W, X, Y, Z, Level, SizeX, SizeZ, unbounded, Trace).

match11(W, X, Y, Z, D, E, F, Mode, Trace):-
    (D >= 0, E >= 0, F >= 0 ->
            match1(W, X, Y, Z, D, E, F, Mode, Trace)
     ;
            Mode = unbounded,
            rigid_norm(X, list_length, 0, SizeX, _),
            rigid_norm(Z, list_length, 0, SizeZ, _),
            bounded_level_mapping_two(match, SizeX, SizeZ, Level, _),
            freeze(Level, match1(W, X, Y, Z, Level, SizeX, SizeZ,
                                 bounded, Trace))
    ).

match1([], _Ts, _P, _T, _, _, _, _, 2).
match1([A|_Ps], [B|_Ts], P, [_X|T], D, E, F, Mode, trace(3, Neq, Mat)):-
    D1 is D - (E + F),
    F1 is F - 1,
    local:'\=='(A, B, Neq),
    match11(P, T, P, T, D1, F1, F1, Mode, Mat).
match1([A|Ps], [A|Ts], P, T, D, E, F, Mode, trace(4, Mat)):-
    D1 is D - 1,
    E1 is E - 1,
    match11(Ps, Ts, P, T, D1, E1, F, Mode, Mat).

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

:- block bounded_level_mapping_two(?, -, ?, ?, -),
         bounded_level_mapping_two(?, ?, -, ?, -).

bounded_level_mapping_two(match, SizeX, SizeZ, Level, _):-
    Level is SizeX + (SizeZ * SizeZ).

