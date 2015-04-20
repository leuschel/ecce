
:- module(revacc, [rev/4]).
:- ensure_loaded(local).

rev(X, Acc, Y, Trace):-
    non_rigid_norm(X, list_length, 0, SizeX),
    non_rigid_norm(Y, list_length, 0, SizeY),
    local:max(SizeX, SizeY, Level),
    rev(X, Acc, Y, Level, 0, unbounded, Trace).

rev(X, Acc, Y, D, E, Mode, Trace):-
    (D >= 0 ->
            rev1(X, Acc, Y, D, E, Mode, Trace)
     ;
            Mode = unbounded,
            rigid_norm(X, list_length, 0, SizeX, Mutex),
            rigid_norm(Y, list_length, 0, SizeY, Mutex),
            bounded_level_mapping_one('=', SizeX, Level, Mutex),
            bounded_level_mapping_one('=', SizeY, Level, Mutex),
            freeze(Level, rev(X, Acc, Y, Level, E, bounded, Trace))
    ).

rev1([], L, L, _, _, _, 1).
rev1([X|Xs], Acc, Res, D, E, Mode, trace(2, IsList, Rev)):-
    D1 is D - 1,
    E1 is E + 1,
    is_list(Acc, E, Mode, IsList),
    rev(Xs, [X|Acc], Res, D1, E1, Mode, Rev).


is_list(X, Trace):-
    non_rigid_norm(X, list_length, 0, Level),
    is_list1(X, Level, unbounded, Trace).

is_list(X, D, Mode, Trace):-
    (D >= 0 ->
            is_list1(X, D, Mode, Trace)
     ;
            Mode = unbounded,
            rigid_norm(X, list_length, 0, Level, _),
            freeze(Level, is_list1(X, Level, bounded, Trace))
    ).

is_list1([], _, _, 3).
is_list1([_|T], D, Mode, trace(4, IsList)):-
    D1 is D - 1,
    is_list(T, D1, Mode, IsList).


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

