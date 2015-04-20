:- module(app, [append/4, append/6]).
:- ensure_loaded(local).

append(X, Y, Z, Trace):-
     non_rigid_norm(list_length, X, 0, SizeX),
     non_rigid_norm(list_length, Z, 0, SizeZ),
     local:max(SizeX, SizeZ, Level),
     append1(X, Y, Z, Level, unbounded, Trace).

append(X, Y, Z, D, Mode, Trace):-
    (D >= 0 ->
          append1(X, Y, Z, D, Mode, Trace)
     ;
          Mode = unbounded,
          rigid_norm(list_length, X, 0, SizeX, Mutex),
          rigid_norm(list_length, Z, 0, SizeZ, Mutex),

          bounded_level_mapping_one('=', SizeX, Level, Mutex),
          bounded_level_mapping_one('=', SizeZ, Level, Mutex),
          freeze(Level, append1(X, Y, Z, Level, bounded, Trace))
    ).

append1([], X, X, _, _, 3).
append1([X|Xs], Y, [X|Zs], D, Mode, trace(4, App)):-
    D1 is D - 1,
    append(Xs, Y, Zs, D1, Mode, App).

%-----------------------------------------------------------------
%
% Norms and level mappings

non_rigid_norm(_, V, D, D):-
    var(V), !.
non_rigid_norm(list_length, [], D, D).
non_rigid_norm(list_length, [_|Y], D_in, D_out):-
    D_in1 is D_in + 1,
    non_rigid_norm(list_length, Y, D_in1, D_out).

:- block rigid_norm(?, -, ?, ?, -), rigid_norm(?, ?, -, ?, -).

rigid_norm(_, _, _, _, Mutex):-
    ground(Mutex), !.
rigid_norm(list_length, [], D, D, _).
rigid_norm(list_length, [_|Y], D_in, D_out, Mutex):-
    D_in1 is D_in + 1,
    rigid_norm(list_length, Y, D_in1, D_out, Mutex).

:- block bounded_level_mapping_one(?, -, ?, -).

bounded_level_mapping_one(_, _, _, Mutex):-
    ground(Mutex), !.
bounded_level_mapping_one('=', Level, Level, 0).
