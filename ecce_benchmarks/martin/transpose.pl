:- module(transpose, [transpose/3, transpose/5]).
:- ensure_loaded(local).

transpose(X, Y, Trace):-
    non_rigid_norm(X, sub_lists_length, 0, SizeX, 0, LengthX),
    non_rigid_norm(Y, list_length, 0, SizeY),
    local:max(SizeX, SizeY, Level),
    transpose1(X, Y, Level, LengthX, unbounded, Trace).

transpose(X, Y, D, E, Mode, Trace):-
    (D >= 0 ->
            transpose1(X, Y, D, E, Mode, Trace)
     ;
            Mode = unbounded,
            rigid_norm(X, sub_lists_length, 0, SizeX, Mutex),
            rigid_norm(Y, list_length, 0, SizeY, Mutex),
            bounded_level_mapping_one('=', SizeX, Level, Mutex),
            bounded_level_mapping_one('=', SizeY, Level, Mutex),
            freeze(Level, transpose1(X, Y, Level, _, bounded, Trace))
    ).

transpose1(Xs, [], _, E, Mode, trace(1, Null)):-
        nullrows(Xs, E, Mode, Null).

transpose1(Xs, [Y|Ys], D, E, Mode, trace(2, Make, Trans)):-
	D1 is D - 1,
        makerow(Xs, Y, Zs, E, Mode, Make),
        transpose(Zs, Ys, D1, E, Mode, Trans).

%------------------------------------------------------------------

makerow(X, Y, Z, Trace):-
    non_rigid_norm(X, list_length, 0, SizeX),
    non_rigid_norm(Y, list_length, 0, SizeY),
    non_rigid_norm(Z, list_length, 0, SizeZ),
    local:max(SizeX, SizeY, Max),
    local:max(Max, SizeZ, Level),
    makerow1(X, Y, Z, Level, unbounded, Trace).


makerow(X, Y, Z, D, Mode, Trace):-
    (D >= 0 ->
          makerow1(X, Y, Z, D, Mode, Trace)
     ;
          Mode = unbounded,
          rigid_norm(X, list_length, 0, SizeX, Mutex),
          rigid_norm(Y, list_length, 0, SizeY, Mutex),
          rigid_norm(Z, list_length, 0, SizeZ, Mutex),
          bounded_level_mapping_one('=', SizeX, Level, Mutex),
          bounded_level_mapping_one('=', SizeY, Level, Mutex),
          bounded_level_mapping_one('=', SizeZ, Level, Mutex),
          freeze(Level, makerow1(X, Y, Z, Level, bounded, Trace))
    ).

makerow1([], [], [], _, _, 3).
makerow1([[X|Xs]|Ys], [X|Xs1], [Xs|Zs], D, Mode, trace(4, Make)):-
	D1 is D - 1,
        makerow(Ys, Xs1, Zs, D1, Mode, Make).


%------------------------------------------------------------------

nullrows(X, Trace):-
    non_rigid_norm(X, list_length, 0, Level),
    nullrows1(X, Level, unbounded, Trace).

nullrows(X, D, Mode, Trace):-
    (D >= 0 ->
          nullrows1(X, D, Mode, Trace)
     ;
          Mode = unbounded,
          rigid_norm(X, list_length, 0, Level, _),
          freeze(Level, nullrows1(X, Level, bounded, Trace))
    ).

nullrows1([], _, _, 5).
nullrows1([[]|Ns], D, Mode, trace(6, Null)) :-
	D1 is D - 1,
        nullrows(Ns, D1, Mode, Null).


%-----------------------------------------------------------------
%
% Norms and level mappings

non_rigid_norm(V, _, D, D):-
    var(V), !.
non_rigid_norm([], list_length, D, D).
non_rigid_norm([_|Y], list_length, D_in, D_out):-
    D_in1 is D_in + 1,
    non_rigid_norm(Y, list_length, D_in1, D_out).

non_rigid_norm(V, _, D, D, L, L):-
    var(V), !.
non_rigid_norm([], sub_lists_length, D, D, L, L).
non_rigid_norm([X|Y], sub_lists_length, D_in, D_out, L_in, L_out):-
    L_in1 is L_in + 1,
    non_rigid_norm(X, list_length, D_in, D_mid),
    local:max(D_in, D_mid, D_max),
    non_rigid_norm(Y, sub_lists_length, D_max, D_out, L_in1, L_out).


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
