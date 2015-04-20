:- module(maxlength, []).
:- ensure_loaded(local).

max_length(Ls, M, Len, trace(1, Max, MyL)):-
    max(Ls, M, Max),
    my_length(Ls, Len, MyL).

%------------------------------------------------------------------

my_length(X, Y, Trace):-
    non_rigid_norm(X, list_length, 0, SizeX),
    non_rigid_norm(Y, integer, 0, SizeY),
    local:max(SizeX, SizeY, Level),
    my_length(X, Y, Level, unbounded, Trace).

my_length(X, Y, D, Mode, Trace):-
    (D >= 0 ->
            my_length1(X, Y, D, Mode, Trace)
     ;
            Mode = unbounded,
            rigid_norm(X, list_length, 0, SizeX, Mutex),
            rigid_norm(Y, list_length, 0, SizeY, Mutex),
            bounded_level_mapping_one('=', SizeX, Level, Mutex),
            bounded_level_mapping_one('=', SizeY, Level, Mutex),
            freeze(Level, my_length(X, Y, Level, bounded, Trace))
    ).

my_length1([], 0, _, _, 2).
my_length1([_X|T], L, D, Mode, trace(3, MyL, Is)) :-
    D1 is D - 1,
    my_length(T, LT, D1, Mode, MyL),
    local:is(L, LT + 1, Is).

%------------------------------------------------------------------

max(X, M, trace(4, Max1)):-
    max1(X, 0, M, Max1).

%------------------------------------------------------------------

max1(X, Y, Z, Trace):-
    non_rigid_norm(X, list_length, 0, Level),
    max1(X, Y, Z, Level, unbounded, Trace).

max1(X, Y, Z, D, Mode, Trace):-
    (D >= 0 ->
          max11(X, Y, Z, D, Mode, Trace)
     ;
          Mode = unbounded,
          rigid_norm(X, list_length, 0, Level, _),
          freeze(Level, max11(X, Y, Z, Level, bounded, Trace))
    ).

max11([], M, M, _, _, 5).
max11([H|T], N, M, D, Mode, trace(6, Leq, Max1)):-
    D1 is D - 1,
    local:'=<'(H, N, Leq),
    max1(T, N, M, D1, Mode, Max1).
max11([H|T], N, M, D, Mode, trace(7, Gr, Max1)) :-
    D1 is D - 1,
    local:'>'(H, N, Gr),
    max1(T, H, M, D1, Mode, Max1).

%-----------------------------------------------------------------
%
% Norms and level mappings

non_rigid_norm(V, _, D, D):-
    var(V), !.
non_rigid_norm([], list_length, D, D).
non_rigid_norm([_|Y], list_length, D_in, D_out):-
    D_in1 is D_in + 1,
    non_rigid_norm(Y, list_length, D_in1, D_out).
non_rigid_norm(X, integer, _, X):-
    integer(X), !.
    

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
