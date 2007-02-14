:- module(rotateprune, [rp/3]).
:- ensure_loaded(local).

rp(T1, T2, trace(1, Rot, Prune)):-
    rotate(T1, U, Rot),
    prune(U, T2, Prune).

%-----------------------------------------------------------------

rotate(X, Y, Trace):-
     non_rigid_norm(X, tree_size, 0, SizeX),
     non_rigid_norm(Y, tree_size, 0, SizeY),
     local:max(SizeX, SizeY, Level),
     rotate1(X, Y, Level, unbounded, Trace).

rotate(X, Y, D, Mode, Trace):-
    (D >= 0 ->
          rotate_det(X, Y, D, Mode, Trace)
     ;
          Mode = unbounded,
          rigid_norm(X, tree_size, 0, SizeX, Mutex),
          rigid_norm(Y, tree_size, 0, SizeY, Mutex),
          bounded_level_mapping_one('=', SizeX, Level, Mutex),
          bounded_level_mapping_one('=', SizeY, Level, Mutex),
          freeze(Level, rotate_det(X, Y, Level, bounded, Trace))
    ).

:- block rotate_det(-,-,?,?,?).

rotate_det(X, Y, D, Mode, Trace):-
    rotate1(X, Y, D, Mode, Trace).

rotate1(leaf(N), leaf(N), _, _, 2).
rotate1(tree(L, N, R), tree(RL, N, RR), D, Mode, trace(3, Rot1, Rot2)):-
    D1 is D - 1,
    rotate(L, RL, D1, Mode, Rot1),
    rotate(R, RR, D1, Mode, Rot2).
rotate1(tree(L, N, R), tree(RR, N, RL), D, Mode, trace(4, Rot1, Rot2)):-
    D1 is D - 1,
    rotate(L, RL, D1, Mode, Rot1),
    rotate(R, RR, D1, Mode, Rot2).


%-----------------------------------------------------------------

prune(X, Y, Trace):-
     non_rigid_norm(X, tree_size, 0, SizeX),
     non_rigid_norm(Y, tree_size, 0, SizeY),
     local:max(SizeX, SizeY, Level),
     prune1(X, Y, Level, unbounded, Trace).

prune(X, Y, D, Mode, Trace):-
    (D >= 0 ->
          prune_det(X, Y, D, Mode, Trace)
     ;
          Mode = unbounded,
          rigid_norm(X, tree_size, 0, SizeX, Mutex),
          rigid_norm(Y, tree_size, 0, SizeY, Mutex),
          bounded_level_mapping_one('=', SizeX, Level, Mutex),
          bounded_level_mapping_one('=', SizeY, Level, Mutex),
          freeze(Level, prune_det(X, Y, Level, bounded, Trace))
    ).

:- block prune_det(-,-,?,?,?).

prune_det(X, Y, D, Mode, Trace):-
    prune1(X, Y, D, Mode, Trace).

prune1(leaf(N), leaf(N), _, _, 5).
prune1(tree(_L, 0, _R), leaf(0), _, _, 6).
prune1(tree(L, s(N), R), tree(PL, s(N), PR), D, Mode, trace(7, Pr1, Pr2)):-
    D1 is D - 1,
    prune(L, PL, D1, Mode, Pr1),
    prune(R, PR, D1, Mode, Pr2).

%-----------------------------------------------------------------
%
% Norms and level mappings

non_rigid_norm(V, _, D, D):-
    var(V), !.
non_rigid_norm(leaf(_), tree_size, D, D).
non_rigid_norm(tree(X, _, Y), tree_size, D_in, D_out):-
    D_in1 is D_in + 1,
    non_rigid_norm(X, tree_size, D_in1, D_mid),
    non_rigid_norm(Y, tree_size, D_mid, D_out).


:- block rigid_norm(-, ?, ?, ?, -), rigid_norm(?, ?, -, ?, -).

rigid_norm(_, _, _, _, Mutex):-
    ground(Mutex), !.

rigid_norm(leaf(_), tree_size, D, D, _).
rigid_norm(tree(X, _, Y), tree_size, D_in, D_out, Mutex):-
    D_in1 is D_in + 1,
    rigid_norm(X, tree_size, D_in1, D_mid, Mutex),
    rigid_norm(Y, tree_size, D_mid, D_out, Mutex).


:- block bounded_level_mapping_one(?, -, ?, -).

bounded_level_mapping_one(_, _, _, Mutex):-
    ground(Mutex), !.
bounded_level_mapping_one('=', Level, Level, 0).
