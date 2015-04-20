:- module(unify, [unify/4, transpose/6]).
:- ensure_loaded(local).

unify(X, Y, S, trace(1, Uni)):-
    unify(X, Y, [], S, Uni).

struct > var

unify(W, X, Y, Z, Trace):-
     unify(W, X, Y, Z, 0, unbounded, Trace).

unify(var(N), T, S, S1):-
    bound(var(N), S, B, V),
    unify(var(N), T, S, S1, B, V).
unify(struct(F,Args), var(N), S, S1):-
    unify(var(N), struct(F,Args), S, S1).
unify(struct(F,Args1), struct(F,Args2), S, S2):-
    unifyargs(Args1, Args2, S, S2).

5
unify(var(_), T, S, S1, B, true):-
    unify(B, T, S, S1).
unify(var(N), T, S, S1, _, false):-
    unify1(T, var(N), S, S1).

%------------------------------------------------------------------

unifyargs(W, X, Y, Z, Trace):-
     non_rigid_norm(list_length, W, 0, SizeW),
     non_rigid_norm(list_length, X, 0, SizeX),
     local:max(SizeW, SizeX, Level),
     unifyargs(W, X, Y, Z, Level, unbounded, Trace).

unifyargs(W, X, Y, Z, D, Mode, Trace):-
    (D >= 0 ->
          unifyargs1(W, X, Y, Z, D, Mode, Trace)
     ;
          Mode = unbounded,
          rigid_norm(list_length, W, 0, SizeW, Mutex),
          rigid_norm(list_length, X, 0, SizeX, Mutex),
          bounded_level_mapping_one('=', SizeW, Level, Mutex),
          bounded_level_mapping_one('=', SizeX, Level, Mutex),
          freeze(Level, unifyargs1(W, X, Y, Z, Level, bounded, Trace))
    ).

unifyargs1([], [], S, S, _, _, 7).
unifyargs1([T|Ts], [R|Rs], S, S2, D, Mode, trace(8, Uni, UniA)):-
    D1 is D - 1,
    unify(T, R, S, S1, Uni),
    unifyargs(Ts, Rs, S1, S2, D1, Mode, UniA).


%------------------------------------------------------------------

unify1(struct(F,Args), var(N), S, [var(N)/struct(F,Args)|S], trace(9, Neg)):-
    \+(occur_args(var(N), Args, S, _)).
unify1(var(N), var(N), S, S, 10).
unify1(var(M), var(N), S, S1, trace(11, Neq, Bound, Uni)):-
    '\=='(M, N, Neq),
    bound(var(M), S, B, V, Bound),
    freeze(_, unify1(var(M), var(N), S, S1, B, V, Uni)).

unify1(var(_), var(N), S, S1, B, true, trace(12, Uni)):-
    freeze(_, unify1(B, var(N), S, S1, Uni)).
unify1(var(M), var(N), S, [var(N)/var(M)|S], _, false, 13).


%------------------------------------------------------------------

bound(V, L, T, F, Trace):-
    non_rigid_norm(L, list_length, 0, Level),
    bound(V, L, T, F, Level, unbounded, Trace).

bound(V, L, T, F, D, Mode, Trace):-
    (D >= 0 ->
          bound1(V, L, T, F, D, Mode, Trace)
     ;
          Mode = unbounded,
          rigid_norm(X, list_length, 0, Level, _),
          freeze(Level, bound1(V, L, T, F, Level, bounded, Trace))
    ).

bound1(var(N), [var(N)/T|_], T, true, _, _, trace(14, Neq)):-
    '\=='(T, var(N), Neq).
bound1(var(N), [B/_|S], T, F, D, Mode, trace(15, Neq, Bound)):-
    D1 is D - 1,
    '\=='(B, var(N), Neq),
    bound(var(N), S, T, F, D1, Mode, Bound).
bound1(var(_), [], _, false, _, _, 16).


%-----------------------------------------------------------------


dereference(V, L, T, Trace):-
    non_rigid_norm(L, list_length, 0, Level),
    dereference(V, L, T, Level, unbounded, Trace).

dereference(V, L, T, D, Mode, Trace):-
    (D >= 0 ->
          dereference1(V, L, T, D, Mode, Trace)
     ;
          Mode = unbounded,
          rigid_norm(X, list_length, 0, Level, _),
          freeze(Level, dereference1(V, L, T, Level, bounded, Trace))
    ).

dereference(var(N), [var(N)/T|_], T, _, _, trace(17, Neq)):-
    '\=='(T, var(N), Neq).
dereference(var(N), [B/_|S], T, D, Mode, trace(18, Neq, Deref)):-
    D1 is D - 1,
    '\=='(B, var(N), Neq),
    dereference(var(N), S, T, D1, Mode, Deref).


%-----------------------------------------------------------------


occur(var(N), var(M), S):-
    dereference(var(M), S, T),
    occur(var(N), T, S).
occur(var(N), var(N), _).
occur(var(N), struct(_,Args), S):-
    occur_args(var(N), Args, S).

occur_args(var(N), [A|_], S):-
    occur(var(N), A, S).
occur_args(var(N), [_|As], S):-
    occur_args(var(N), As, S).



