

:- module(map, [map/4]).
:- ensure_loaded(local).

map(P, X, Y, Trace):-
    non_rigid_norm(X, list_length, 0, SizeX),
    non_rigid_norm(Y, list_length, 0, SizeY),
    local:max(SizeX, SizeY, Level),
    map(P, X, Y, Level, unbounded, Trace).

map(P, X, Y, D, Mode, Trace):-
    (D >= 0 ->
            map1(P, X, Y, D, Mode, Trace)
     ;
            Mode = unbounded,
            rigid_norm(X, list_length, 0, SizeX, Mutex),
            rigid_norm(Y, list_length, 0, SizeY, Mutex),
            bounded_level_mapping_one('=', SizeX, Level, Mutex),
            bounded_level_mapping_one('=', SizeY, Level, Mutex),
            freeze(Level, map(P, X, Y, Level, bounded, Trace))
    ).

map1(_P, [], [], _, _, 1).
map1(P, [H|T], [PH|PT], D, Mode, trace(2, Con, Ca, Map)):-
    D1 is D - 1,
    local:'=..'(Call, P, [H,PH], Con), % Call =.. [P,H,PH],
    local:call(map:Call, Ca),
    map(P, T, PT, D1, Mode, Map).


%-----------------------------------------------------------------

reduce(F, B, X, R, Trace):-
    non_rigid_norm(X, list_length, 0, Level),
    reduce(F, B, X, R, Level, unbounded, Trace).

reduce(F, B, X, R, D, Mode, Trace):-
    (D >= 0 ->
          reduce1(F, B, X, R, D, Mode, Trace)
     ;
          Mode = unbounded,
          rigid_norm(X, list_length, 0, Level, _),
          freeze(Level, reduce1(F, B, X, R, Level, bounded, Trace))
    ).

reduce1(_Func, Base, [], Base, _, _, 3).
reduce1(Func, Base, [H|T], Res, D, Mode, trace(4, Red, Con, Ca)):-
    D1 is D - 1,
    reduce(Func, Base, T, TRes, D1, Mode, Red),
    local:'=..'(Call, Func, [H,TRes,Res], Con), % Call =.. [Func,H,TRes,Res],
    local:call(map:Call, Ca).

%-----------------------------------------------------------------

q(a, b, 5).
q(b, c, 6).
q(c, d, 7).
q(d, e, 8).

%-----------------------------------------------------------------

reduce_add(List, Res, trace(9, Red)):-
    reduce(add, 0, List, Res, Red).

%-----------------------------------------------------------------

add(X, Y, Z, trace(10, Is)):-
    local:is(Z, X + Y, Is).

%-----------------------------------------------------------------

rev(L, R, trace(11, Rev)):-
    rev(L, [], R, Rev).

rev(L, Acc, R, Trace):-
    non_rigid_norm(L, list_length, 0, SizeL),
    non_rigid_norm(R, list_length, 0, SizeR),
    local:max(SizeL, SizeR, Level),
    rev(L, Acc, R, Level, unbounded, Trace).

rev(X, Acc, Y, D, Mode, Trace):-
    (D >= 0 ->
            rev1(X, Acc, Y, D, Mode, Trace)
     ;
            Mode = unbounded,
            rigid_norm(X, list_length, 0, SizeX, Mutex),
            rigid_norm(Y, list_length, 0, SizeY, Mutex),
            bounded_level_mapping_one('=', SizeX, Level, Mutex),
            bounded_level_mapping_one('=', SizeY, Level, Mutex),
            freeze(Level, rev(X, Acc, Y, Level, bounded, Trace))
    ).

rev1([], L, L, _, _, 12).
rev1([H|T], A, R, D, Mode, trace(13, Rev)):-
    D1 is D - 1,
    rev(T, [H|A], R, D1, Mode, Rev).


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

