:- module(upto, [square_square/3, sumsquaresupto/3, sumtrsquaretr/3]).
:- ensure_loaded(local).

square_square(L, SSL, trace(1, Sq1, Sq2)):-
    squares(L, SL, Sq1),
    squares(SL, SSL, Sq2).

sumsquaresupto(N, S, trace(2, Up, Sq, Sum)):-
   upto(1, N, Ns, Up),
   squares(Ns, SONs, Sq),
   sum(SONs, S, Sum).

sumtrsquaretr(XT, S, trace(3, Sq, Sum)):-
   squaretr(XT, SOXt, Sq),
   sumtr(SOXt, S, Sum).

%-----------------------------------------------------------------

upto(X, Y, Z, Trace):-
    non_rigid_norm(Z, list_length, 0, Level),
    upto(X, Y, Z, Level, unbounded, Trace).

upto(X, Y, Z, D, Mode, Trace):-
    (D >= 0 ->
          upto1(X, Y, Z, D, Mode, Trace)
     ;
          Mode = unbounded,
          rigid_norm(Z, list_length, 0, Level, _),
          freeze(Level, upto1(X, Y, Z, Level, bounded, Trace))
    ).

upto1(M, N, [], _, _, trace(4, Gr)):-
    local:'>'(M, N, Gr).
upto1(M, N, [M|Ms], D, Mode, trace(5, Leq, Is, Up)):- 
    D1 is D - 1,
    local:'=<'(M, N, Leq),
    local:is(M1, M + 1, Is),
    upto(M1, N, Ms, D1, Mode, Up).

%-----------------------------------------------------------------

sum(Ns, S, trace(6, Sum1)):-
    sum1(Ns, 0, S, Sum1).

%-----------------------------------------------------------------

sum1(X, Y, Z, Trace):-
    non_rigid_norm(X, list_length, 0, Level),
    sum1(X, Y, Z, Level, unbounded, Trace).

sum1(X, Y, Z, D, Mode, Trace):-
    (D >= 0 ->
          sum11(X, Y, Z, D, Mode, Trace)
     ;
          Mode = unbounded,
          rigid_norm(X, list_length, 0, Level, _),
          freeze(Level, sum11(X, Y, Z, Level, bounded, Trace))
    ).

sum11([], S, S, _, _, 7).
sum11([N|Ns], A, S, D, Mode, trace(8, Is, Sum1)):-
    D1 is D - 1,
    local:is(A1, A + N, Is),
    sum1(Ns, A1, S, D1, Mode, Sum1).

%-----------------------------------------------------------------

square(N, SON, trace(9, Is)):-
    local:is(SON, N * N, Is).

%-----------------------------------------------------------------

squares(X, Y, Trace):-
     non_rigid_norm(X, list_length, 0, SizeX),
     non_rigid_norm(Y, list_length, 0, SizeY),
     local:max(SizeX, SizeY, Level),
     squares(X, Y, Level, unbounded, Trace).

squares(X, Y, D, Mode, Trace):-
    (D >= 0 ->
          squares1(X, Y, D, Mode, Trace)
     ;
          Mode = unbounded,
          rigid_norm(X, list_length, 0, SizeX, Mutex),
          rigid_norm(Y, list_length, 0, SizeY, Mutex),

          bounded_level_mapping_one('=', SizeX, Level, Mutex),
          bounded_level_mapping_one('=', SizeY, Level, Mutex),
          freeze(Level, squares1(X, Y, Level, bounded, Trace))
    ).

squares1([], [], _, _, 10).
squares1([N|Ns], [SON|SONs], D, Mode, trace(11, Sq, Sqs)):-
    D1 is D - 1,
    square(N, SON, Sq),
    squares(Ns, SONs, D1, Mode, Sqs).

%-----------------------------------------------------------------

sumtr(X, Y, Trace):-
     non_rigid_norm(X, branch_size, 0, Level),
     sumtr1(X, Y, Level, unbounded, Trace).

sumtr(X, Y, D, Mode, Trace):-
    (D >= 0 ->
          sumtr_det(X, Y, D, Mode, Trace)
     ;
          Mode = unbounded,
          rigid_norm(X, branch_size, 0, Level, _),
          freeze(Level, sumtr_det(X, Y, Level, bounded, Trace))
    ).

:- block sumtr_det(-,?,?,?,?).

sumtr_det(X, Y, D, Mode, Trace):-
    sumtr1(X, Y, D, Mode, Trace).

sumtr1(leaf(X), X, _, _, 12).
sumtr1(branch(Xt,Yt), S, D, Mode, trace(13, Sumtr1, Sumtr2, Is)):-
    D1 is D - 1,
    sumtr(Xt, SX, D1, Mode, Sumtr1),
    sumtr(Yt, SY, D1, Mode, Sumtr2),
    local:is(S, SX + SY, Is).

%-----------------------------------------------------------------

squaretr(X, Y, Trace):-
     non_rigid_norm(X, branch_size, 0, SizeX),
     non_rigid_norm(Y, branch_size, 0, SizeY),
     local:max(SizeX, SizeY, Level),
     squaretr1(X, Y, Level, unbounded, Trace).

squaretr(X, Y, D, Mode, Trace):-
    (D >= 0 ->
          squaretr_det(X, Y, D, Mode, Trace)
     ;
          Mode = unbounded,
          rigid_norm(X, branch_size, 0, SizeX, Mutex),
          rigid_norm(Y, branch_size, 0, SizeY, Mutex),
          bounded_level_mapping_one('=', SizeX, Level, Mutex),
          bounded_level_mapping_one('=', SizeY, Level, Mutex),
          freeze(Level, squaretr_det(X, Y, Level, bounded, Trace))
    ).

:- block squaretr_det(-,-,?,?,?).

squaretr_det(X, Y, D, Mode, Trace):-
    squaretr1(X, Y, D, Mode, Trace).

squaretr1(leaf(X), leaf(SOX), _, _, trace(14, Sq)):- 
    square(X, SOX, Sq).
squaretr1(branch(Xt,Yt), branch(SOXt,SOYt), D, Mode, trace(15, Sq1, Sq2)):-
    D1 is D - 1,
    squaretr(Xt, SOXt, D1, Mode, Sq1),
    squaretr(Yt, SOYt, D1, Mode, Sq2).


%-----------------------------------------------------------------
%
% Norms and level mappings

non_rigid_norm(V, _, D, D):-
    var(V), !.
non_rigid_norm([], list_length, D, D).
non_rigid_norm([_|Y], list_length, D_in, D_out):-
    D_in1 is D_in + 1,
    non_rigid_norm(Y, list_length, D_in1, D_out).

non_rigid_norm(leaf(_), branch_size, D, D).
non_rigid_norm(branch(X, Y), branch_size, D_in, D_out):-
    D_in1 is D_in + 1,
    non_rigid_norm(X, branch_size, D_in1, D_mid),
    non_rigid_norm(Y, branch_size, D_mid, D_out).


:- block rigid_norm(-, ?, ?, ?, -), rigid_norm(?, ?, -, ?, -).

rigid_norm(_, _, _, _, Mutex):-
    ground(Mutex), !.
rigid_norm([], list_length, D, D, _).
rigid_norm([_|Y], list_length, D_in, D_out, Mutex):-
    D_in1 is D_in + 1,
    rigid_norm(Y, list_length, D_in1, D_out, Mutex).

rigid_norm(leaf(_), branch_size, D, D, _).
rigid_norm(branch(X, Y), branch_size, D_in, D_out, Mutex):-
    D_in1 is D_in + 1,
    rigid_norm(X, branch_size, D_in1, D_mid, Mutex),
    rigid_norm(Y, branch_size, D_mid, D_out, Mutex).


:- block bounded_level_mapping_one(?, -, ?, -).

bounded_level_mapping_one(_, _, _, Mutex):-
    ground(Mutex), !.
bounded_level_mapping_one('=', Level, Level, 0).
