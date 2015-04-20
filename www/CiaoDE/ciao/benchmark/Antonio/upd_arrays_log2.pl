:- module(upd_arrays_log2,
        [
            make_array/3,
            make_array_free/2,
            access_array/3,
            unify_array_element/3,
            update_array/3,
            access_update_array/4,
            access_update_array_ho/5
        ], []).

:- use_module(library(odd), [setarg/3]).  % Yes! I am using it!
:- use_package(hiord).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Maximum arity to use, minus one (I need one extra position to hold
% the bucket size at each level, see below).

max_arity(255).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Create new array with dimensions Dims, and initialized to the value Init.
make_array(Dims, Array, Init):- make_array_(Dims, Array, Init, init).

%% Create new array with dimensions Dims; all values in it will be
%% free, distinct variables.  This is useful to use write-once arrays
%% (which are created and consulte, but which do not need to be updated).
make_array_free(Dims, Array):- make_array_(Dims, Array, _Init, no_init).

% Create a new array: we need the dope vector (to store it
% in the array) and the size (to create the array itself).
make_array_(Dims, '$array'(Size, Dope, Container), InitValue, InitSwitch):-
        dope(Dims, Dope),
        size(Dims, 1, Size),
        max_arity(MaxArity),
        make_container(Size, MaxArity, Container, InitValue, InitSwitch).

%% A precomputed vector to speed up computing the final index.
dope(Dims, Dope):- dope2(Dims, 1, Dope).
dope2([], _Curr, []).
dope2([D|Ds], Curr, [Curr|Rest]):-
        NextCurr is Curr*D,
        dope2(Ds, NextCurr, Rest).

%% Total number of elements in the array.
size([], D, D).
size([S|Ss], I, R):-
        I1 is I * S,
        size(Ss, I1, R).


%% This makes the actual container where the elements of the array will
%% go.  In cont(Exp, Functor), Functor corresponds to the structure
%% which will hold the array, and 2^Exp is how many elements (maximum)
%% will be stored in each argument of the functor (in each bucket).

make_container(NumEls, MaxArity, cont(0, Container), InitValue, InitSwitch):-
        NumEls =< MaxArity, !,                  % This is a leave
        functor(Container, '$array', NumEls),
        (
            InitSwitch = init ->
            initialize_leave(NumEls, Container, InitValue)
        ;
            true
        ).
make_container(NumEls, MaxAr, cont(BSizeLog, Cont), InitVal, InitSwitch):-
        NumEls > MaxAr,                         % Intermediate level
        BSizeLog is ceiling(log(ceiling(NumEls/MaxAr))/log(2)),
        BSize is ceiling(2**BSizeLog),          % Size of this bucket
        NumBucks is ceiling(NumEls / BSize),    % No. of buckets
        functor(Cont, '$array', NumBucks),
        initialize_middle(NumBucks, BSize, MaxAr, Cont, InitVal, InitSwitch).

initialize_middle(0, _BucketSize, _Max, _Container, _Init, _InitS):- !.
initialize_middle(NumBucks, BucketSize, MaxAr, Cont, InitV, InitS):-
        NumBucks > 0,
        arg(NumBucks, Cont, Arg),
        make_container(BucketSize, MaxAr, Arg, InitV, InitS),
        NextNumB is NumBucks - 1,
        initialize_middle(NextNumB, BucketSize, MaxAr, Cont, InitV, InitS).

initialize_leave(0, _F, _I).
initialize_leave(N, Functor, Init):-
        N > 0,
        arg(N, Functor, Init),
        N1 is N - 1,
        initialize_leave(N1, Functor, Init).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Note: all the indexes in the array start at 0!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% accss_array/3 and unify_array_element/3  are actually the same
%% predicate; unify_array_element/3 has a more logical flavor, to be
%% called when make_array_free/2 is used.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

access_array(Array, Loc, Ele):-
        leaf_functor(Array, Loc, Functor, Position),
        arg(Position, Functor, Ele).

unify_array_element(Array, Loc, Ele):- access_array(Array, Loc, Ele).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Destructive update --- it is undone on backtracking, though.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_array(Array, Loc, New):-
        leaf_functor(Array, Loc, Functor, Position),
        setarg(Position, Functor, New).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Retrieve the stored value and store a new one in one shot.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

access_update_array(Array, Loc, Old, New):-
        leaf_functor(Array, Loc, Functor, Position),
        arg(Position, Functor, Old),
        setarg(Position, Functor, New).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A trick to avoid accessing twice the same array: for example, the call
%% access_update_array_ho(A, L, O, N, (_(X, Y):- Y is X * 2))
%% will multiply the value pointe to by L by 2 with only one access.
%% I do not know if the idea is really useful in more general cases
%% --- need to look at that.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate(access_update_array_ho(?, ?, ?, ?, pred(2))).

access_update_array_ho(Array, Loc, Old, New, Update):-
        leaf_functor(Array, Loc, Functor, Position),
        arg(Position, Functor, Old),
        Update(Old, New),
        setarg(Position, Functor, New).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Return the (bottommost) functor where Loc is stored in Array, and
%% the position within the functor where Loc is stored.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

leaf_functor(Array, Loc, Functor, Position):-
        nonvar(Array),
        Array = '$array'(Size, Dope, Container),
        direct_product(Loc, Dope, 1, Index),
        Index > 0, Index =< Size,  %% Check wether in limits
        locate_leaf(1, Index, Container, Functor, Position).

% The index (in a linear vector) corresponding to a given index (Location)
% using the previously computed Dope vector.

direct_product([], [], Accum, Accum).
direct_product([L|Ls], [D|Ds], Accum, Index):-
        NewAccum is Accum+D*L,
        direct_product(Ls, Ds, NewAccum, Index).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% While this is the most heavily used routine, it is pretty fast ---
%% i.e., for arrays up to 65000 elements only two accesses are needed.


locate_leaf(Low, Index, cont(BSizeLog, Container), Functor, Position):-
        (
            BSizeLog = 0 ->
            Position is Index - Low + 1,
            Container = Functor
        ;
            TargetIndex is ((Index - Low) >> BSizeLog) + 1,
            arg(TargetIndex, Container, Bucket),
            NewLow is Low + (TargetIndex - 1) << BSizeLog,
            locate_leaf(NewLow, Index, Bucket, Functor, Position)
        ).
