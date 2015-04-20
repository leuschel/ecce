:- module(upd_arrays,
        [
            make_array/3,
            make_array_free/2,
            access_array/3,
            unify_array_element/3,
            update_array/3,
            access_update_array/4,
            access_update_array_ho/5
        ], [assertions,hiord]).


:- use_module(library(odd), [setarg/3]).  % Yes! I am using it!

:- comment(author, "Manuel Carro").

:- comment(title, "Multi-dimensional (updatable) arrays").

:- comment(summary, "This package provides an abstract data type for
multi-dimensional (updatable) arrays.").

:- comment(module, "Multi-dimensional (updatable) arrays implement
primitives for the creation, access, and modification of arrays.
Modification can be performed monotonically (i.e., instantiating
variables inside the array) or destructively (i.e., replacing
components inside the array using @pred{setarg/3}).

Destructive assignment, besides being @bf{against} the spirit of Logic
Programming, is slower than creating an array full of free, unique
variables, and then instantiating these variables.  If the intended
use of the array is to create it and then use its contents, we
recommend creating an array full of free variables and instanting them
rather than setting an initial value for the array compnentes and then
updating the array cells.

The library @ref{arrays} might also be of interest to the reader
interested this library.

").

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
make_array_(Dims, '$array'(Size,Ind,Dims,Dope,Cont), InitValue, InitSwitch):-
        dope(Dims, Dope, Ind),
        size(Dims, 1, Size),
        max_arity(MaxArity),
        make_container(Size, MaxArity, Cont, InitValue, InitSwitch).

%% A precomputed vector to speed up computing the final index.
dope(Dims, Dope, Indexes):- dope2(Dims, 1, Dope, Indexes).
dope2([], _Curr, [], []).
dope2([D|Ds], Curr, [Curr|Rest], [I|Is]):-
        I is D - 1,
        NextCurr is Curr*D,
        dope2(Ds, NextCurr, Rest, Is).

%% Total number of elements in the array.
size([], D, D).
size([S|Ss], I, R):-
        I1 is I * S,
        size(Ss, I1, R).


%% This makes the actual container where the elements of the array will
%% go.  In cont(Num, Functor), Functor corresponds to the structure
%% which will hold the array, and Num is how many elements (maximum)
%% will be stored in each argument of the functor (in each bucket).

make_container(NumEls, MaxArity, cont(1, Container), InitValue, InitSwitch):-
        NumEls =< MaxArity, !,                  % This is a leave
        functor(Container, '$array', NumEls),
        (
            InitSwitch = init ->
            initialize_leave(NumEls, Container, InitValue)
        ;
            true
        ).
make_container(NumEls, MaxAr, cont(BSize, Cont), InitVal, InitSwitch):-
        NumEls > MaxAr,                        % Intermediate level
        BSize is ceiling(NumEls / MaxAr), % Size of this bucket
        NumBucks is ceiling(NumEls / BSize), % No. of buckets
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
        Array = '$array'(Size, _Ind, _Dims, Dope, Container),
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


locate_leaf(Low, Index, cont(BucketSize, Container), Functor, Position):-
        (
            BucketSize = 1 ->
            Position is Index - Low + 1,
            Container = Functor
        ;
            TargetIndex is ((Index - Low) // BucketSize) + 1,
            NewLow is Low + BucketSize * (TargetIndex - 1),
            arg(TargetIndex, Container, Bucket),
            locate_leaf(NewLow, Index, Bucket, Functor, Position)
        ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

array_dimensions(Array, Dimensions):-
        nonvar(Array),
        Array = '$array'(_Size, _Ind, Dimensions, _Dope, _Container).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

array_indexes(Array, Indexes):-
        nonvar(Array),
        Array = '$array'(_Size, Indexes, _Dims, _Dope, _Container).
