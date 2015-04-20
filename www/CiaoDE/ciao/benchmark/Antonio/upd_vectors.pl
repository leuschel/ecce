:- module(upd_vectors,
        [
            make_vector/3,
            make_vector_free/2,
            unify_vector_element/3,
            access_vector/3,
            update_vector/3,
            access_update_vector/4,
            access_update_vector_ho/5
        ], []).

:- use_module(library(odd), [setarg/3]).  % Yes! I am using it!
:- use_package(hiord).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Maximum arity to use, minus one (I need one extra position to hold
% the bucket size at each level, see below).

max_arity(255).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_vector(Dims, Array, Init):- make_vector_(Dims, Array, Init, init).

%% Create new vector with dimensions Dims; all values in it will be
%% free, distinct variables.  This is useful to use write-once vectors
%% (which are created and consulte, but which do not need to be updated).
make_vector_free(Dims, Array):- make_vector_(Dims, Array, _Init, no_init).


% create a new vector: we need the dope vector (to store it
% in the vector) and the size (to create the vector itself).
make_vector_(Size, '$vector'(Size, Container), InitV, InitS):-
        max_arity(MaxArity),
        make_container(Size, MaxArity, Container, InitV, InitS).

make_container(NumEls, MaxArity, cont(1, Container), InitV, InitS):-
        NumEls =< MaxArity, !,                  % This is a leave
        functor(Container, '$vector', NumEls),
        (
            InitS = init ->
            initialize_leave(NumEls, Container, InitV)
        ;
            true
        ).

make_container(NumEls, MaxArity, cont(BucketSize, Container), InitV, InitS):-
        NumEls > MaxArity,                        % Intermediate level
        BucketSize is ceiling(NumEls / MaxArity), % Size of this bucket
        NumBucks is ceiling(NumEls / BucketSize), % No. of buckets
        functor(Container, '$vector', NumBucks),
        initialize_middle(NumBucks, BucketSize, MaxArity, Container, InitV, InitS).

initialize_middle(0, _BucketSize, _Max, _Container, _InitV, _InitS):- !.
initialize_middle(NumBucks, BucketSize, MaxArity, Container, InitV, InitS):-
        NumBucks > 0,
        arg(NumBucks, Container, Arg),
        make_container(BucketSize, MaxArity, Arg, InitV, InitS),
        NextNumBucks is NumBucks - 1,
        initialize_middle(NextNumBucks, BucketSize, MaxArity, Container, InitV, InitS).

initialize_leave(0, _F, _I).
initialize_leave(N, Functor, Init):-
        N > 0,
        arg(N, Functor, Init),
        N1 is N - 1,
        initialize_leave(N1, Functor, Init).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Indexes: they start at 0!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

access_vector(A, Loc, Ele):-
        leaf_functor(A, Loc, Functor, Position),
        arg(Position, Functor, Ele).

unify_vector_element(Array, Loc, Ele):- access_vector(Array, Loc, Ele).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_vector(A, Loc, New):-
        leaf_functor(A, Loc, Functor, Position),
        setarg(Position, Functor, New).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate(access_update_vector_ho(?, ?, ?, ?, pred(2))).

access_update_vector_ho(A, Loc, Old, New, Update):-
        leaf_functor(A, Loc, Functor, Position),
        arg(Position, Functor, Old),
        Update(Old, New),
        setarg(Position, Functor, New).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

access_update_vector(A, Loc, Old, New):-
        leaf_functor(A, Loc, Functor, Position),
        arg(Position, Functor, Old),
        setarg(Position, Functor, New).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

leaf_functor(Vector, Index, Functor, Position):-
        nonvar(Vector),
        Vector = '$vector'(Size, Container),
        Index >= 0, Index < Size,  %% Check wether in limits
        Ind1 is Index + 1,
        locate_leaf(1, Ind1, Container, Functor, Position).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 
locate_leaf(Low, Index, cont(BucketSize, Container), Functor, Position):-
        (
            BucketSize = 1 ->
            Position is Index - Low + 1,
            Container = Functor
        ;
            TargetIndex is ((Index - Low) // BucketSize) + 1,
            arg(TargetIndex, Container, Bucket),
            NewLow is Low + BucketSize * (TargetIndex - 1),
            locate_leaf(NewLow, Index, Bucket, Functor, Position)
        ).
