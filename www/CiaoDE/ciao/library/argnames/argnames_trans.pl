:- module(argnames_trans, [argnames_def/3, argnames_use/3], [assertions]).

:- use_module(library(terms), [arg/2]).

:- data argnames/4.

argnames_def((:- argnames(R)), _, M) :-
        functor(R, F, N),
        add_argnames_def(R, F, N, M).
argnames_def((:- data(R)), (:- data(F/N)), M) :-
        functor(R, F, N),
        (F,N) \== ('/',2),
        (F,N) \== (',',2),
        add_argnames_def(R, F, N, M).
argnames_def(end_of_file, end_of_file, M) :-
        retractall_fact(argnames(_,_,_,M)).

add_argnames_def(R, F, N, M) :-
        ( argnames(F, _, R0, M)  ->
            ( R0 == R -> true
            ; error(['incompatible argnames declaration ',R])
            )
        ; arg(R, A), \+ atomic(A) ->
            error(['invalid argnames declaration ',R])
        ; assertz_fact(argnames(F,N,R,M))
        ).

argnames_use($(F,TheArgs), T, M) :-
        atom(F),
        argnames_args(TheArgs, Args),
        argnames_trans(Args, F, M, T).

argnames_trans((/), F, M, T) :-
        argnames(F, A, _, M),
        T = F/A, !.
argnames_trans(Args, F, M, T) :-
        argnames(F, A, R, M),
        functor(T, F, A),
        insert_args(Args, R, A, T), !.
argnames_trans(Args, F, _, _) :-
        argnames_args(TheArgs, Args), !,
        warning(['invalid argnames ',F,' $ ',~~(TheArgs),' - not translated']),
        fail.

insert_args([], _, _, _).
insert_args('=>'(F,A), R, N, T) :-
        insert_arg(N, F, A, R, T).
insert_args(('=>'(F,A), As), R, N, T) :-
        insert_arg(N, F, A, R, T),
        insert_args(As, R, N, T).

insert_arg(N, F, A, R, T) :-
        N > 0,
        (   arg(N, R, F) ->
                arg(N, T, A)
        ;   N1 is N-1,
            insert_arg(N1, F, A, R, T)
        ).

argnames_args({}, []).
argnames_args({Args}, Args).

/********************************
  Example translations :

:- argnames person(name, age, profession).

p(person${}).
q(person${age=> 25}).
r(person${name=> D, profession=>prof(D),age=>age(D)}).
s(person${age=>t(25), name=> daniel}).

% argnames(person, 3, person(name,age,profession)).
% 
% p(person(_,_,_)).
% q(person(_,25,_)).
% r(person(A,age(A),prof(A))).
% s(person(daniel,t(25),_)).

********************************/
