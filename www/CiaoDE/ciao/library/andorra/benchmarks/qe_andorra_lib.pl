:- module(qe_andorra_lib,_,_).

:- use_module(library(when)).

% utility to query the benchmarks

%query:- ourmain(L,L1), wakeup(L,L1).

% time to wake up!

wakeup(L1,L2) :- L1==L2, !.
wakeup([L1|L2],L3) :-
        L1=up,
        wakeup(L2,L3).

% suspend on any var of a term

%suspend_on_vars(X,Flag):- var(X), !, wait(X,Flag).
suspend_on_vars(X,Flag):- var(X), !, when((nonvar(X);nonvar(Flag)), Flag = go).
suspend_on_vars(T,Flag):-
	functor(T,_,A),
	go_inside(A,T,Flag).

go_inside(0,_T,_F):- !.
go_inside(N,T,Flag) :-
	arg(N,T,Arg),
	suspend_on_vars(Arg,Flag),
	Nth is N-1,
	go_inside(Nth,T,Flag).

% :- block wait(-,-).

% wait(_,go).

% check depth of a term

instantiated(Term,[]):- nonvar(Term).
instantiated(Term,[N|Path]):-
	nonvar(Term),
	functor(Term,_,A),
	A>=N,
	arg(N,Term,Arg),
	instantiated(Arg,Path).

% non-determinate versions of (some) builtins

=<(A,B,L,L0) :-
        ground(A/B), !,
	A=<B,
	L=L0.
=<(A,B,L,L0) :-
	L=[S|L0],
	when((ground(A/B);nonvar(S)),A=<B).

>(A,B,L,L0) :-
        ground(A/B), !,
	A>B,
	L=L0.
>(A,B,L,L0) :-
	L=[S|L0],
	when((ground(A/B);nonvar(S)),A>B).

>=(A,B,L,L0) :-
        ground(A/B), !,
	A>=B,
	L=L0.
>=(A,B,L,L0) :-
	L=[S|L0],
	when((ground(A/B);nonvar(S)),A>=B).

<(A,B,L,L0) :-
        ground(A/B), !,
	A<B,
	L=L0.
<(A,B,L,L0) :-
	L=[S|L0],
	when((ground(A/B);nonvar(S)),A<B).

=:=(A,B,L,L0) :-
        ground(A/B), !,
	A=:=B,
	L=L0.
=:=(A,B,L,L0) :-
	L=[S|L0],
	when((ground(A/B);nonvar(S)),A=:=B).

=\=(A,B,L,L0) :-
        ground(A/B), !,
	A=\=B,
	L=L0.
=\=(A,B,L,L0) :-
	L=[S|L0],
	when((ground(A/B);nonvar(S)),A=\=B).

\==(A,B,L,L0) :-
	L=[S|L0],
	when(nonvar(S),A\==B).

==(A,B,L,L0) :-
	L=[S|L0],
	when(nonvar(S),A==B).

@>(A,B,L,L0) :-
	L=[S|L0],
	when(nonvar(S),A@>B).

is(A,B,L,L0) :-
        ground(B),!,
	A is B,
	L=L0.
is(A,B,L,L0) :-
	L=[S|L0],
	when((ground(B);nonvar(S)),A is B).

integer(A,L,L0) :-
	L=[S|L0],
	when(nonvar(S),integer(A)).
