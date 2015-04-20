:- module(fib_and1p1,[fib/2],_).
:- use_module(qe_andorra_lib).
:- use_module(library(when),[when/2]).

fib(X,Y) :-
	fib(X,Y,_1,_2),
	wakeup(_1,_2).

fib(A,B,_1,_2) :-
        _1=[_3|_4],
        fib_check(A,B,_3,_4,_2).

fib_check(A,B,_2,_3,_1) :-
        nonvar(_2),
        !,
        fib_work(A,B,_3,_1).

fib_check(A,B,_2,_3,_1) :-
        nonvar(A),
        !,
        fib_work(A,B,_3,_1).

fib_check(A,B,_2,_3,_1) :-
        nonvar(B),
        !,
        (
          B\==1  ->
             fib_work(A,B,_3,_1)
        ;
             when((nonvar(A);nonvar(_2)),fib_check(A,B,_2,_3,_1))
        ).

fib_check(A,B,_2,_3,_1) :-
        when((nonvar(B);nonvar(A);nonvar(_2)),fib_check(A,B,_2,_3,_1)).

fib_work(0,1,_1,_1).

fib_work(1,1,_1,_1).

fib_work(A,B,_1,_2) :-
        >(A,1,_1,_3),
        is(C,A-1,_3,_4),
        is(D,A-2,_4,_5),
        fib(C,E,_5,_6),
        fib(D,F,_6,_7),
        is(B,F+E,_7,_2).

test(_1,_2) :-
        fib(15,F,_1,_2).

go(F,_1,_2) :-
        fib(15,F,_1,_2).
