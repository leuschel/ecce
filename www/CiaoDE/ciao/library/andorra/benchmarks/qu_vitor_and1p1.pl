:- module(qu_vitor_and1p1,[run/2],_).
:- use_module(qe_andorra_lib).
:- use_module(library(when),[when/2]).
:- use_module(library(prolog_sys),[statistics/2]).
:- use_module(library(write),[write/1]).

run(A,B):-
	run(A,B,_1,_2),
	wakeup(_1,_2).

test(_1,_2) :-
        run(8,A,_1,_2),
        fail.

go(X,_1,_2) :-
        run(8,X,_1,_2).

snint(A,_2,_3) :-
        (
          nonvar(A)  ->
             snint(A),_2=_3
        ;
             _2=[_1|_3],
	     when((nonvar(_1);nonvar(A)),snint(A))
%             snint_susp(A,_1)
        ).

%:- block snint_susp(-,-).

%snint_susp(A,_1) :-
%        snint(A).

snint(1).

snint(2).

snint(3).

snint(4).

snint(5).

snint(6).

snint(7).

snint(8).

size(8).

safe(A,B,C,_4,_3) :-
        (
          nonvar(C)  ->
             safe_work(A,B,C,_4,_3)
        ;
             _4=[_1|_2],
	     when((nonvar(_1);nonvar(C)),safe_work(A,B,C,_2,_3))
%             safe_susp(A,B,C,_1,_2,_3)
        ).

%:- block safe_susp(?,?,-,-,?,?).

%safe_susp(A,B,C,_1,_2,_3) :-
%        safe_work(A,B,C,_2,_3).

safe_work(A,B,[],_1,_1).

safe_work(A,B,[square(C,D)|E],_1,_2) :-
        not_threatened(C,D,A,B,_1,_3),
        safe(A,B,E,_3,_2).

solve(A,B,C,_3,_4) :-
        _3=[_5|_6],
        solve_check(A,B,C,_5,_6,_4).

solve_check(A,B,C,_4,_5,_3) :-
        nonvar(_4),
        !,
        solve_work(A,B,C,_5,_3).

solve_check(A,B,C,_4,_5,_3) :-
        nonvar(B),
        !,
        (
          \+B=[_1|_2]  ->
             solve_work(A,B,C,_5,_3)
        ;
             when(nonvar(_4),solve_work(A,B,C,_5,_3))
        ).

solve_check(A,B,C,_4,_5,_3) :-
        when((nonvar(B);nonvar(_4)),solve_check(A,B,C,_4,_5,_3)).

solve_work(A,[square(8,B)|C],D,_1,_1) :-
        !,
        D=[square(8,B)|C].

solve_work(A,B,C,_1,_2) :-
        newsquare(B,D,_1,_3),
        solve(A,[D|B],C,_3,_2).

not_threatened(A,B,C,D,_1,_2) :-
        \==(B,D,_1,_3),
        is(E,A-B,_3,_4),
        is(F,C-D,_4,_5),
        \==(F,E,_5,_6),
        is(G,A+B,_6,_7),
        is(H,C+D,_7,_8),
        \==(G,H,_8,_2).

get_solutions(A,B,_1,_2) :-
        solve(A,[],B,_1,_2).

run(A,B,_1,_2) :-
        get_solutions(A,B,_1,_2).

newsquare(A,B,_4,_3) :-
        (
          nonvar(A)  ->
             newsquare_work(A,B,_4,_3)
        ;
             _4=[_1|_2],
	     when((nonvar(_1);nonvar(A)),newsquare_work(A,B,_2,_3))
%             newsquare_susp(A,B,_1,_2,_3)
        ).

%:- block newsquare_susp(-,?,-,?,?).

%newsquare_susp(A,B,_1,_2,_3) :-
%        newsquare_work(A,B,_2,_3).

newsquare_work([],square(1,A),_1,_2) :-
        snint(A,_1,_2).

newsquare_work([square(A,B)|C],square(D,E),_1,_2) :-
        is(D,A+1,_1,_3),
        snint(E,_3,_4),
        not_threatened(A,B,D,E,_4,_5),
        safe(D,E,C,_5,_2).

ourmain(_3,_4) :-
        statistics(runtime,_1),
        ourdo(_3,_4),
        statistics(runtime,[_2,T1]),
        write(T1).

ourdo(_2,_3) :-
        run(8,_1,_2,_3),
        fail.

ourdo(_1,_1).
