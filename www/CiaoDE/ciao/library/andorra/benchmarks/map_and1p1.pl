:- module(map_and1p1,[test/1,test_all/1],_).
:- use_module(qe_andorra_lib).
:- use_module(library(when),[when/2]).
:- use_module(library(prolog_sys),[statistics/2]).
:- use_module(library(write),[write/1]).

test(A):-
	test(A,_1,_2),
	wakeup(_1,_2).

test_all(A):-
	test_all(A,_1,_2),
	wakeup(_1,_2).


test_all(A,_4,_3) :-
        (
          nonvar(A)  ->
             test_all_work(A,_4,_3)
        ;
             _4=[_1|_2],
	     when((nonvar(A);nonvar(_1)),test_all_work(A,_2,_3))
%             test_all_susp(A,_1,_2,_3)
        ).

%:- block test_all_susp(-,-,?,?).

%test_all_susp(A,_1,_2,_3) :-
%        test_all_work(A,_2,_3).

test_all_work(A,_1,_2) :-
        >(A,0,_1,_3),
        goal_all(B,C,D,E,F,G,_3,_4),
        is(H,A-1,_4,_5),
        test_all(H,_5,_2).

test_all_work(0,_1,_1).

next(A,B,_2,_3) :-
        (
          nonvar(A),
          nonvar(B)  ->
             next(A,B),_2=_3
        ;
             _2=[_1|_3],
	     when((nonvar(_1);(nonvar(A),nonvar(B))),next(A,B))
%             next_susp(A,B,_1)
        ).

%:- block next_susp(-,?,-),next_susp(?,-,-).

%next_susp(A,B,_1) :-
%        next(A,B).

next(blue,yellow).

next(blue,red).

next(blue,green).

next(yellow,blue).

next(yellow,red).

next(yellow,green).

next(red,blue).

next(red,yellow).

next(red,green).

next(green,blue).

next(green,yellow).

next(green,red).

test(A,_4,_3) :-
        (
          nonvar(A)  ->
             test_work(A,_4,_3)
        ;
             _4=[_1|_2],
	     when((nonvar(_1);nonvar(A)),test_work(A,_2,_3))
%             test_susp(A,_1,_2,_3)
        ).

%:- block test_susp(-,-,?,?).

%test_susp(A,_1,_2,_3) :-
%        test_work(A,_2,_3).

test_work(A,_1,_2) :-
        >(A,0,_1,_3),
        goal(B,C,D,E,F,G,_3,_4),
        is(H,A-1,_4,_5),
        test(H,_5,_2).

test_work(0,_1,_1).

goal(A,B,C,D,E,F,_1,_2) :-
        next(A,B,_1,_3),
        next(A,C,_3,_4),
        next(A,E,_4,_5),
        next(A,F,_5,_6),
        next(B,C,_6,_7),
        next(B,D,_7,_8),
        next(B,E,_8,_9),
        next(B,F,_9,_10),
        next(C,D,_10,_11),
        next(C,F,_11,_12),
        next(E,F,_12,_2).

goal_all(_1,_2,_3,_4,_5,_6,[_8|_9],_7) :-
        when(nonvar(_8),goal_all_work(_1,_2,_3,_4,_5,_6,_9,_7)).
%        goal_all_susp(_1,_2,_3,_4,_5,_6,_8,_9,_7).

%:- block goal_all_susp(?,?,?,?,?,?,-,?,?).

%goal_all_susp(_1,_2,_3,_4,_5,_6,_8,_9,_7) :-
%        goal_all_work(_1,_2,_3,_4,_5,_6,_9,_7).

goal_all_work(A,B,C,D,E,F,_1,_2) :-
        next(A,B,_1,_3),
        next(A,C,_3,_4),
        next(A,E,_4,_5),
        next(A,F,_5,_6),
        next(B,C,_6,_7),
        next(B,D,_7,_8),
        next(B,E,_8,_9),
        next(B,F,_9,_10),
        next(C,D,_10,_11),
        next(C,F,_11,_12),
        next(E,F,_12,_2),
        fail.

goal_all_work(A,B,C,D,E,F,_1,_1).

go(_1,_2) :-
        goal_all(A,B,C,D,E,F,_1,_2).

test(_1,_2) :-
        goal(A,B,C,D,E,F,_1,_2).

ourmain(_3,_4) :-
        statistics(runtime,_1),
        ourdo(_3,_4),
        statistics(runtime,[_2,T1]),
        write(T1).

ourdo(_1,_2) :-
        test(_1,_2),
        fail.

ourdo(_1,_1).
