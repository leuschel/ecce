:- module(qu_evan_and1p1,[test/0,queen/2],_).
:- use_module(qe_andorra_lib).
:- use_module(library(when),[when/2]).
:- use_module(library(prolog_sys),[statistics/2]).
:- use_module(library(write),[write/1]).

test :-
	test(_1,_2),
	wakeup(_1,_2).

test(_1,_2) :-
        queen(8,G,_1,_2),
        fail.

queen(N,A):-
	queen(N,A,_1,_2),
	wakeup(_1,_2).

queen(N,A,_1,_2) :-
        gen(N,L,_1,_3),
        queen2(L,[],A,_3,_2).

queen2(A,B,C,_1,_2) :-
        _1=[_3|_4],
        queen2_check(A,B,C,_3,_4,_2).

queen2_check(A,B,C,_2,_3,_1) :-
        nonvar(_2),
        !,
        queen2_work(A,B,C,_3,_1).

queen2_check(A,B,C,_2,_3,_1) :-
        nonvar(A),
        !,
        queen2_work(A,B,C,_3,_1).

queen2_check(A,B,C,_2,_3,_1) :-
        nonvar(B),
        !,
        queen2_check_1(A,B,C,_2,_3,_1).

queen2_check(A,B,C,_2,_3,_1) :-
        when((nonvar(B);nonvar(A);nonvar(_2)),queen2_check(A,B,C,_2,_3,_1)).

queen2_check_1(A,B,C,_2,_3,_1) :-
        nonvar(_2),
        !,
        queen2_work(A,B,C,_3,_1).

queen2_check_1(A,B,C,_2,_3,_1) :-
        nonvar(A),
        !,
        queen2_work(A,B,C,_3,_1).



queen2_check_1(A,B,C,_2,_3,_1) :-
        nonvar(C),
        !,
        (
          \+B=C  ->
             queen2_work(A,B,C,_3,_1)
        ;
             (
               B==C  ->
                  when(nonvar(_2),queen2_work(A,B,C,_3,_1))
             ;
%                  suspend_on_vars([B,C,_2],_4),
                  suspend_on_vars([A,B,C,_2],_4),
                  when(nonvar(_4),queen2_check_1(A,B,C,_2,_3,_1))
             )
        ).

queen2_check_1(A,B,C,_2,_3,_1) :-
        when((nonvar(A);nonvar(C);nonvar(_2)),queen2_check_1(A,B,C,_2,_3,_1)).

% queen2_check_1(A,B,C,_2,_3,_1) :-
%         when((nonvar(C);nonvar(_2)),queen2_check_1(A,B,C,_2,_3,_1)).


queen2_work([],R,R,_1,_1).

queen2_work([H|T],R,P,_1,_2) :-
        del(A,[H|T],L,_1,_3),
        safe(R,A,1,_3,_4),
        queen2(L,[A|R],P,_4,_2).

safe(A,B,C,_4,_3) :-
        (
          nonvar(A)  ->
             safe_work(A,B,C,_4,_3)
        ;
             _4=[_1|_2],
	     when((nonvar(_1);nonvar(A)),safe_work(A,B,C,_2,_3))
%             safe_susp(A,B,C,_1,_2,_3)
        ).

%:- block safe_susp(-,?,?,-,?,?).

%safe_susp(A,B,C,_1,_2,_3) :-
%        safe_work(A,B,C,_2,_3).

safe_work([],_1,_2,_3,_3).

safe_work([H|T],U,D,_1,_2) :-
        is(G,H+D,_1,_3),
        \==(G,U,_3,_4),
        is(F,H-D,_4,_5),
        \==(F,U,_5,_6),
        is(D1,D+1,_6,_7),
        safe(T,U,D1,_7,_2).

del(A,B,C,_4,_5) :-
        _4=[_6|_7],
        del_check(A,B,C,_6,_7,_5).

del_check(A,B,C,_5,_6,_4) :-
        nonvar(_5),
        !,
        del_work(A,B,C,_6,_4).

del_check(A,B,C,_5,_6,_4) :-
        nonvar(A),
        !,
        del_check_1(A,B,C,_5,_6,_4).

del_check(A,B,C,_5,_6,_4) :-
        nonvar(C),
        !,
        (
          \+C=[_2|_3]  ->
             del_work(A,B,C,_6,_4)
        ;
             when((nonvar(A);nonvar(_5)),del_check(A,B,C,_5,_6,_4))
        ).

del_check(A,B,C,_5,_6,_4) :-
        when((nonvar(C);nonvar(A);nonvar(_5)),del_check(A,B,C,_5,_6,_4)).

del_check_1(A,B,C,_5,_6,_4) :-
        nonvar(_5),
        !,
        del_work(A,B,C,_6,_4).

del_check_1(A,B,C,_5,_6,_4) :-
        nonvar(B),
        !,
        (
          B=[H|_1]  ->
             del_check_1(A,B,C,_5,_6,_4,H)
        ;
             del_check_false(A,B,C,_5,_6,_4)
        ).

del_check_1(A,B,C,_5,_6,_4) :-
        nonvar(C),
        !,
        (
          \+C=[_2|_3]  ->
             del_work(A,B,C,_6,_4)
        ;
             when((nonvar(B);nonvar(_5)),del_check_1(A,B,C,_5,_6,_4))
        ).

del_check_1(A,B,C,_5,_6,_4) :-
        when((nonvar(C);nonvar(B);nonvar(_5)),del_check_1(A,B,C,_5,_6,_4)).

del_check_1(A,B,C,_5,_6,_4,H) :-
        nonvar(_5),
        !,
        del_work(A,B,C,_6,_4).


del_check_1(A,B,C,_5,_6,_4,H) :-
        nonvar(C),
        !,
        (
          \+C=[_2|_3]  ->
             del_work(A,B,C,_6,_4)
        ;
             when((nonvar(H);nonvar(_5)),del_check_1(A,B,C,_5,_6,_4,H))
        ).

del_check_1(A,B,C,_5,_6,_4,H) :-
        nonvar(H),
        !,
        (
          \+H=A  ->
             del_work(A,B,C,_6,_4)
        ;
             (
               H==A  ->
                  del_check_false(A,B,C,_5,_6,_4)
             ;
%                  suspend_on_vars([H,A,_5],_7),
                  suspend_on_vars([C,H,A,_5],_7),
                  when(nonvar(_7),del_check_1(A,B,C,_5,_6,_4,H))
             )
        ).



del_check_1(A,B,C,_5,_6,_4,H) :-
        when((nonvar(C);nonvar(H);nonvar(_5)),del_check_1(A,B,C,_5,_6,_4,H)).

del_check_false(A,B,C,_5,_6,_4) :-
        nonvar(_5),
        !,
        del_work(A,B,C,_6,_4).

del_check_false(A,B,C,_5,_6,_4) :-
        nonvar(C),
        !,
        (
          \+C=[_2|_3]  ->
             del_work(A,B,C,_6,_4)
        ;
             when(nonvar(_5),del_work(A,B,C,_6,_4))
        ).

del_check_false(A,B,C,_5,_6,_4) :-
        when((nonvar(C);nonvar(_5)),del_check_false(A,B,C,_5,_6,_4)).

del_work(X,[X|Y],Y,_1,_1).

del_work(X,[Y|Z],[Y|W],_1,_2) :-
        del(X,Z,W,_1,_2).

gen(A,B,_4,_3) :-
        (
          (
               nonvar(A)
          ;
               nonvar(B)
          )  ->
             gen_work(A,B,_4,_3)
        ;
             _4=[_1|_2],
	     when((nonvar(_1);nonvar(A);nonvar(B)),gen_work(A,B,_2,_3))
%             gen_susp(A,B,_1,_2,_3)
        ).

%:- block gen_susp(-,-,-,?,?).

%gen_susp(A,B,_1,_2,_3) :-
%        gen_work(A,B,_2,_3).

gen_work(0,[],_1,_1) :- !.

gen_work(N,[N|X],_1,_2) :-
        is(M,N-1,_1,_3),
        gen(M,X,_3,_2).

ourmain(_3,_4) :-
        statistics(runtime,_1),
        ourdo(_3,_4),
        statistics(runtime,[_2,T1]),
        write(T1).

ourdo(_2,_3) :-
        queen(8,_1,_2,_3),
        fail.

ourdo(_1,_1).
