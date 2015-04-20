:- module(dia_sums_and1p1,[test/0]).

:- use_module(qe_andorra_lib).
:- use_module(library(write),[write/1]).
:- use_module(library(when)).

test:-
	test(_1,_2),
	wakeup(_1,_2).

test(_1,_2) :-
        A=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19],
        member(B,A,C,_1,_3),
        member(D,C,E,_3,_4),
        is(F,38-B-D,_4,_5),
        member(F,E,G,_5,_6),
        <(B,F,_6,_7),
        member(H,G,I,_7,_8),
        is(J,38-B-H,_8,_9),
        member(J,I,K,_9,_10),
        <(B,J,_10,_11),
        <(F,J,_11,_12),
        member(L,K,M,_12,_13),
        member(N,M,O,_13,_14),
        is(P,38-H-L-N,_14,_15),
        member(P,O,Q,_15,_16),
        is(R,38-F-P,_16,_17),
        member(R,Q,S,_17,_18),
        <(B,R,_18,_19),
        member(T,S,U,_19,_20),
        is(V,38-D-L-T,_20,_21),
        member(V,U,W,_21,_22),
        is(X,38-J-V,_22,_23),
        member(X,W,Y,_23,_24),
        <(B,X,_24,_25),
        member(Z,Y,A1,_25,_26),
        is(B1,38-F-N-Z-X,_26,_27),
        member(B1,A1,C1,_27,_28),
        is(D1,38-J-T-Z-R,_28,_29),
        member(D1,C1,E1,_29,_30),
        is(F1,38-D-N-D1,_30,_31),
        member(F1,E1,G1,_31,_32),
        is(H1,38-R-F1,_32,_33),
        member(H1,G1,I1,_33,_34),
        <(B,H1,_34,_35),
        is(J1,38-X-H1,_35,_36),
        member(J1,I1,K1,_36,_37),
        is(J1,38-H-T-B1,_37,_38),
        member(L1,K1,[],_38,_39),
        is(L1,38-F1-B1-V,_39,_40),
        is(L1,38-B-L-Z-H1,_40,_41),
        is(L1,38-P-D1-J1,_41,_2),
	wakeup(_1,_2),
        write('The solution is: '),
        write([B,D,F,H,L,N,P,J,T,Z,D1,R,V,B1,L1,F1,X,J1,H1]),
        nl.

member(A,B,C,_4,_5) :-
        _4=[_6|_7],
        member_check(A,B,C,_6,_7,_5).

member_check(A,B,C,_5,_6,_4) :-
        nonvar(_5),
        !,
        member_work(A,B,C,_6,_4).

member_check(A,B,C,_5,_6,_4) :-
        nonvar(A),
        !,
        member_check_1(A,B,C,_5,_6,_4).

member_check(A,B,C,_5,_6,_4) :-
        nonvar(C),
        !,
        (
          \+C=[_2|_3]  ->
             member_work(A,B,C,_6,_4)
        ;
             when((nonvar(A);nonvar(_5)),member_check(A,B,C,_5,_6,_4))
        ).

member_check(A,B,C,_5,_6,_4) :-
        when((nonvar(C);nonvar(A);nonvar(_5)),member_check(A,B,C,_5,_6,_4)).

member_check_1(A,B,C,_5,_6,_4) :-
        nonvar(_5),
        !,
        member_work(A,B,C,_6,_4).

member_check_1(A,B,C,_5,_6,_4) :-
        nonvar(B),
        !,
        (
          B=[H|_1]  ->
             member_check_1(A,B,C,_5,_6,_4,H)
        ;
             member_check_false(A,B,C,_5,_6,_4)
        ).

member_check_1(A,B,C,_5,_6,_4) :-
        nonvar(C),
        !,
        (
          \+C=[_2|_3]  ->
             member_work(A,B,C,_6,_4)
        ;
             when((nonvar(B);nonvar(_5)),member_check_1(A,B,C,_5,_6,_4))
        ).

member_check_1(A,B,C,_5,_6,_4) :-
        when((nonvar(C);nonvar(B);nonvar(_5)),member_check_1(A,B,C,_5,_6,_4)).

member_check_1(A,B,C,_5,_6,_4,H) :-
        nonvar(_5),
        !,
        member_work(A,B,C,_6,_4).

member_check_1(A,B,C,_5,_6,_4,H) :-
        nonvar(H),
        !,
        (
          \+H=A  ->
             member_work(A,B,C,_6,_4)
        ;
             (
               H==A  ->
                  member_check_false(A,B,C,_5,_6,_4)
             ;
                  suspend_on_vars([H,A,_5],_7),
                  when(nonvar(_7),member_check_1(A,B,C,_5,_6,_4,H))
             )
        ).

member_check_1(A,B,C,_5,_6,_4,H) :-
        nonvar(C),
        !,
        (
          \+C=[_2|_3]  ->
             member_work(A,B,C,_6,_4)
        ;
             when((nonvar(H);nonvar(_5)),member_check_1(A,B,C,_5,_6,_4,H))
        ).

member_check_1(A,B,C,_5,_6,_4,H) :-
        when((nonvar(C);nonvar(H);nonvar(_5)),member_check_1(A,B,C,_5,_6,_4,H)).

member_check_false(A,B,C,_5,_6,_4) :-
        nonvar(_5),
        !,
        member_work(A,B,C,_6,_4).

member_check_false(A,B,C,_5,_6,_4) :-
        nonvar(C),
        !,
        (
          \+C=[_2|_3]  ->
             member_work(A,B,C,_6,_4)
        ;
             when(nonvar(_5),member_work(A,B,C,_6,_4))
        ).

member_check_false(A,B,C,_5,_6,_4) :-
        when((nonvar(C);nonvar(_5)),member_check_false(A,B,C,_5,_6,_4)).

member_work(A,[A|B],B,_1,_1).

member_work(A,[B|C],[B|D],_1,_2) :-
        \==(A,B,_1,_3),
        member(A,C,D,_3,_2).

ourmain(_3,_4) :-
        statistics(runtime,_1),
        ourdo(_3,_4),
        statistics(runtime,[_2,T1]),
        write(T1).

ourdo(_1,_2) :-
        test(_1,_2),
        fail.

ourdo(_1,_1).
