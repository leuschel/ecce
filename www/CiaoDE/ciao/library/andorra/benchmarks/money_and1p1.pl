:- module(money_and1p1,[test/0],_).

:- use_module(qe_andorra_lib).
:- use_module(library(write),[write/1]).
:- use_module(library(when)).
:- use_module(library(prolog_sys),[statistics/2]).

test:- 
	test(_1,_2),
	wakeup(_1,_2).

test(_1,_2) :-
        solve(A,B,C,D,E,F,G,H,_1,_3),
	wakeup(_1,_3),
        print_out(A,B,C,D,E,F,G,H,_3,_2).

do(_1,_2) :-
        solve(A,B,C,D,E,F,G,H,_1,_2).

%difflist_susp(A,_1,_2,_3) :-
%        difflist_work(A,_2,_3).

difflist(A,_4,_3) :-
        (
          nonvar(A)  ->
             difflist_work(A,_4,_3)
        ;
             _4=[_1|_2],
	     when((nonvar(_1);nonvar(A)),difflist_work(A,_2,_3))
%             difflist_susp(A,_1,_2,_3)
        ).

% :- block difflist_susp(-,-,?,?).


% difflist_susp(A,_1,_2,_3) :-
%         difflist_work(A,_2,_3).

difflist_work([A|B],_1,_2) :-
        not_el(A,B,_1,_3),
        difflist(B,_3,_2).

difflist_work([],_1,_1).

digi(A,_2,_3) :-
        (
          nonvar(A)  ->
             digi(A),_2=_3
        ;
             _2=[_1|_3],
	     when((nonvar(_1);nonvar(A)),digi(A))
%             digi_susp(A,_1)
        ).

%:- block digi_susp(-,-).

%digi_susp(A,_1) :-
%        digi(A).

digi(9).

digi(8).

digi(7).

digi(6).

digi(5).

digi(4).

digi(3).

digi(2).

digi(1).

digi(0).

go(fn(A,B,C,D,E,F,G,H),_1,_2) :-
        solve(A,B,C,D,E,F,G,H,_1,_3),
	wakeup(_1,_3),
        print_out(A,B,C,D,E,F,G,H,_3,_2).

%go_with_trace(A,_1,_2) :-
%        trace(_1,_3),
%        go(A,_3,_4),
%        no_trace(_4,_2).

not_el(X,A,_4,_3) :-
        (
          nonvar(A)  ->
             not_el_work(X,A,_4,_3)
        ;
             _4=[_1|_2],
	     when((nonvar(_1);nonvar(A)),not_el_work(X,A,_2,_3))
%             not_el_susp(X,A,_1,_2,_3)
        ).

%:- block not_el_susp(?,-,-,?,?).

%not_el_susp(X,A,_1,_2,_3) :-
%        not_el_work(X,A,_2,_3).

not_el_work(A,[],_1,_1).

not_el_work(A,[B|C],_1,_2) :-
        \==(A,B,_1,_3),
        not_el(A,C,_3,_2).

carry(A,B,C,_2,_3) :-
        (
          nonvar(A),
          nonvar(B),
          nonvar(C)  ->
             carry(A,B,C),_2=_3
        ;
             _2=[_1|_3],
	     when((nonvar(_1);nonvar(A),nonvar(B),nonvar(C)),carry(A,B,C))
%             carry_susp(A,B,C,_1)
        ).

%:- block carry_susp(-,?,?,-),carry_susp(?,-,?,-),carry_susp(?,?,-,-).

%carry_susp(A,B,C,_1) :-
%        carry(A,B,C).

carry(1,1,1).

carry(1,1,0).

carry(1,0,1).

carry(1,0,0).

carry(0,1,1).

carry(0,1,0).

carry(0,0,1).

carry(0,0,0).

generate(A,B,C,D,E,F,G,H,_1,_2) :-
        digi(E,_1,_3),
        digi(A,_3,_4),
        digi(F,_4,_5),
        digi(B,_5,_6),
        digi(C,_6,_7),
        digi(G,_7,_8),
        digi(D,_8,_9),
        digi(H,_9,_10),
        difflist([A,B,C,D,E,F,G,H],_10,_2).

print_out(A,B,C,D,E,F,G,H,_1,_1) :-
        write('    SEND'),
        nl,
        write(' +  MORE'),
        nl,
        write(-----------),
        nl,
        write('   MONEY'),
        nl,
        nl,
        write('The solution is:'),
        nl,
        nl,
        write('   '),
        write(A),
        write(B),
        write(C),
        write(D),
        nl,
        write(' + '),
        write(E),
        write(F),
        write(G),
        write(B),
        nl,
        write(-----------),
        nl,
        write('  '),
        write(E),
        write(F),
        write(C),
        write(B),
        write(H),
        nl.

solve(A,B,C,D,E,F,G,H,_1,_2) :-
        carry(I,J,K,_1,_3),
        E=1,
        generate(A,B,C,D,E,F,G,H,_3,_4),
        >(A,0,_4,_5),
        is(L,D+B,_5,_6),
        is(M,H+10*I,_6,_7),
        L=M,
        is(N,I+C+G,_7,_8),
        is(O,B+10*J,_8,_9),
        N=O,
        is(P,J+B+F,_9,_10),
        is(Q,C+10*K,_10,_11),
        P=Q,
        is(R,K+A+E,_11,_12),
        is(S,F+10*E,_12,_2),
        R=S.

ourmain(_3,_4) :-
        statistics(runtime,_1),
        ourdo(_3,_4),
        statistics(runtime,[_2,T1]),
        write(T1).

ourdo(_1,_2) :-
        solve(_A,_B,_C,_D,_E,_F,_G,_H,_1,_2),
        fail.

ourdo(_1,_1).
