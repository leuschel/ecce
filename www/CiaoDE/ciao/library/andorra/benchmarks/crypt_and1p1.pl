:- module(crypt_and1p1,[test/0,go/0],_).

:- use_module(qe_andorra_lib).
:- use_module(library(write),[write/1]).
:- use_module(library(read),[read/1]).
:- use_module(library(when)).

test:- 
	test(_1,_2),
	wakeup(_1,_2).

go:- 
	go(_1,_2),
	wakeup(_1,_2).

test(_1,_2) :-
        solve(A,B,C,D,E,F,G,H,_1,_3),
	wakeup(_1,_3),
        print_out(A,B,C,D,E,F,G,H,_3,_2).

go(_1,_2) :-
        solve(S,E,N,D,M,O,R,Y,_1,_2).

go(fn(S,E,N,D,M,O,R,Y),_1,_2) :-
        solve(S,E,N,D,M,O,R,Y,_1,_3),
	wakeup(_1,_3),
        print_out(S,E,N,D,M,O,R,Y,_3,_2).

solve(S,E,N,D,M,O,R,Y,_1,_2) :-
        M=1,
        carry(C1,C2,C3,_1,_3),
        ganerate(S,E,N,D,M,O,R,Y,_3,_4),
        >(S,0,_4,_5),
        const(0,D,E,Y,C1,_5,_6),
        const(C1,N,R,E,C2,_6,_7),
        const(C2,E,O,N,C3,_7,_8),
        const(C3,S,M,O,M,_8,_2).

const(A,B,C,D,E,_2,_3) :-
        (
          (
               ground(E),
               ground(D),
               ground(B),
               ground(C)
          ;
               (
                    ground(D),
                    ground(E),
                    ground(A),
                    ground(C)
               ;
                    (
                         ground(A),
                         ground(E),
                         ground(D),
                         ground(B)
                    ;
                         (
                              ground(B),
                              ground(E),
                              ground(A),
                              ground(C)
                         ;
                              ground(C),
                              ground(D),
                              ground(A),
                              ground(B)
                         )
                    )
               )
          )  ->
             const_work(A,B,C,D,E,_2,_3)
        ;
             _2=[_1|_4],
             when((nonvar(_1);ground(E),ground(D),ground(B),ground(C);ground(D),ground(E),ground(A),ground(C);ground(A),ground(E),ground(D),ground(B);ground(B),ground(E),ground(A),ground(C);ground(C),ground(D),ground(A),ground(B)),const_work(A,B,C,D,E,_4,_3))
        ).

const_work(A,B,C,D,E,_1,_2) :-
        is(A,10*E+D-B-C,_1,_2),
        !.

const_work(A,B,C,D,E,_1,_2) :-
        is(B,10*E+D-A-C,_1,_2),
        !.

const_work(A,B,C,D,E,_1,_2) :-
        is(C,10*E+D-A-B,_1,_2),
        !.

const_work(A,B,C,D,E,_1,_2) :-
        is(D,C-10*E+A+B,_1,_2),
        !.

const_work(A,B,C,D,E,_1,_2) :-
        is(E,(C-D+A+B)/10,_1,_2).

ganerate(S,E,N,D,M,O,R,Y,_1,_2) :-
        digi(S,1,_1,_3),
        digi(E,2,_3,_4),
        digi(N,3,_4,_5),
        digi(D,4,_5,_6),
        digi(M,5,_6,_7),
        digi(O,6,_7,_8),
        digi(R,7,_8,_9),
        digi(Y,8,_9,_10),
        difflist([S,E,N,D,M,O,R,Y],_10,_2).

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

difflist_work([],_1,_1).

difflist_work([X|T],_1,_2) :-
        not_member(X,T,_1,_3),
        difflist(T,_3,_2).

not_member(A,B,_4,_3) :-
        (
          nonvar(B)  ->
             not_member_work(A,B,_4,_3)
        ;
             _4=[_1|_2],
	     when((nonvar(_1);nonvar(B)),not_member_work(A,B,_2,_3))
%             not_member_susp(A,B,_1,_2,_3)
        ).

% :- block not_member_susp(?,-,-,?,?).

% not_member_susp(A,B,_1,_2,_3) :-
%         not_member_work(A,B,_2,_3).

not_member_work(X,[],_1,_1).

not_member_work(X,[Y|L],_1,_2) :-
        not_same(X,Y,_1,_3),
        not_member(X,L,_3,_2).

not_same(X,Y,_1,_2) :-
        \==(X,Y,_1,_2).

digi(A,B,_2,_3) :-
        (
          nonvar(A)  ->
             digi(A,B),_2=_3
        ;
             _2=[_1|_3],
	     when((nonvar(_1);nonvar(A)),digi(A,B))
%             digi_susp(A,B,_1)
        ).

% :- block digi_susp(-,?,-).

% digi_susp(A,B,_1) :-
%         digi(A,B).

digi(9,X).

digi(8,X).

digi(7,X).

digi(6,X).

digi(5,X).

digi(4,X).

digi(3,X).

digi(2,X).

digi(1,X).

digi(0,X).

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

% :- block carry_susp(-,?,?,-),carry_susp(?,-,?,-),carry_susp(?,?,-,-).

% carry_susp(A,B,C,_1) :-
%         carry(A,B,C).

carry(1,1,1).

carry(1,1,0).

carry(1,0,1).

carry(1,0,0).

carry(0,1,1).

carry(0,1,0).

carry(0,0,1).

carry(0,0,0).

print_out(S,E,N,D,M,O,R,Y,_1,_1) :-
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
        write(S),
        write(E),
        write(N),
        write(D),
        nl,
        write(' + '),
        write(M),
        write(O),
        write(R),
        write(E),
        nl,
        write(-----------),
        nl,
        write('  '),
        write(M),
        write(O),
        write(N),
        write(E),
        write(Y),
        nl.
