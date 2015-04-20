:- module(mqu_and1p1,[queens/1],_).
:- use_module(qe_andorra_lib).
:- use_module(library(when),[when/2]).
:- use_module(library(prolog_sys),[statistics/2]).
:- use_module(library(write),[write/1]).

queens(A) :-
	queens(A,_1,_2),
	wakeup(_1,_2).

queens(A,_1,_2) :-
        queens(A,A,A,B,_1,_3),
	wakeup(_1,_3),
        nl,
        displ(B,_3,_2).

zero(A,_4,_3) :-
        (
          nonvar(A)  ->
             zero_work(A,_4,_3)
        ;
             _4=[_1|_2],
	     when((nonvar(_1);nonvar(A)),zero_work(A,_2,_3))
%             zero_susp(A,_1,_2,_3)
        ).

%:- block zero_susp(-,-,?,?).

%zero_susp(A,_1,_2,_3) :-
%        zero_work(A,_2,_3).

zero_work([],_1,_1).

zero_work([0|A],_1,_2) :-
        zero(A,_1,_2).

adddig([A|B],C,[[A]|D],_1,_2) :-
        add(B,C,D,_1,_2).

place(A,B,_3,_4) :-
        _3=[_5|_6],
        place_check(A,B,_5,_6,_4).

place_check(A,B,_4,_5,_3) :-
        nonvar(_4),
        !,
        place_work(A,B,_5,_3).

place_check(A,B,_4,_5,_3) :-
        nonvar(A),
        !,
        (
          A=0  ->
             place_work(A,B,_5,_3)
        ;
             place_check_false(A,B,_4,_5,_3)
        ).

place_check(A,B,_4,_5,_3) :-
        nonvar(A),
        !,
        (
          A=s(_1)  ->
             place_check(A,B,_4,_5,_3,X,_2)
        ;
             when((nonvar(A);nonvar(_4)),place_check(A,B,_4,_5,_3))
        ).

place_check(A,B,_4,_5,_3) :-
        when((nonvar(A);nonvar(A);nonvar(_4)),place_check(A,B,_4,_5,_3)).

place_check(A,B,_4,_5,_3,X,_2) :-
        nonvar(_4),
        !,
        place_work(A,B,_5,_3).

place_check(A,B,_4,_5,_3,X,_2) :-
        nonvar(B),
        !,
        (
          B=[X|_2]  ->
             place_check_1(A,B,_4,_5,_3,X,_2)
        ;
             when(nonvar(_4),place_work(A,B,_5,_3))
        ).

place_check(A,B,_4,_5,_3,X,_2) :-
        when((nonvar(B);nonvar(_4)),place_check(A,B,_4,_5,_3,X,_2)).

place_check_1(A,B,_4,_5,_3,X,_2) :-
        nonvar(_4),
        !,
        place_work(A,B,_5,_3).

place_check_1(A,B,_4,_5,_3,X,_2) :-
        nonvar(X),
        !,
        place_work(A,B,_5,_3).

place_check_1(A,B,_4,_5,_3,X,_2) :-
        when((nonvar(X);nonvar(_4)),place_check_1(A,B,_4,_5,_3,X,_2)).

place_check_false(A,B,_4,_5,_3) :-
        nonvar(_4),
        !,
        place_work(A,B,_5,_3).

place_check_false(A,B,_4,_5,_3) :-
        nonvar(A),
        !,
        (
          A=s(_1)  ->
             place_check_false(A,B,_4,_5,_3,X,_2)
        ;
             when(nonvar(_4),place_work(A,B,_5,_3))
        ).

place_check_false(A,B,_4,_5,_3) :-
        when((nonvar(A);nonvar(_4)),place_check_false(A,B,_4,_5,_3)).

place_check_false(A,B,_4,_5,_3,X,_2) :-
        nonvar(_4),
        !,
        place_work(A,B,_5,_3).

place_check_false(A,B,_4,_5,_3,X,_2) :-
        nonvar(B),
        !,
        (
          B=[X|_2]  ->
             place_check_false_1(A,B,_4,_5,_3,X,_2)
        ;
             when(nonvar(_4),place_work(A,B,_5,_3))
        ).

place_check_false(A,B,_4,_5,_3,X,_2) :-
        when((nonvar(B);nonvar(_4)),place_check_false(A,B,_4,_5,_3,X,_2)).

place_check_false_1(A,B,_4,_5,_3,X,_2) :-
        nonvar(_4),
        !,
        place_work(A,B,_5,_3).

place_check_false_1(A,B,_4,_5,_3,X,_2) :-
        nonvar(X),
        !,
        place_work(A,B,_5,_3).

place_check_false_1(A,B,_4,_5,_3,X,_2) :-
        when((nonvar(X);nonvar(_4)),place_check_false_1(A,B,_4,_5,_3,X,_2)).

place_work(0,A,_1,_2) :-
        zero(A,_1,_2).

place_work(s(A),[q|B],_1,_2) :-
        place(A,B,_1,_2).

place_work(s(A),[0|B],_1,_2) :-
        place(s(A),B,_1,_2).

app(A,B,C,_3,_4) :-
        _3=[_5|_6],
        app_check(A,B,C,_5,_6,_4).

app_check(A,B,C,_4,_5,_3) :-
        nonvar(_4),
        !,
        app_work(A,B,C,_5,_3).

app_check(A,B,C,_4,_5,_3) :-
        nonvar(A),
        !,
        app_work(A,B,C,_5,_3).

app_check(A,B,C,_4,_5,_3) :-
        nonvar(B),
        !,
        app_check_1(A,B,C,_4,_5,_3).

app_check(A,B,C,_4,_5,_3) :-
        nonvar(C),
        !,
        (
          \+C=[_1|_2]  ->
             app_work(A,B,C,_5,_3)
        ;
             when((nonvar(B);nonvar(A);nonvar(_4)),app_check(A,B,C,_4,_5,_3))
        ).

app_check(A,B,C,_4,_5,_3) :-
        when((nonvar(C);nonvar(B);nonvar(A);nonvar(_4)),app_check(A,B,C,_4,_5,_3)).

app_check_1(A,B,C,_4,_5,_3) :-
        nonvar(_4),
        !,
        app_work(A,B,C,_5,_3).

app_check_1(A,B,C,_4,_5,_3) :-
        nonvar(C),
        !,
        (
          \+B=C  ->
             app_work(A,B,C,_5,_3)
        ;
             (
               B==C  ->
                  app_check_false_1(A,B,C,_4,_5,_3)
             ;
                  suspend_on_vars([B,C,_4],_6),
                  when(nonvar(_6),app_check_1(A,B,C,_4,_5,_3))
             )
        ).

app_check_1(A,B,C,_4,_5,_3) :-
        nonvar(C),
        !,
        (
          \+C=[_1|_2]  ->
             app_work(A,B,C,_5,_3)
        ;
             when((nonvar(C);nonvar(_4)),app_check_1(A,B,C,_4,_5,_3))
        ).

app_check_1(A,B,C,_4,_5,_3) :-
        when((nonvar(C);nonvar(C);nonvar(_4)),app_check_1(A,B,C,_4,_5,_3)).

app_check_false_1(A,B,C,_4,_5,_3) :-
        nonvar(_4),
        !,
        app_work(A,B,C,_5,_3).

app_check_false_1(A,B,C,_4,_5,_3) :-
        nonvar(C),
        !,
        (
          \+C=[_1|_2]  ->
             app_work(A,B,C,_5,_3)
        ;
             when(nonvar(_4),app_work(A,B,C,_5,_3))
        ).

app_check_false_1(A,B,C,_4,_5,_3) :-
        when((nonvar(C);nonvar(_4)),app_check_false_1(A,B,C,_4,_5,_3)).

app_work([],A,A,_1,_1).

app_work([A|B],C,[A|D],_1,_2) :-
        app(B,C,D,_1,_2).

seed(A,B,_4,_3) :-
        (
          (
               nonvar(A)
          ;
               nonvar(B)
          )  ->
             seed_work(A,B,_4,_3)
        ;
             _4=[_1|_2],
	     when((nonvar(_1);nonvar(A);nonvar(B)),seed_work(A,B,_2,_3))
%             seed_susp(A,B,_1,_2,_3)
        ).

%:- block seed_susp(-,-,-,?,?).

%seed_susp(A,B,_1,_2,_3) :-
%        seed_work(A,B,_2,_3).

seed_work(0,[],_1,_1).

seed_work(s(A),[[]|B],_1,_2) :-
        seed(A,B,_1,_2).

displ(A,_4,_3) :-
        (
          nonvar(A)  ->
             displ_work(A,_4,_3)
        ;
             _4=[_1|_2],
	     when((nonvar(_1);nonvar(A)),displ_work(A,_2,_3))
%             displ_susp(A,_1,_2,_3)
        ).

%:- block displ_susp(-,-,?,?).

%displ_susp(A,_1,_2,_3) :-
%       displ_work(A,_2,_3).

displ_work([],_1,_1).

displ_work([A|B],_1,_2) :-
        write(A),
        nl,
        displ(B,_1,_2).

board(A,B,C,D,E,F,G,_2,_3) :-
        _2=[_4|_5],
        board_check(A,B,C,D,E,F,G,_4,_5,_3).

board_check(A,B,C,D,E,F,G,_3,_4,_2) :-
        nonvar(_3),
        !,
        board_work(A,B,C,D,E,F,G,_4,_2).

board_check(A,B,C,D,E,F,G,_3,_4,_2) :-
        nonvar(A),
        !,
        board_work(A,B,C,D,E,F,G,_4,_2).

board_check(A,B,C,D,E,F,G,_3,_4,_2) :-
        nonvar(B),
        !,
        (
          \+B=s(_1)  ->
             board_work(A,B,C,D,E,F,G,_4,_2)
        ;
             board_check_false_1(A,B,C,D,E,F,G,_3,_4,_2)
        ).

board_check(A,B,C,D,E,F,G,_3,_4,_2) :-
        nonvar(C),
        !,
        (
          \+C=[]  ->
             board_work(A,B,C,D,E,F,G,_4,_2)
        ;
             board_check_false_2(A,B,C,D,E,F,G,_3,_4,_2)
        ).

board_check(A,B,C,D,E,F,G,_3,_4,_2) :-
        nonvar(D),
        !,
        board_work(A,B,C,D,E,F,G,_4,_2).

board_check(A,B,C,D,E,F,G,_3,_4,_2) :-
        nonvar(F),
        !,
        board_check_1(A,B,C,D,E,F,G,_3,_4,_2).

board_check(A,B,C,D,E,F,G,_3,_4,_2) :-
        when((nonvar(F);nonvar(D);nonvar(C);nonvar(B);nonvar(A);nonvar(_3)),board_check(A,B,C,D,E,F,G,_3,_4,_2)).

board_check_1(A,B,C,D,E,F,G,_3,_4,_2) :-
        nonvar(_3),
        !,
        board_work(A,B,C,D,E,F,G,_4,_2).

board_check_1(A,B,C,D,E,F,G,_3,_4,_2) :-
        nonvar(G),
        !,
        (
          \+F=G  ->
             board_work(A,B,C,D,E,F,G,_4,_2)
        ;
             (
               F==G  ->
                  when(nonvar(_3),board_work(A,B,C,D,E,F,G,_4,_2))
             ;
                  suspend_on_vars([F,G,_3],_5),
                  when(nonvar(_5),board_check_1(A,B,C,D,E,F,G,_3,_4,_2))
             )
        ).

board_check_1(A,B,C,D,E,F,G,_3,_4,_2) :-
        when((nonvar(G);nonvar(_3)),board_check_1(A,B,C,D,E,F,G,_3,_4,_2)).

board_check_false_1(A,B,C,D,E,F,G,_3,_4,_2) :-
        nonvar(_3),
        !,
        board_work(A,B,C,D,E,F,G,_4,_2).

board_check_false_1(A,B,C,D,E,F,G,_3,_4,_2) :-
        nonvar(C),
        !,
        (
          \+C=[]  ->
             board_work(A,B,C,D,E,F,G,_4,_2)
        ;
             board_check_false_2(A,B,C,D,E,F,G,_3,_4,_2)
        ).

board_check_false_1(A,B,C,D,E,F,G,_3,_4,_2) :-
        nonvar(D),
        !,
        board_work(A,B,C,D,E,F,G,_4,_2).

board_check_false_1(A,B,C,D,E,F,G,_3,_4,_2) :-
        nonvar(F),
        !,
        board_check_false_1_1(A,B,C,D,E,F,G,_3,_4,_2).

board_check_false_1(A,B,C,D,E,F,G,_3,_4,_2) :-
        when((nonvar(F);nonvar(D);nonvar(C);nonvar(_3)),board_check_false_1(A,B,C,D,E,F,G,_3,_4,_2)).

board_check_false_1_1(A,B,C,D,E,F,G,_3,_4,_2) :-
        nonvar(_3),
        !,
        board_work(A,B,C,D,E,F,G,_4,_2).

board_check_false_1_1(A,B,C,D,E,F,G,_3,_4,_2) :-
        nonvar(G),
        !,
        (
          \+F=G  ->
             board_work(A,B,C,D,E,F,G,_4,_2)
        ;
             (
               F==G  ->
                  when(nonvar(_3),board_work(A,B,C,D,E,F,G,_4,_2))
             ;
                  suspend_on_vars([F,G,_3],_5),
                  when(nonvar(_5),board_check_false_1_1(A,B,C,D,E,F,G,_3,_4,_2))
             )
        ).

board_check_false_1_1(A,B,C,D,E,F,G,_3,_4,_2) :-
        when((nonvar(G);nonvar(_3)),board_check_false_1_1(A,B,C,D,E,F,G,_3,_4,_2)).

board_check_false_2(A,B,C,D,E,F,G,_3,_4,_2) :-
        nonvar(_3),
        !,
        board_work(A,B,C,D,E,F,G,_4,_2).

board_check_false_2(A,B,C,D,E,F,G,_3,_4,_2) :-
        nonvar(D),
        !,
        board_work(A,B,C,D,E,F,G,_4,_2).

board_check_false_2(A,B,C,D,E,F,G,_3,_4,_2) :-
        nonvar(F),
        !,
        board_check_false_2_1(A,B,C,D,E,F,G,_3,_4,_2).

board_check_false_2(A,B,C,D,E,F,G,_3,_4,_2) :-
        when((nonvar(F);nonvar(D);nonvar(_3)),board_check_false_2(A,B,C,D,E,F,G,_3,_4,_2)).

board_check_false_2_1(A,B,C,D,E,F,G,_3,_4,_2) :-
        nonvar(_3),
        !,
        board_work(A,B,C,D,E,F,G,_4,_2).

board_check_false_2_1(A,B,C,D,E,F,G,_3,_4,_2) :-
        nonvar(G),
        !,
        (
          \+F=G  ->
             board_work(A,B,C,D,E,F,G,_4,_2)
        ;
             (
               F==G  ->
                  when(nonvar(_3),board_work(A,B,C,D,E,F,G,_4,_2))
             ;
                  suspend_on_vars([F,G,_3],_5),
                  when(nonvar(_5),board_check_false_2_1(A,B,C,D,E,F,G,_3,_4,_2))
             )
        ).

board_check_false_2_1(A,B,C,D,E,F,G,_3,_4,_2) :-
        when((nonvar(G);nonvar(_3)),board_check_false_2_1(A,B,C,D,E,F,G,_3,_4,_2)).

board_work(0,s(A),[],[],B,C,C,_1,_2) :-
        seed(s(A),B,_1,_3),
        seed(A,C,_3,_2).

board_work(s(A),B,C,[D|E],F,G,H,_1,_2) :-
        board(A,B,I,E,J,K,L,_1,_3),
        new(B,D,_3,_4),
        app(D,I,C,_4,_5),
        add(D,J,F,_5,_6),
        adddig(D,K,G,_6,_7),
        rev(D,[],M,_7,_8),
        adddig(M,L,H,_8,_2).

atmost1(A,_2,_3) :-
        _2=[_4|_5],
        atmost1_check(A,_4,_5,_3).

atmost1_check(A,_3,_4,_2) :-
        nonvar(_3),
        !,
        atmost1_work(A,_4,_2).

atmost1_check(A,_3,_4,_2) :-
        nonvar(A),
        !,
        (
          A=[]  ->
             atmost1_work(A,_4,_2)
        ;
             atmost1_check_false(A,_3,_4,_2)
        ).

atmost1_check(A,_3,_4,_2) :-
        nonvar(A),
        !,
        (
          A=[X|_1]  ->
             atmost1_check(A,_3,_4,_2,X)
        ;
             when((nonvar(A);nonvar(_3)),atmost1_check(A,_3,_4,_2))
        ).

atmost1_check(A,_3,_4,_2) :-
        when((nonvar(A);nonvar(A);nonvar(_3)),atmost1_check(A,_3,_4,_2)).

atmost1_check(A,_3,_4,_2,X) :-
        nonvar(_3),
        !,
        atmost1_work(A,_4,_2).

atmost1_check(A,_3,_4,_2,X) :-
        nonvar(X),
        !,
        atmost1_work(A,_4,_2).

atmost1_check(A,_3,_4,_2,X) :-
        when((nonvar(X);nonvar(_3)),atmost1_check(A,_3,_4,_2,X)).

atmost1_check_false(A,_3,_4,_2) :-
        nonvar(_3),
        !,
        atmost1_work(A,_4,_2).

atmost1_check_false(A,_3,_4,_2) :-
        nonvar(A),
        !,
        (
          A=[X|_1]  ->
             atmost1_check_false(A,_3,_4,_2,X)
        ;
             when(nonvar(_3),atmost1_work(A,_4,_2))
        ).

atmost1_check_false(A,_3,_4,_2) :-
        when((nonvar(A);nonvar(_3)),atmost1_check_false(A,_3,_4,_2)).

atmost1_check_false(A,_3,_4,_2,X) :-
        nonvar(_3),
        !,
        atmost1_work(A,_4,_2).

atmost1_check_false(A,_3,_4,_2,X) :-
        nonvar(X),
        !,
        atmost1_work(A,_4,_2).

atmost1_check_false(A,_3,_4,_2,X) :-
        when((nonvar(X);nonvar(_3)),atmost1_check_false(A,_3,_4,_2,X)).

atmost1_work([],_1,_1).

atmost1_work([q|A],_1,_2) :-
        zero(A,_1,_2).

atmost1_work([0|A],_1,_2) :-
        atmost1(A,_1,_2).

add(A,B,C,_5,_6) :-
        _5=[_7|_8],
        add_check(A,B,C,_7,_8,_6).

add_check(A,B,C,_6,_7,_5) :-
        nonvar(_6),
        !,
        add_work(A,B,C,_7,_5).

add_check(A,B,C,_6,_7,_5) :-
        nonvar(A),
        !,
        add_work(A,B,C,_7,_5).

add_check(A,B,C,_6,_7,_5) :-
        nonvar(B),
        !,
        add_check_1(A,B,C,_6,_7,_5).

add_check(A,B,C,_6,_7,_5) :-
        nonvar(B),
        !,
        (
          \+B=[_1|_2]  ->
             add_work(A,B,C,_7,_5)
        ;
             add_check_false_2(A,B,C,_6,_7,_5)
        ).

add_check(A,B,C,_6,_7,_5) :-
        nonvar(C),
        !,
        (
          \+C=[_3|_4]  ->
             add_work(A,B,C,_7,_5)
        ;
             when((nonvar(B);nonvar(B);nonvar(A);nonvar(_6)),add_check(A,B,C,_6,_7,_5))
        ).

add_check(A,B,C,_6,_7,_5) :-
        when((nonvar(C);nonvar(B);nonvar(B);nonvar(A);nonvar(_6)),add_check(A,B,C,_6,_7,_5)).

add_check_1(A,B,C,_6,_7,_5) :-
        nonvar(_6),
        !,
        add_work(A,B,C,_7,_5).

add_check_1(A,B,C,_6,_7,_5) :-
        nonvar(C),
        !,
        (
          \+B=C  ->
             add_work(A,B,C,_7,_5)
        ;
             (
               B==C  ->
                  add_check_false_1(A,B,C,_6,_7,_5)
             ;
                  suspend_on_vars([B,C,_6],_8),
                  when(nonvar(_8),add_check_1(A,B,C,_6,_7,_5))
             )
        ).

add_check_1(A,B,C,_6,_7,_5) :-
        nonvar(B),
        !,
        (
          \+B=[_1|_2]  ->
             add_work(A,B,C,_7,_5)
        ;
             add_check_false_2(A,B,C,_6,_7,_5)
        ).

add_check_1(A,B,C,_6,_7,_5) :-
        nonvar(C),
        !,
        (
          \+C=[_3|_4]  ->
             add_work(A,B,C,_7,_5)
        ;
             when((nonvar(B);nonvar(C);nonvar(_6)),add_check_1(A,B,C,_6,_7,_5))
        ).

add_check_1(A,B,C,_6,_7,_5) :-
        when((nonvar(C);nonvar(B);nonvar(C);nonvar(_6)),add_check_1(A,B,C,_6,_7,_5)).

add_check_false_1(A,B,C,_6,_7,_5) :-
        nonvar(_6),
        !,
        add_work(A,B,C,_7,_5).

add_check_false_1(A,B,C,_6,_7,_5) :-
        nonvar(B),
        !,
        (
          \+B=[_1|_2]  ->
             add_work(A,B,C,_7,_5)
        ;
             add_check_false_2(A,B,C,_6,_7,_5)
        ).

add_check_false_1(A,B,C,_6,_7,_5) :-
        nonvar(C),
        !,
        (
          \+C=[_3|_4]  ->
             add_work(A,B,C,_7,_5)
        ;
             when((nonvar(B);nonvar(_6)),add_check_false_1(A,B,C,_6,_7,_5))
        ).

add_check_false_1(A,B,C,_6,_7,_5) :-
        when((nonvar(C);nonvar(B);nonvar(_6)),add_check_false_1(A,B,C,_6,_7,_5)).

add_check_false_2(A,B,C,_6,_7,_5) :-
        nonvar(_6),
        !,
        add_work(A,B,C,_7,_5).

add_check_false_2(A,B,C,_6,_7,_5) :-
        nonvar(C),
        !,
        (
          \+C=[_3|_4]  ->
             add_work(A,B,C,_7,_5)
        ;
             when(nonvar(_6),add_work(A,B,C,_7,_5))
        ).

add_check_false_2(A,B,C,_6,_7,_5) :-
        when((nonvar(C);nonvar(_6)),add_check_false_2(A,B,C,_6,_7,_5)).

add_work([],A,A,_1,_1).

add_work([A|B],[C|D],[[A|C]|E],_1,_2) :-
        add(B,D,E,_1,_2).

check(A,_4,_3) :-
        (
          nonvar(A)  ->
             check_work(A,_4,_3)
        ;
             _4=[_1|_2],
	     when((nonvar(_1);nonvar(A)),check_work(A,_2,_3))
%             check_susp(A,_1,_2,_3)
        ).

%:- block check_susp(-,-,?,?).

%check_susp(A,_1,_2,_3) :-
%        check_work(A,_2,_3).

check_work([],_1,_1).

check_work([A|B],_1,_2) :-
        atmost1(A,_1,_3),
        check(B,_3,_2).

rev(A,B,C,_1,_2) :-
        _1=[_3|_4],
        rev_check(A,B,C,_3,_4,_2).

rev_check(A,B,C,_2,_3,_1) :-
        nonvar(_2),
        !,
        rev_work(A,B,C,_3,_1).

rev_check(A,B,C,_2,_3,_1) :-
        nonvar(A),
        !,
        rev_work(A,B,C,_3,_1).

rev_check(A,B,C,_2,_3,_1) :-
        nonvar(B),
        !,
        rev_check_1(A,B,C,_2,_3,_1).

rev_check(A,B,C,_2,_3,_1) :-
        when((nonvar(B);nonvar(A);nonvar(_2)),rev_check(A,B,C,_2,_3,_1)).

rev_check_1(A,B,C,_2,_3,_1) :-
        nonvar(_2),
        !,
        rev_work(A,B,C,_3,_1).

rev_check_1(A,B,C,_2,_3,_1) :-
        nonvar(C),
        !,
        (
          \+B=C  ->
             rev_work(A,B,C,_3,_1)
        ;
             (
               B==C  ->
                  when(nonvar(_2),rev_work(A,B,C,_3,_1))
             ;
                  suspend_on_vars([B,C,_2],_4),
                  when(nonvar(_4),rev_check_1(A,B,C,_2,_3,_1))
             )
        ).

rev_check_1(A,B,C,_2,_3,_1) :-
        when((nonvar(C);nonvar(_2)),rev_check_1(A,B,C,_2,_3,_1)).

rev_work([],A,A,_1,_1).

rev_work([A|B],C,D,_1,_2) :-
        rev(B,[A|C],D,_1,_2).

new(A,B,_4,_3) :-
        (
          (
               nonvar(A)
          ;
               nonvar(B)
          )  ->
             new_work(A,B,_4,_3)
        ;
             _4=[_1|_2],
	     when((nonvar(_1);nonvar(A);nonvar(B)),new_work(A,B,_2,_3))
%             new_susp(A,B,_1,_2,_3)
        ).

%:- block new_susp(-,-,-,?,?).

%new_susp(A,B,_1,_2,_3) :-
%        new_work(A,B,_2,_3).

new_work(0,[],_1,_1).

new_work(s(A),[B|C],_1,_2) :-
        new(A,C,_1,_2).

queens(A,B,C,D,_1,_2) :-
        board(B,C,E,F,G,H,I,_1,_3),
        place(A,E,_3,_4),
        check(F,_4,_5),
        check(G,_5,_6),
        check(H,_6,_7),
        check(I,_7,_2),
        D=F.

ourmain(_3,_4) :-
        statistics(runtime,_1),
        ourdo(_3,_4),
        statistics(runtime,[_2,T1]),
        write(T1).

ourdo(_1,_2) :-
        queens(s(s(s(s(s(s(s(s(0)))))))),_1,_2),
        fail.

ourdo(_1,_1).
