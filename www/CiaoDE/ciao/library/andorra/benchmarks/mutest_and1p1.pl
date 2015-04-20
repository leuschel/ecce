:- module(mutest_and1p1,[test/0],_).
:- use_module(qe_andorra_lib).
:- use_module(library(when),[when/2]).
:- use_module(library(prolog_sys),[statistics/2]).
:- use_module(library(write),[write/1]).

test:-
	test(_1,_2),
	wakeup(_1,_2).

test(_1,_2) :-
        theorem(5,[m,u,i,i,u],_1,_2).

rule1(A,B,_1,_2) :-
        app(C,[i],A,_1,_3),
        app(C,[i,u],B,_3,_2).

rule2([m|A],[m|B],_1,_2) :-
        app(A,A,B,_1,_2).

rule3(A,B,_5,_6) :-
        _5=[_7|_8],
        rule3_check(A,B,_7,_8,_6).

rule3_check(A,B,_6,_7,_5) :-
        nonvar(_6),
        !,
        rule3_work(A,B,_7,_5).

rule3_check(A,B,_6,_7,_5) :-
        nonvar(A),
        !,
        (
          \+A=[_1|_2]  ->
             rule3_work(A,B,_7,_5)
        ;
             rule3_check_false(A,B,_6,_7,_5)
        ).

rule3_check(A,B,_6,_7,_5) :-
        nonvar(B),
        !,
        (
          \+B=[_3|_4]  ->
             rule3_work(A,B,_7,_5)
        ;
             when((nonvar(A);nonvar(_6)),rule3_check(A,B,_6,_7,_5))
        ).

rule3_check(A,B,_6,_7,_5) :-
        when((nonvar(B);nonvar(A);nonvar(_6)),rule3_check(A,B,_6,_7,_5)).

rule3_check_false(A,B,_6,_7,_5) :-
        nonvar(_6),
        !,
        rule3_work(A,B,_7,_5).

rule3_check_false(A,B,_6,_7,_5) :-
        nonvar(B),
        !,
        (
          \+B=[_3|_4]  ->
             rule3_work(A,B,_7,_5)
        ;
             when(nonvar(_6),rule3_work(A,B,_7,_5))
        ).

rule3_check_false(A,B,_6,_7,_5) :-
        when((nonvar(B);nonvar(_6)),rule3_check_false(A,B,_6,_7,_5)).

rule3_work(A,B,_1,_2) :-
        app([i,i,i],C,A,_1,_3),
        app([u],C,B,_3,_2).

rule3_work([A|B],[A|C],_1,_2) :-
        rule3(B,C,_1,_2).

rule4(A,B,_5,_6) :-
        _5=[_7|_8],
        rule4_check(A,B,_7,_8,_6).

rule4_check(A,B,_6,_7,_5) :-
        nonvar(_6),
        !,
        rule4_work(A,B,_7,_5).

rule4_check(A,B,_6,_7,_5) :-
        nonvar(B),
        !,
        (
          \+B=[_1|_2]  ->
             rule4_work(A,B,_7,_5)
        ;
             rule4_check_false(A,B,_6,_7,_5)
        ).

rule4_check(A,B,_6,_7,_5) :-
        nonvar(A),
        !,
        (
          A=[Y|_3]  ->
             rule4_check(A,B,_6,_7,_5,Y,X,_4)
        ;
             when((nonvar(B);nonvar(_6)),rule4_check(A,B,_6,_7,_5))
        ).

rule4_check(A,B,_6,_7,_5) :-
        when((nonvar(A);nonvar(B);nonvar(_6)),rule4_check(A,B,_6,_7,_5)).

rule4_check(A,B,_6,_7,_5,Y,X,_4) :-
        nonvar(_6),
        !,
        rule4_work(A,B,_7,_5).

rule4_check(A,B,_6,_7,_5,Y,X,_4) :-
        nonvar(B),
        !,
        (
          B=[X|_4]  ->
             rule4_check_1(A,B,_6,_7,_5,Y,X,_4)
        ;
             when(nonvar(_6),rule4_work(A,B,_7,_5))
        ).

rule4_check(A,B,_6,_7,_5,Y,X,_4) :-
        when((nonvar(B);nonvar(_6)),rule4_check(A,B,_6,_7,_5,Y,X,_4)).

rule4_check_1(A,B,_6,_7,_5,Y,X,_4) :-
        nonvar(_6),
        !,
        rule4_work(A,B,_7,_5).

rule4_check_1(A,B,_6,_7,_5,Y,X,_4) :-
        nonvar(X),
        !,
        rule4_check_1_1(A,B,_6,_7,_5,Y,X,_4).

rule4_check_1(A,B,_6,_7,_5,Y,X,_4) :-
        when((nonvar(X);nonvar(_6)),rule4_check_1(A,B,_6,_7,_5,Y,X,_4)).

rule4_check_1_1(A,B,_6,_7,_5,Y,X,_4) :-
        nonvar(_6),
        !,
        rule4_work(A,B,_7,_5).

rule4_check_1_1(A,B,_6,_7,_5,Y,X,_4) :-
        nonvar(Y),
        !,
        (
          \+X=Y  ->
             rule4_work(A,B,_7,_5)
        ;
             (
               X==Y  ->
                  when(nonvar(_6),rule4_work(A,B,_7,_5))
             ;
                  suspend_on_vars([X,Y,_6],_8),
                  when(nonvar(_8),rule4_check_1_1(A,B,_6,_7,_5,Y,X,_4))
             )
        ).

rule4_check_1_1(A,B,_6,_7,_5,Y,X,_4) :-
        when((nonvar(Y);nonvar(_6)),rule4_check_1_1(A,B,_6,_7,_5,Y,X,_4)).

rule4_check_false(A,B,_6,_7,_5) :-
        nonvar(_6),
        !,
        rule4_work(A,B,_7,_5).

rule4_check_false(A,B,_6,_7,_5) :-
        nonvar(A),
        !,
        (
          A=[Y|_3]  ->
             rule4_check_false(A,B,_6,_7,_5,Y,X,_4)
        ;
             when(nonvar(_6),rule4_work(A,B,_7,_5))
        ).

rule4_check_false(A,B,_6,_7,_5) :-
        when((nonvar(A);nonvar(_6)),rule4_check_false(A,B,_6,_7,_5)).

rule4_check_false(A,B,_6,_7,_5,Y,X,_4) :-
        nonvar(_6),
        !,
        rule4_work(A,B,_7,_5).

rule4_check_false(A,B,_6,_7,_5,Y,X,_4) :-
        nonvar(B),
        !,
        (
          B=[X|_4]  ->
             rule4_check_false_1(A,B,_6,_7,_5,Y,X,_4)
        ;
             when(nonvar(_6),rule4_work(A,B,_7,_5))
        ).

rule4_check_false(A,B,_6,_7,_5,Y,X,_4) :-
        when((nonvar(B);nonvar(_6)),rule4_check_false(A,B,_6,_7,_5,Y,X,_4)).

rule4_check_false_1(A,B,_6,_7,_5,Y,X,_4) :-
        nonvar(_6),
        !,
        rule4_work(A,B,_7,_5).

rule4_check_false_1(A,B,_6,_7,_5,Y,X,_4) :-
        nonvar(X),
        !,
        rule4_check_false_1_1(A,B,_6,_7,_5,Y,X,_4).

rule4_check_false_1(A,B,_6,_7,_5,Y,X,_4) :-
        when((nonvar(X);nonvar(_6)),rule4_check_false_1(A,B,_6,_7,_5,Y,X,_4)).

rule4_check_false_1_1(A,B,_6,_7,_5,Y,X,_4) :-
        nonvar(_6),
        !,
        rule4_work(A,B,_7,_5).

rule4_check_false_1_1(A,B,_6,_7,_5,Y,X,_4) :-
        nonvar(Y),
        !,
        (
          \+X=Y  ->
             rule4_work(A,B,_7,_5)
        ;
             (
               X==Y  ->
                  when(nonvar(_6),rule4_work(A,B,_7,_5))
             ;
                  suspend_on_vars([X,Y,_6],_8),
                  when(nonvar(_8),rule4_check_false_1_1(A,B,_6,_7,_5,Y,X,_4))
             )
        ).

rule4_check_false_1_1(A,B,_6,_7,_5,Y,X,_4) :-
        when((nonvar(Y);nonvar(_6)),rule4_check_false_1_1(A,B,_6,_7,_5,Y,X,_4)).

rule4_work([A|B],C,_1,_2) :-
        app([u,u],C,[A|B],_1,_2).

rule4_work([A|B],[A|C],_1,_2) :-
        rule4(B,C,_1,_2).

rules(A,B,[_2|_3],_1) :-
	when(nonvar(_2),rules_work(A,B,_3,_1)).
%        rules_susp(A,B,_2,_3,_1).

%:- block rules_susp(?,?,-,?,?).

%rules_susp(A,B,_2,_3,_1) :-
%        rules_work(A,B,_3,_1).

rules_work(A,B,_1,_2) :-
        rule3(A,B,_1,_2).

rules_work(A,B,_1,_2) :-
        rule4(A,B,_1,_2).

rules_work(A,B,_1,_2) :-
        rule1(A,B,_1,_2).

rules_work(A,B,_1,_2) :-
        rule2(A,B,_1,_2).

theorem(A,B,_1,_2) :-
        _1=[_3|_4],
        theorem_check(A,B,_3,_4,_2).

theorem_check(A,B,_2,_3,_1) :-
        nonvar(_2),
        !,
        theorem_work(A,B,_3,_1).

theorem_check(A,B,_2,_3,_1) :-
        nonvar(A),
        !,
        (
          \+atomic(A)  ->
             theorem_work(A,B,_3,_1)
        ;
             when(nonvar(_2),theorem_work(A,B,_3,_1))
        ).

theorem_check(A,B,_2,_3,_1) :-
        when((nonvar(A);nonvar(_2)),theorem_check(A,B,_2,_3,_1)).

theorem_work(A,[m,i],_1,_1).

theorem_work(A,[B|C],_1,_2) :-
        >(A,0,_1,_3),
        is(D,A-1,_3,_4),
        theorem(D,E,_4,_5),
        rules(E,[B|C],_5,_2).

app(A,B,C,_5,_6) :-
        _5=[_7|_8],
        app_check(A,B,C,_7,_8,_6).

app_check(A,B,C,_6,_7,_5) :-
        nonvar(_6),
        !,
        app_work(A,B,C,_7,_5).

app_check(A,B,C,_6,_7,_5) :-
        nonvar(A),
        !,
        (
          (
               A=[]
          ;
               A=[_1|_2]
          )  ->
             app_work(A,B,C,_7,_5)
        ;
             app_check_false(A,B,C,_6,_7,_5)
        ).

app_check(A,B,C,_6,_7,_5) :-
        nonvar(C),
        !,
        (
          \+C=[_3|_4]  ->
             app_work(A,B,C,_7,_5)
        ;
             app_check_false_1(A,B,C,_6,_7,_5)
        ).

app_check(A,B,C,_6,_7,_5) :-
        nonvar(B),
        !,
        app_check_1(A,B,C,_6,_7,_5).

app_check(A,B,C,_6,_7,_5) :-
        when((nonvar(B);nonvar(C);nonvar(A);nonvar(_6)),app_check(A,B,C,_6,_7,_5)).

app_check_1(A,B,C,_6,_7,_5) :-
        nonvar(_6),
        !,
        app_work(A,B,C,_7,_5).

% add by cl
app_check_1(A,B,C,_6,_7,_5) :-
        nonvar(A),
        !,
        (
          (
               A=[]
          ;
               A=[_1|_2]
          )  ->
             app_work(A,B,C,_7,_5)
        ;
             app_check_false(A,B,C,_6,_7,_5)
        ).


app_check_1(A,B,C,_6,_7,_5) :-
        nonvar(C),
        !,
        (
          \+B=C  ->
             app_work(A,B,C,_7,_5)
        ;
             (
               B==C  ->
%                  when(nonvar(_6),app_work(A,B,C,_7,_5))
                  when((nonvar(A);nonvar(_6)),app_work(A,B,C,_7,_5))
             ;
%                  suspend_on_vars([B,C,_6],_8),
                  suspend_on_vars([A,B,C,_6],_8),
                  when(nonvar(_8),app_check_1(A,B,C,_6,_7,_5))
             )
        ).

app_check_1(A,B,C,_6,_7,_5) :-
        when((nonvar(A);nonvar(C);nonvar(_6)),app_check_1(A,B,C,_6,_7,_5)).

app_check_false(A,B,C,_6,_7,_5) :-
        nonvar(_6),
        !,
        app_work(A,B,C,_7,_5).

app_check_false(A,B,C,_6,_7,_5) :-
        nonvar(C),
        !,
        (
          \+C=[_3|_4]  ->
             app_work(A,B,C,_7,_5)
        ;
             app_check_false_1(A,B,C,_6,_7,_5)
        ).

app_check_false(A,B,C,_6,_7,_5) :-
        nonvar(B),
        !,
        app_check_false_2(A,B,C,_6,_7,_5).

app_check_false(A,B,C,_6,_7,_5) :-
        when((nonvar(B);nonvar(C);nonvar(_6)),app_check_false(A,B,C,_6,_7,_5)).

app_check_false_2(A,B,C,_6,_7,_5) :-
        nonvar(_6),
        !,
        app_work(A,B,C,_7,_5).

app_check_false_2(A,B,C,_6,_7,_5) :-
        nonvar(C),
        !,
        (
          \+B=C  ->
             app_work(A,B,C,_7,_5)
        ;
             (
               B==C  ->
                  when(nonvar(_6),app_work(A,B,C,_7,_5))
             ;
                  suspend_on_vars([B,C,_6],_8),
                  when(nonvar(_8),app_check_false_2(A,B,C,_6,_7,_5))
             )
        ).

app_check_false_2(A,B,C,_6,_7,_5) :-
        when((nonvar(C);nonvar(_6)),app_check_false_2(A,B,C,_6,_7,_5)).

app_check_false_1(A,B,C,_6,_7,_5) :-
        nonvar(_6),
        !,
        app_work(A,B,C,_7,_5).

app_check_false_1(A,B,C,_6,_7,_5) :-
        nonvar(B),
        !,
        app_check_false_1_1(A,B,C,_6,_7,_5).

app_check_false_1(A,B,C,_6,_7,_5) :-
        when((nonvar(B);nonvar(_6)),app_check_false_1(A,B,C,_6,_7,_5)).

app_check_false_1_1(A,B,C,_6,_7,_5) :-
        nonvar(_6),
        !,
        app_work(A,B,C,_7,_5).

app_check_false_1_1(A,B,C,_6,_7,_5) :-
        nonvar(C),
        !,
        (
          \+B=C  ->
             app_work(A,B,C,_7,_5)
        ;
             (
               B==C  ->
                  when(nonvar(_6),app_work(A,B,C,_7,_5))
             ;
                  suspend_on_vars([B,C,_6],_8),
                  when(nonvar(_8),app_check_false_1_1(A,B,C,_6,_7,_5))
             )
        ).

app_check_false_1_1(A,B,C,_6,_7,_5) :-
        when((nonvar(C);nonvar(_6)),app_check_false_1_1(A,B,C,_6,_7,_5)).

app_work([],A,A,_1,_1).

app_work([A|B],C,[A|D],_1,_2) :-
        app(B,C,D,_1,_2).
