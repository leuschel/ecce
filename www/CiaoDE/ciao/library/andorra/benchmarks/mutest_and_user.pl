:- module(mutest_and_user,[test/0,do_mutest/0],[andorra]).



:- determinate( test, true ).

test :-
        theorem(5,[m,u,i,i,u]).

:- determinate( rule1(_A,_B), true ).

rule1(A,B) :-
        app(C,[i],A),
        app(C,[i,u],B).

:- determinate( rule2(_A,_B), true ).

rule2([m|A],[m|B]) :-
        app(A,A,B).

%:- determinate( rule3(A,B), ( nonvar(A), A?\=[_|_] ; nonvar(B), B?\=[_|_] )).
rule3(A,B) :-
        app([i,i,i],C,A),
        app([u],C,B).
rule3([A|B],[A|C]) :-
        rule3(B,C).



%:- determinate( rule4(A,B), (nonvar(B), B?\=[_|_]; nonvar(A),nonvar(B), term(A,[1]) ?\= term(B,[1]))). 

rule4([A|B],C) :-
        app([u,u],C,[A|B]).
rule4([A|B],[A|C]) :-
        rule4(B,C).

:- determinate( rules(_A,_B), false ).

rules(A,B) :-
        rule3(A,B).
rules(A,B) :-
        rule4(A,B).
rules(A,B) :-
        rule1(A,B).
rules(A,B) :-
        rule2(A,B).

%:- determinate( theorem(A,_B), (nonvar(A), \+atomic(A)) ).

theorem(A,[m,i]).
theorem(A,[B|C]) :-
        A>0,
        D is A-1,
        theorem(D,E),
        rules(E,[B|C]).

%:- determinate( app(A,B,C), ( nonvar(C), nonvar(B) , B?\=C ;
%	                      nonvar(A),( A?=[] ;  A?=[_|_]) ; 
%	                       nonvar(C), C?\=[_|_] )).

app([],A,A).
app([A|B],C,[A|D]) :-
        app(B,C,D).




do_mutest:-
	test,
	fail.
do_mutest.
