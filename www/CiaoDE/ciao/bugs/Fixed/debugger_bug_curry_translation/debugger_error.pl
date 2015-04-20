:- use_module(library(system)).
:- use_module(library(lists)).

:- op(500, xfx, :=).
:- op(550, xfx, @).

% This operator is used by core and trans_lazy.

:- op(500, xfx, #).

:- discontiguous tohnf/2, tonf/2, constructor/2.

tonf(A#B, C) :-
        tohnf(A#B, D),
        tonf(D, C).

tonf((A,B),(Anf,Bnf)) :-
	!,
	tonf(A,Anf),
	tonf(B,Bnf).

tonf(A, A) :-
        (
	    number(A)
        ;   
	    var(A)
        ), 
	!.

% Head normal form detection.

is_in_hnf(A) :-
	var(A),
	!.

is_in_hnf(A) :-
	number(A),
	!.

tohnf(A,A) :- 
	is_in_hnf(A),
	!.

tohnf((A,B),(A,B)) :-
	!.

tohnf(apply_list(A,B)#C, C) :-
        apply_list_lazy(A, B, D),
        eval_lazy(D, C).

eval_lazy(pap(A,0,B)#_, C) :- 
	!,
        revonto_lazy(B, [], D),
        E=..[A|D], 
	F=..[tohnf,E#C,C],
        call(F).

eval_lazy(A, A).

%% Reverse list.
revonto_lazy([], A, A).
revonto_lazy([A|B], C, D) :-
        revonto_lazy(B, [A|C], D).

apply_lazy(pap(A,B,C)#X, D, pap(A,E,[D|C])#X) :-
        E is B-1.

apply_list_lazy(A, [], A).
apply_list_lazy(A, [B|C], D) :-
        apply_lazy(A, B, E),
        apply_list_lazy(E, C, D).

tohnf((lamdba0(R1) # R0),R0):-
	tohnf(R1,R2),
	lamdba01(R2,R0).

tohnf((reverseTuples(R1) # R0),R0):-
	reverseTuples1(R1,R0).

lamdba01(','(Vx,Vy),R0):-
	tohnf(','(Vy,Vx),R0).

reverseTuples1(R1,R0):-
	tohnf((apply_list((pap(lamdba0,1,[]) # R2),[R1]) # R0),R0).

goal(VR0):-
	tonf((reverseTuples(','(1,2)) # R1),VR0).


main:-
	goal(VR),
	write(VR).
