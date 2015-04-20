
:- module(dynamic,[wellformed_body/3]).

%% Dynamic predicates are built-in in SICStus3

wellformed_body(B, _, call(B)) :- var(B), !.
wellformed_body(!, Env, !) :- !, Env = + .
wellformed_body((A,B), Env, (A1,B1)) :- !,
        wellformed_body(A, Env, A1),
	wellformed_body(B, Env, B1).
wellformed_body((A->B), Env, (A1->B1)) :- !,
        wellformed_body(A, -, A1),
	wellformed_body(B, Env, B1).
wellformed_body((A;B), Env, (A1;B1)) :- !,
        wellformed_body(A, Env, A1),
	wellformed_body(B, Env, B1).
wellformed_body((\+ A), _, (\+ A1)) :- !,
        wellformed_body(A, -, A1).
wellformed_body(if(A,B,C), Env, if(A1,B1,C1)) :- !,
        wellformed_body(A, -, A1),
        wellformed_body(B, Env, B1),
        wellformed_body(C, Env, C1).
wellformed_body(L^A, Env, L^A1) :- !,
        wellformed_body(A, Env, A1).
wellformed_body(Goal, _, Goal) :-
	functor(Goal, F, _),
	atom(F).
