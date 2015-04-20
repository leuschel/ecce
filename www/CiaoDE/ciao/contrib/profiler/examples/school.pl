:- module(school,_,[profiler,expander]).
%:- module(school,_,[]).
%,[catch,expander]).
%:- module(school,_,[catch,profile,expander]).

:- use_module(library(aggregates)).
:- use_module(library('profiler/profiler_utils')).

%:- notiming '*'/'*'.
%:- counting '*'/'*'.

% :- timing '*'/'*'.
% :- nocounting '*'/'*'.

%:- timing student/2.
%:- timing prog/1.

student(john, cs453).
student(john, cs520).
student(john, cs455).
student(john, ma561).
student(tom, cs342).
student(tom, cs453).
student(mary, cs455).
student(mary, cs520).
student(paul, cs520).
student(jane, cs453).
student(jane, ma561).
student(robert, cs342).
student(larry, ma561).
student(larry, cs342).
student(larry, cs455).

teacher(binkley, cs453).
teacher(binkley, cs342).
teacher(opus, cs455).
teacher(dallas, cs520).
teacher(dallas, ma561).

course(cs453, eco103, tue).
course(cs455, gs701, mon).
course(cs455, gs701, wed).
course(cs342, eco103, fri).
course(cs520, gs703, tue).
course(ma561, ma123, mon).

prog(L) :- findall( (S, T, R), p(S, T, R), L).

p(S, T, R) :-
	student(S, C1),student(S, C2),
	teacher(T, C1), teacher(T, C2),
	course(C1, R, _D1), course(C2, R, _D2),
	\+(C1 = C2).

%:- data pppppp/2.

%main :-
%	measure((assertz_fact(pppppp(x,y)),current_fact(pppppp(X,Y)),retract_fact(pppppp(X,Y))),T),display(T),nl.

main :-
	measure(prog(X),T),
	display(X),nl,display('Time='),display(T),display(' ms'),nl.
%profile_reset,profile_start,prog(X),profile_stop,profile_dump.
