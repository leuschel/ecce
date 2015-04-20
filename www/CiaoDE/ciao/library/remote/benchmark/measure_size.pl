:- module(_, [main/1], []).

:- use_module(library('remote/read_and_write')).
:- use_module(library(lists)).
:- include(library(clpq)).  


main([Benchmark]):- main([Benchmark, Benchmark]).

main([Benchmark, FileName]):-
        generate_term(Benchmark, Term),
        transmit(received(Term, _Result), FileName).


transmit(Goal, FileName):-
        translate_goal(Goal, GoalToSend),
        open(FileName, write, Stream),
        remote_write(Stream, GoalToSend),
        close(Stream).


translate_goal(Goal, user_goal(Goal)).


generate_term(int_100, Term):-
        list_of_integers(100, Term).
generate_term(int_500, Term):-
        list_of_integers(500, Term).
generate_term(int_1000, Term):-
        list_of_integers(1000, Term).
generate_term(var_100, Term):-
        list_of_vars(100, Term).
generate_term(var_500, Term):-
        list_of_vars(500, Term).
generate_term(var_1000, Term):-
        list_of_vars(1000, Term).

generate_term(g0_10, Term):-
        variables1(10, Term).
generate_term(g0_50, Term):-
        variables1(50, Term).
generate_term(g0_100, Term):-
        variables1(100, Term).
generate_term(f_10, Term):-
        variables2(10, Term).
generate_term(f_50, Term):-
        variables2(50, Term).
generate_term(f_100, Term):-
        variables2(100, Term).


variables1(N, List):-
        length(List, N),
        set_constraints1(List).


variables2(N, List):-
        length(List, N),
        set_constraints2(List).


set_constraints2([_X, _Y]):- !.
set_constraints2([X, Y, Z|Rest]):-
        X .=. Y + Z,
        X .>. Y,
        X .>. Z,
        set_constraints2([Y,Z|Rest]).


set_constraints1([]).
set_constraints1([X|Xs]):-
        X .>. 0,
        set_constraints1(Xs).

list_of_integers(0, []).
list_of_integers(N, [Number|Ns]):-
        N > 0,
        Number is N + 255,
        N1 is N - 1,
        list_of_integers(N1, Ns).


list_of_vars(N, List):- length(List, N).
