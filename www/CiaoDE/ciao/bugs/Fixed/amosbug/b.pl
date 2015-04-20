:- module(_, [fff/0], []).

:- use_module(library(streams)).
:- use_module(library(read)).

:- initialization(fff).

fff :-
	foo9(_,_,_,_,_,_,_,_,_),
        foo3,
        foo(Spec, Key).
fff.

foo9(_,_,_,_,_,_,_,_,_).
foo9(_,_,_,_,_,_,_,_,_) :- fail.
foo(_,_).

foo3 :-
        foo2(a,a),
        display(a), nl,
        foo5a(Terms),
	display(b), nl,
	foo5b(Lops),
	display(c), nl,
	true,
	Lops = _,
	Terms = _.

foo2(_, _).
foo2(_, _).

foo5a(Terms) :-
        current_input(OldInput),
        open(data1, read, Stream),
        set_input(Stream),
        read_terms(a, Terms, []),
        set_input(OldInput),
        close(Stream).

foo5b(Lops) :-
        current_input(OldInput2),
        open(data2, read, Stream2),
        set_input(Stream2),
        read_terms(a, Lops, []),
        set_input(OldInput2),
        close(Stream2).

read_terms(end_of_file, Ts, Ts) :- !.
read_terms(T, [a|Ts], Ts_) :-
        read(T1),
        read_terms(T1, Ts, Ts_).
