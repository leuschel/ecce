:- module(_, _, []).

:- use_module('/home/clip/Systems/ciao-1.11/library.development/multi_dynamic/multi_dynamic.pl').

point_new(P) :-
	multi_new(P),
	multi_data(P, x/1),
	multi_data(P, y/1),
	multi_dynamic(P, show/1),
	multi_asserta(P, show(Obj), point_print(Obj)).

point_move(P, X, Y) :-
	multi_set_fact(P, x(X)),
	multi_set_fact(P, y(Y)).

point_get(P, X, Y) :-
	multi_current_fact(P, x(X)),
	multi_current_fact(P, y(Y)).

point_print(P) :-
	point_get(P, X, Y),
	display('Point'),
	display(' x:'), display(X), 
	display(' y:'), display(Y), nl.

point_show(P) :-
	multi_call(P, show(P)).

point2_new(P) :-
	point_new(P),
	multi_abolish(P, show/1),
	multi_asserta(P, show(Obj), point2_print(Obj)).

point2_print(P) :-
	point_get(P, X, Y),
	display('Point('),
	display(X), 
	display(', '), display(Y), display(')'), nl.

point3_new(P) :-
	multi_new(P),
	multi_data(P, x/1),
	multi_data(P, y/1),
	multi_dynamic(P, show/1),
	multi_asserta(P, show(Obj), point3_print(Obj)).

point3_print(P) :-
	point_get(P, X, Y),
	display('<point x="'),
	display(X), 
	display('" y="'), display(Y), display('"/>'), nl.

test :-
	point_new(P1),
	point2_new(P2),
	point3_new(P3),
	point_move(P1, 10, 10),
	point_move(P2, 100, 100),
	point_move(P3, 1000, 1000),
	point_show(P1), 
	point_show(P2),
	point_show(P3).
