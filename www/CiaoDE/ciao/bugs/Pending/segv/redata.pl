:- module(_, _, []).

:- use_module(library(read)).

main :-
	loop.

loop :-
	read(X) ,
	( X = end_of_file ->
	    true
	; rewrite(X),
	  loop
	).

rewrite(X) :-
	X = f(_A1,A2,A3,A4,A5,A6,A7,A8,A9),
	display('f('),
	atom_concat(A3,A3,B3),
	atom_concat(B3,A4,C4),
	atom_concat(aaaaaaaaaaaaaaaaaaaa, A8, B8),
	atom_concat(B8,C4,D4),
	atom_concat(A5,D4,E5),
	md(E5),
	display(','),
	md(a),
	display(','),
	md(a),
	display(','),
	md(a),
	display(','),
	md(a),
	display(','),
	md(a),
	display(','),
	md(a),
	display(','),
	md(a),
	display(','),
	md(a),
	display(').\n').

md('') :- !, display(a).
md(A) :-
	atom_codes(A, C),
	tr(C, C2),
	atom_codes(B, C2),
	display(B).

tr([], []).
tr("\'"||Xs, "aaaaaaaaaaaaaaaa"||Ys) :- !,
	tr(Xs, Ys).
tr([_|Xs], "a"||Ys) :-
	tr(Xs, Ys).
