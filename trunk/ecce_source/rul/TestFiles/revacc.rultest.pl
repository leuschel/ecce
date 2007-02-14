
rev_tc(X,Res) :- rev(X,[],Res), list(Res).

rev([],A,A).
rev([H|X],A,R) :- rev(X,[H|A],R).

list(a) :- print(not_list(a)),nl.
list([]).
list([H|T]) :- list(T).

rev_tc2(X,Res) :-
    rev(X,[],Res),
    list_check(Res).

list_check([]).
list_check([_H|T]) :- list_check(T).
list_check(X) :- X\=[], X\= [_|_], print(not_list(X)),nl.

rev1(L,R) :- rev1(L,[],R).
rev1([],A,A) :- list1(A).
rev1([H|X],A,R) :- rev1(X,[H|A],R).

list1(a) :- print(not_list(a)),nl.
list1([]).
list1([H|T]) :- list1(T).

rev2(L,R) :- list(L,a),rev2(L,[],R).

rev2([],A,A) :- list(A,a).
rev2([H|X],A,R) :- rev2(X,[H|A],R).

list([],_).
list([H|T],H) :- list(T,H).