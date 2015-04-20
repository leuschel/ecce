/* file revlast.pro */

revlast(L,X,Last) :- copy(L,X,LX),
	rev(LX,[],RLX),member(M,RLX),
	append(L,[M],LM),last(Last,LM).

revlast_simple(L,X,Res) :-
	rev_simple(L,[],_,X,Res).

revlast_very_simple(L,Res) :- rev_very_simple(L,[],_,Res).

copy([],_,[]).
copy([_|T],X,[X|T2]) :- copy(T,X,T2).

last(X,[X]).
last(X,[H|T]) :- last(X,T).

rev([],L,L).
rev([H|X],A,R) :- rev(X,[H|A],R).

append([],L,L).
append([H|L1],L2,[H|L3]) :- append(L1,L2,L3).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

rev_simple([],L,L,_,Res) :- member(Res,L).
rev_simple([H|X],A,R,H,Res) :- rev_simple(X,[H|A],R,H,Res).

rev_very_simple([],L,L,Res) :- L = [Res|_].
rev_very_simple([H|X],A,R,Res) :-
	rev_very_simple(X,[a|A],R,Res).