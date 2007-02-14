/* Example from Wadler */
flipflip(XT,YT) :- flip(XT,TT), flip(TT,YT).

flip(leaf(X),leaf(X)).
flip(tree(XT,Info,YT),tree(FYT,Info,FXT)) :-
	flip(XT,FXT),
	flip(YT,FYT).
