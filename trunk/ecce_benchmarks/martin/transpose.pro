/* file: transpose.pro */

/* PD query:
	transpose:transpose([[X1,X2,X3,X4,X5,X6,X7,X8,X9],
		Xr2,Xr3],Xtrm) */

transpose:transpose(Xs,[]) :-
	transpose:nullrows(Xs).
transpose:transpose(Xs,[Y|Ys]) :-
	transpose:makerow(Xs,Y,Zs),
	transpose:transpose(Zs,Ys).

transpose:makerow([],[],[]).
transpose:makerow([[X|Xs]|Ys],[X|Xs1],[Xs|Zs]) :-
	transpose:makerow(Ys,Xs1,Zs).

transpose:nullrows([]).
transpose:nullrows([[]|Ns]) :-
	transpose:nullrows(Ns).



