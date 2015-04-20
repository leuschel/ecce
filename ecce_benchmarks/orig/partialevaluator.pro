
pe_atom(Atom) :-
	generalise(Atom,GAtom),
	unfold([Atom],[],ResultantBody),
	print(Atom),
	print(' <- '),
	print(ResultantBody),nl.

unfold([],_AncList,[]).
unfold([Head|Tail],AncList,[Head|Res]) :-
	pred_member(Head,AncList), /* potential loop: do not unfold */
	unfold(Tail,AncList,Res).
unfold([Head|Tail],AncList,Res) :-
	\+(pred_member(Head,AncList)), /* then unfold */
	claus(Head,Body),
	Head =..[Pred|_Args],
	unfold(Body,[Pred|AncList],BRes),
	unfold(Tail,AncList,TRes),
	app(BRes,TRes,Res).

pred_member(X,[Pred|T]) :- X=..[Pred|_Args].
pred_member(X,[_P|T]) :- pred_member(X,T).

generalise(member(X,Y),member(X,Y2)).
generalise(inboth(X,L1,L2),member(X,L12,L22)).
generalise(app(X,Y,Z),app(X2,Y2,Z2)).
generalise(delete(X,Y,Z),delete(X,Y2,Z2)).
generalise(test(A,L1,L2,Res),test(A,L1,L2,Res)).

claus(member(X,[X|T]),[]).
claus(member(X,[Y|T]),[member(X,T)]).

claus(inboth(X,L1,L2),[member(X,L1),member(X,L2)]).

claus(app([],L,L),[]).
claus(app([H|X],Y,[H|Z]),[app(X,Y,Z)]).

claus(delete(X,[X|T],T),[]).
claus(delete(X,[Y|T],[Y|D]),[delete(X,T,D)]).

claus(test(A,L1,L2,Res),
	[inboth(A,L1,L2),delete(A,L1,D1),app(D1,L2,Res)]).

app([],L,L).
app([H|X],Y,[H|Z]) :- app(X,Y,Z).