:- module(symm,[main/0],[objects]).

:- use_class(symmclass).

:- use_module(library(dummy)).

main :-
	statistics(program,[Prog0,_]),
	statistics(symbols,[Symb0,Pred0]),
	X new symmclass,
	statistics(symbols,[Symb1,Pred1]),
	destroy X,
	statistics(symbols,[Symb2,Pred2]),
	statistics(program,[Prog1,_]),
	!,
	S1 is Symb1-Symb0,
	S2 is Symb2-Symb0,
	P1 is Pred1-Pred0,
	P2 is Pred2-Pred0,
	Pr is Prog1-Prog0,
	!,
	inform_user(['after new      symb/pred usage:    ',S1,'/',P1]),
	inform_user(['after destroy  symb/pred usage:    ',S2,'/',P2]),
	inform_user(['               program area usage: ',Pr]),
	inform_user(['NOTE: expected results are 0 usage after destroy...']),
	true.
