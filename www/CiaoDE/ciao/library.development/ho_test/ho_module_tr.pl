:- module(ho_module_tr,[ho_module_tr/2],[]).

%% Expands:
%% Lists::member(X,[1,2,3])
%% where Lists = lists(_,_...)

ho_module_tr( '::'(ObjectId,Call), Conjunction ) :-
	Conjunction =  (
	  Call =.. [MethodName|Args],
	  member(MethodName:PredId,ObjectId),
	  RealCall =.. [call,PredId|Args],
	  call(RealCall)
	  ),
	display('Goals '),
	display(Conjunction),
	nl.
