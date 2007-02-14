
rev(A,B) :- rev(A,[],B).

rev([],Acc,Acc).
rev([H|T],Acc,Res) :- rev(T,[H|Acc],Res).