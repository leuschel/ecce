:- dynamic last/2, reva/3, reverselast/0.last([X],X).last([H,H2|T],X) :- last([H2|T],X).reva([], Acc, Acc).reva([Y|Z], R, Acc):-	reva(Z, R, [Y|Acc]).reverselast:- reva(L, R, [a]), last(R, b).