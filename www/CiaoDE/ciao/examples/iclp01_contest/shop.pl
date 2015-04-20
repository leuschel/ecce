shop([Mn-M|S]):-
    milieus([M|Ms]),
    outerwall(Mn),
    next(Mn,Ms,[Mn],Last,S),
    outerwall(Last).

next(M,[N|Ms],Visit,Last, [Mn-N|S]):-
    (nextto(M,Mn);shortcut(M,Mn)),
    \+ member(Mn,Visit),
    next(Mn,Ms,[Mn|Visit],Last, S).

next(_,[],[L|_],L,[]).

member(X, [X|_]).
member(X, [_Y|Xs]):- member(X, Xs).
