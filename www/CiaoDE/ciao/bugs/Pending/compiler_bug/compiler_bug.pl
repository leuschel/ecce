bla(X,R) :-
        X = [A],
        ( A = T, f(_S, T) ->
            g([A],R)
        ; R = 2
        ).

f(A,t(A)).
g(A,l(A)).


main :- bla([term],R), display(R), nl.

/* Compiler says:

{WARNING: user:bla(_5291,_5292) :- ,(=(_5291,[_5299]),;(->(,(=(_5299,_5312),user:f(_5314,_5312)),user:g([_5299],_5292)),=(_5292,2))) - clause failed to compile}

*/

/*
should display: 2

both compiled and consulted

*/
