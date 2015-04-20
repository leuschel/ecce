
:- module(attrvars,[attr_vars/2],[]).

attr_vars(X,Vs):- collect(X,[],Vs).

memberchk(X,[Y|_]):- X==Y, !.
memberchk(X,[_|L]):- memberchk(X,L).

collect(X,V0,V1):-
    memberchk(X,V0), !,
    V1=V0.
collect(X,V0,V1):-
    get_attribute(X,A), !,
    collect(A,[X|V0],V1).
collect(X,V0,V1):-
    var(X), !,
    V1=V0.
collect(X,V0,V1):-
    term_basic:functor(X,_,N),
    collect_(N,X,V0,V1).

collect_(0,_,V0,V1):- !,
    V1=V0.
collect_(N,X,V0,V2):-
    N > 0, 
    arg(N,X,ArgX),
    collect(ArgX,V0,V1),
    N1 is N-1,
    collect_(N1,X,V1,V2).
