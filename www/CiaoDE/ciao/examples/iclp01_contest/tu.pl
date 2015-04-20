
tu(L):- tu00(L,_).

tu00((X=Y,M),B):- !, tu0(X,Y,B), tu00(M,B).
tu00((X=Y),B):- tu0(X,Y,B).

tu0(X,Y,_):- var(X), !, X=Y.
tu0(X,Y,_):- var(Y), !, X=Y.
tu0(X,Y,_):- number(X), !, X=Y.
tu0(X,Y,_):- number(Y), !, X=Y.
tu0([],Y,_):- !, []=Y.
tu0(Y,[],_):- !, []=Y.
tu0(Z,[Ya|Yb],B):-  !, Z= [Xa|Xb],
    tu0(Xa,Ya,B),
    tu0(Xb,Yb,B).
tu0([Xa|Xb],Z,B):- !, Z= [Ya|Yb],
    tu0(Xa,Ya,B),
    tu0(Xb,Yb,B).
tu0(X,Y,B):- functor(Y,Fy,A), functor(X,Fx,A), tolerated(Fx/A,Fy/A,B),
              tu_(A,X,Y,B).

tolerated(X,Y,[X=Y|_]):- !.
tolerated(X,Y,[A=B|Bs]):-
    A\==X,
    A\==Y,
    B\==X,
    B\==Y,
    tolerated(X,Y,Bs).

tu_(0,X,Y,B).
tu_(A,X,Y,B):- A>0,
    arg(A,X,Ax),
    arg(A,Y,Ay),
    tu0(Ax,Ay,B),
    A1 is A-1,
    tu_(A1,X,Y,B).
