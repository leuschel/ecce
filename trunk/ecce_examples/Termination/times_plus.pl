r(X,Y):- times(X,Y,0), plus(X,Y,0).
plus(0,X,X).
plus(s(X),Y,s(Z)):- plus(X,Y,Z).
times(0,X,0).
times(s(X),Y,Z):- times(X,Y,W), plus(W,Y,Z).
