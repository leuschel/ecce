even(0).
even(s(X)) :- odd(X).
odd(s(X)) :- even(X).

eo(X) :- even(X),odd(X).

 :- mode plus(i,i,o).
plus(0,X,X).
plus(s(X),Y,s(Z)) :- plus(X,Y,Z).

pzero(X,XPlusZero) :- plus(X,0,XPlusZero).
passoc(X,Y,Z,R1,R2) :- plus(X,Y,XY),plus(XY,Z,R1),
  plus(Y,Z,YZ),plus(X,YZ,R2).

pcomm(X,Y,R1,R2) :- plus(X,Y,R1),plus(Y,X,R2).

 :- mode mul(i,i,o). 

mul(0,X,0).
mul(s(X),Y,Z) :- mul(X,Y,XY),plus(XY,Y,Z).

pmul(X,Y,Z,R1,R2) :- mul(X,Y,XY),mul(XY,Z,R1),
  mul(Y,Z,YZ),mul(X,YZ,R2).


exp(Base,0,s(0)).
exp(Base,s(E),R) :-
	exp(Base,E,BE),
	mul(BE,Base,R).

tfail(B,E) :-
	exp(B,s(s(E)),s(s(s(0)))).
