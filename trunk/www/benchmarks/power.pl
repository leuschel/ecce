power(B,0,1).
power(B,1,B).
power(B,E,R) :- E>1, E1 is E-1, power(B,E1,R1), R is R1*B.

square(X,Res) :- power(X,2,Res).