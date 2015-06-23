


simplify(mul(0,X),0).
simplify(mul(1,X),R) :- simplify(X,R).
simplify(mul(X,0),0).
simplify(mul(X,1),R) :- simplify(X,R).
simplify(mul(X,add(Y,Z)),R) :- dif(X,1), dif(X,0),simplify(add(mul(X,Y),mul(X,Z)),R).
simplify(mul(X,Y),mul(SX,SY)) :- dif(X,0), dif(Y,0), dif(X,1), dif(Y,1),
  \+ functor(Y,add,2),
  simplify(X,SX), simplify(Y,SY).
simplify(add(0,X),R) :- simplify(X,R).
simplify(add(X,0),R) :- simplify(X,R).
simplify(add(X,Y),add(SX,SY)) :- dif(X,0), dif(Y,0), 
  simplify(X,SX), simplify(Y,SY).
simplify(N,N) :- atomic(N).
%simplify(mul(2,Y),R) :- simplify(add(Y,Y),R).
%simplify(add(Y,Y),R) :- simplify(mul(2,Y),R).

test(R) :- simplify(mul(2,add(x,1)),R).
