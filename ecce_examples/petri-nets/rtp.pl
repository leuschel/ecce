/*
Example : rtp

vars
  X1 X2 X3 X4 X5 X6 X7 X8 X9 
rules
  X1>=1        ->  X1'=X1-1,X2'=X2+1;
  X2>=1        ->  X2'=X2-1,X3'=X3+1;
  X3>=1        ->  X3'=X3-1,X4'=X4+1;
  X4>=1        ->  X4'=X4-1,X5'=X5+1;
  x4>=1        ->  X4'=X4-1,X9'=X9+1;
  X5>=1        ->  X5'=X5-1,X6'=X6+1;
  X6>=1        ->  X6'=X6-1,X9'=X9+1;
  X6>=1        ->  X6'=X6-1,X7'=X7+1;
  X6>=1        ->  X6'=X6-1,X8'=X8+1;
  X7>=1        ->  X7'=X7-1,X9'=X9+1;
  X8>=1        ->  X8'=X8-1,X9'=X9+1;
  X9>=1        ->  X9'=X9-1,X2'=X2+1;
init
  X1=1,X2=0,X3=0,X4=0,X5=0,X6=0,X7=0,X8=0,X9=0
target
  X7>=1,X8>=1

target in prolog : sat([X1,X2,X3,X4,X5,X6,s(X7),s(X8),X9])

*/

trans([s(X1),X2,X3,X4,X5,X6,X7,X8,X9],[X1,s(X2),X3,X4,X5,X6,X7,X8,X9]).
trans([X1,s(X2),X3,X4,X5,X6,X7,X8,X9],[X1,X2,s(X3),X4,X5,X6,X7,X8,X9]).
trans([X1,X2,s(X3),X4,X5,X6,X7,X8,X9],[X1,X2,X3,s(X4),X5,X6,X7,X8,X9]).
trans([X1,X2,X3,s(X4),X5,X6,X7,X8,X9],[X1,X2,X3,X4,s(X5),X6,X7,X8,X9]).
trans([X1,X2,X3,s(X4),X5,X6,X7,X8,X9],[X1,X2,X3,X4,X5,X6,X7,X8,s(X9)]).
trans([X1,X2,X3,X4,s(X5),X6,X7,X8,X9],[X1,X2,X3,X4,X5,s(X6),X7,X8,X9]).
trans([X1,X2,X3,X4,X5,s(X6),X7,X8,X9],[X1,X2,X3,X4,X5,X6,X7,X8,s(X9)]).
trans([X1,X2,X3,X4,X5,s(X6),X7,X8,X9],[X1,X2,X3,X4,X5,X6,s(X7),X8,X9]).
trans([X1,X2,X3,X4,X5,s(X6),X7,X8,X9],[X1,X2,X3,X4,X5,X6,X7,s(X8),X9]).
trans([X1,X2,X3,X4,X5,X6,s(X7),X8,X9],[X1,X2,X3,X4,X5,X6,X7,X8,s(X9)]).
trans([X1,X2,X3,s(X4),X5,X6,X7,s(X8),X9],[X1,X2,X3,X4,X5,X6,X7,X8,s(X9)]).
trans([X1,X2,X3,X4,X5,X6,X7,X8,s(X9)],[X1,s(X2),X3,X4,X5,X6,X7,X8,X9]).

p([s(0),0,0,0,0,0,0,0,0]).

sat([X1,X2,X3,X4,X5,X6,X7,X8,X9]) :- p([X1,X2,X3,X4,X5,X6,X7,X8,X9]).
sat([X1,X2,X3,X4,X5,X6,X7,X8,X9]) :- trans([Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9],[X1,X2,X3,X4,X5,X6,X7,X8,X9]), sat([Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9]).

