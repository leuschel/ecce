/*
 
example : Reader-Writer 

vars
  x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 
rules
  x0>=1              ->  x0'=x0-1,x1'=x1+1;
  x1>=1,x4>=1        ->  x0'=x0+1,x1'=x1-1,x3'=x3+1,x4'=x4-1;
  x2>=1,x11>=1       ->  x1'=x1+1,x2'=x2-1,x11'=x11-1;
  x1>=1              ->  x1'=x1-1,x2'=x2+1,x5'=x5+1,x9'=x9+1;
  x6>=1              ->  x6'=x6-1,x7'=x7+1;
  x3>=1,x7>=1        ->  x3'=x3-1,x4'=x4+1,x6'=x6+1,x7'=x7-1;
  x8>=1,x12>=1       ->  x4'=x4+5,x7'=x7+1,x8'=x8-1,x12'=x12-1;
  x4>=5,x5>=1,x7>=1  ->  x4'=x4-5,x5'=x5-1,x7'=x7-1,x8'=x8+1,x10'=x10+1;
  x9>=1,x10>=1       ->  x9'=x9-1,x10'=x10-1,x11'=x11+1,x12'=x12+1;
init
  x0=0,x1=0,x2=1,x3=0,x4=0,x5=0,x6=0,x7=0,x8=1,x9=0,x10=0,x11=1,x12=1
target
  x3>=1,x10>=1

target in prolog : 
prolog target :
forward : sat([X0,X1,X2,s(X3),X4,X5,X6,X7,X8,X9,s(X10),X11,X12])
backward : p([0,0,s(0),0,0,0,0,0,s(0),0,0,s(0),s(0)])

*/

*/

trans([s(X0),X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12],[X0,s(X1),X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12]).
trans([X0,s(X1),X2,X3,s(X4),X5,X6,X7,X8,X9,X10,X11,X12],[s(X0),X1,X2,s(X3),X4,X5,X6,X7,X8,X9,X10,X11,X12]).
trans([X0,X1,s(X2),X3,X4,X5,X6,X7,X8,X9,X10,s(X11),X12],[X0,s(X1),X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12]).
trans([X0,s(X1),X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12],[X0,X1,s(X2),X3,X4,s(X5),X6,X7,X8,s(X9),X10,X11,X12]).
trans([X0,X1,X2,X3,X4,X5,s(X6),X7,X8,X9,X10,X11,X12],[X0,X1,X2,X3,X4,X5,X6,s(X7),X8,X9,X10,X11,X12]).
trans([X0,X1,X2,s(X3),X4,X5,X6,s(X7),X8,X9,X10,X11,X12],[X0,X1,X2,X3,s(X4),X5,s(X6),X7,X8,X9,X10,X11,X12]).
trans([X0,X1,X2,X3,X4,X5,X6,X7,s(X8),X9,X10,X11,s(X12)],[X0,X1,X2,X3,s(s(s(s(s(X4))))),X5,X6,s(X7),X8,X9,X10,X11,X12]).
trans([X0,X1,X2,X3,s(s(s(s(s(X4))))),s(X5),X6,s(X7),X8,X9,X10,X11,X12],[X0,X1,X2,X3,X4,X5,X6,X7,s(X8),X9,s(X10),X11,X12]).
trans([X0,X1,X2,X3,X4,X5,X6,X7,X8,s(X9),s(X10),X11,X12],[X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,s(X11),s(X12)]).

prop([X0,X1,X2,s(X3),X4,X5,X6,X7,X8,X9,s(X10),X11,X12],unsafe).

/*init forward 
p([0,0,s(0),0,0,0,0,0,s(0),0,0,s(0),s(0)]).
*/

/*init backward :*/
sat([X0,X1,X2,s(X3),X4,X5,X6,X7,X8,X9,s(X10),X11,X12]).


/*  forward :
sat(X) :- p(X).
sat(X) :-trans(Y,X),sat(Y).
*/
/* backward */
p(X):- sat(X).
sat(X) :-trans(X,Y),sat(Y).


