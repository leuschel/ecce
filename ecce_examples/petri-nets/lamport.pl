/*

example : Lamport ME

vars
x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10
rules
 x0>=1,x3>=1  ->  x0'=x0-1,x1'=x1+1,x3'=x3-1,x4'=x4+1;
 x1>=1,x4>=1  ->  x1'=x1-1,x2'=x2+1,x3'=x3+1,x4'=x4-1;
 x2>=1,x5>=1  ->  x0'=x0+1,x2'=x2-1;
 x3>=1,x8>=1  ->  x6'=x6+1,x8'=x8-1;
 x6>=1        ->  x5'=x5+1,x6'=x6-1,x7'=x7+1;
 x4>=1,x7>=1  ->  x7'=x7-1,x10'=x10+1;
 x4>=1,x8>=1  ->  x8'=x8-1,x9'=x9+1;
 x9>=1        ->  x5'=x5+1,x9'=x9-1,x10'=x10+1;
 x5>=1,x10>=1 ->  x5'=x5-1,x8'=x8+1,x10'=x10-1;
init
 x0=0,x1=1,x2=0,x3=0,x4=1,x5=1,x6=0,x7=0,x8=0,x9=0,x10=1
target
 x0>=1,x9>=1

target en prolog : 
  sat([s(X0),X1,X2,X3,X4,X5,X6,X7,X8,s(X9),X10]) if forward
  p([0,s(0),0,0,s(0),s(0),0,0,0,0,s(0)]) if backward

*/

trans([s(X0),X1,X2,s(X3),X4,X5,X6,X7,X8,X9,X10],[X0,s(X1),X2,X3,s(X4),X5,X6,X7,X8,X9,X10]).
trans([X0,s(X1),X2,X3,s(X4),X5,X6,X7,X8,X9,X10],[X0,X1,s(X2),s(X3),X4,X5,X6,X7,X8,X9,X10]).
trans([X0,X1,s(X2),X3,X4,s(X5),X6,X7,X8,X9,X10],[s(X0),X1,X2,X3,X4,s(X5),X6,X7,X8,X9,X10]).
trans([X0,X1,X2,s(X3),X4,X5,X6,X7,s(X8),X9,X10],[X0,X1,X2,s(X3),X4,X5,s(X6),X7,X8,X9,X10]).
trans([X0,X1,X2,X3,X4,X5,s(X6),X7,X8,X9,X10],[X0,X1,X2,X3,X4,s(X5),X6,s(X7),X8,X9,X10]).
trans([X0,X1,X2,X3,s(X4),X5,X6,s(X7),X8,X9,X10],[X0,X1,X2,X3,s(X4),X5,X6,X7,X8,X9,s(X10)]).
trans([X0,X1,X2,X3,s(X4),X5,X6,X7,s(X8),X9,X10],[X0,X1,X2,X3,s(X4),X5,X6,X7,X8,s(X9),X10]).
trans([X0,X1,X2,X3,X4,X5,X6,X7,X8,s(X9),X10],[X0,X1,X2,X3,X4,s(X5),X6,X7,X8,X9,s(X10)]).
trans([X0,X1,X2,X3,X4,s(X5),X6,X7,X8,X9,s(X10)],[X0,X1,X2,X3,X4,X5,X6,X7,s(X8),X9,X10]).
/* init forward
p([0,s(0),0,0,s(0),s(0),0,0,0,0,s(0)]).
*/
/* init backward */
sat([s(X0),X1,X2,X3,X4,X5,X6,X7,X8,s(X9),X10]).

/*  forward :
sat([X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10]) :- p([X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10]).
sat([X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10]) :- trans([Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10],[X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10]), sat([Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10]).
*/
/* backward */
p([X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10]):- sat([X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10]).
sat([X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10]) :- trans([X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10], [Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10]), sat([Y0,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10]).



