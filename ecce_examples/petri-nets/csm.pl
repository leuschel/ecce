/*  csm spec
vars
  x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13
rules
  x0>=1		->	x0'=x0-1,x1'=x1+1;
  x4>=1,x9>=1	->	x0'=x0+1,x2'=x2+1,x4'=x4-1,x9'=x9-1;
  x4>=1,x6>=1	->	x0'=x0+1,x3'=x3+1,x4'=x4-1,x6'=x6-1;
  x1>=1,x3>=1	->	x1'=x1-1,x3'=x3-1,x5'=x5+1,x6'=x6+1;
  x1>=1,x2>=1	->	x1'=x1-1,x2'=x2-1,x5'=x5+1,x9'=x9+1;
  x5>=1		->	x4'=x4+1,x5'=x5-1;
  x10>=1		->	x7'=x7+1,x10'=x10-1;
  x7>=1		->	x7'=x7-1,x8'=x8+1;
  x6>=1,x8>=1	->	x6'=x6-1,x8'=x8-1,x9'=x9+1;
  x9>=1		->	x6'=x6+1,x9'=x9-1,x10'=x10+1;
  x10>=1		->	x10'=x10-1,x12'=x12+1;
  x11>=1		->	x8'=x8+1,x11'=x11-1,x13'=x13+1;
  x12>=1,x13>=1	->	x11'=x11+1,x12'=x12-1,x13'=x13-1;
init         
  x0=0,x1=0,x2=0,x3=0,x4=0,x5=1,x6=1,x7>=1,x8=0,x9=0,x10=0,x11=0,x12=0,x13=1
target
  x9>=2

prolog target :
forward : sat([X0,X1,X2,X3,X4,X5,X6,X7,X8,s(s(X9)),X10,X11,X12,X13])
backward : p([0,0,0,0,0,s(0),s(0),s(X7),0,0,0,0,0,s(0)])

*/


/* The transition relation */
trans(t1,[s(X0),X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13],[X0,s(X1),X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13]).
trans(t2,[X0,X1,X2,X3,s(X4),X5,X6,X7,X8,s(X9),X10,X11,X12,X13],[s(X0),X1,s(X2),X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13]).
trans(t3,[X0,X1,X2,X3,s(X4),X5,s(X6),X7,X8,X9,X10,X11,X12,X13],[s(X0),X1,X2,s(X3),X4,X5,X6,X7,X8,X9,X10,X11,X12,X13]).
trans(t4,[X0,s(X1),X2,s(X3),X4,X5,X6,X7,X8,X9,X10,X11,X12,X13],[X0,X1,X2,X3,X4,s(X5),s(X6),X7,X8,X9,X10,X11,X12,X13]).
trans(t5,[X0,s(X1),s(X2),X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13],[X0,X1,X2,X3,X4,s(X5),X6,X7,X8,s(X9),X10,X11,X12,X13]).
trans(t6,[X0,X1,X2,X3,X4,s(X5),X6,X7,X8,X9,X10,X11,X12,X13],[X0,X1,X2,X3,s(X4),X5,X6,X7,X8,X9,X10,X11,X12,X13]).
trans(t7,[X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,s(X10),X11,X12,X13],[X0,X1,X2,X3,X4,X5,X6,s(X7),X8,X9,X10,X11,X12,X13]).
trans(t8,[X0,X1,X2,X3,X4,X5,X6,s(X7),X8,X9,X10,X11,X12,X13],[X0,X1,X2,X3,X4,X5,X6,X7,s(X8),X9,X10,X11,X12,X13]).
trans(t9,[X0,X1,X2,X3,X4,X5,s(X6),X7,s(X8),X9,X10,X11,X12,X13],[X0,X1,X2,X3,X4,X5,X6,X7,X8,s(X9),X10,X11,X12,X13]).
trans(t10,[X0,X1,X2,X3,X4,X5,X6,X7,X8,s(X9),X10,X11,X12,X13],[X0,X1,X2,X3,X4,X5,s(X6),X7,X8,X9,s(X10),X11,X12,X13]).
trans(t11,[X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,s(X10),X11,X12,X13],[X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,s(X12),X13]).
trans(t12,[X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,s(X11),X12,X13],[X0,X1,X2,X3,X4,X5,X6,X7,s(X8),X9,X10,X11,X12,s(X13)]).
trans(t13,[X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,s(X12),s(X13)],[X0,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,s(X11),X12,X13]).

/* The basic properties */
prop([X0,X1,X2,X3,X4,X5,X6,X7,X8,s(s(X9)),X10,X11,X12,X13],unsafe).

/* The start state for the animator and for backwards analysis */
start([0,0,0,0,0,s(0),s(0),s(X7),0,0,0,0,0,s(0)]) :- X7 = 0.

/* Finite state model checking tasks */
finite_state_model_check(csm_1,Formula) :- X7 = s(0),
     Formula = sat([0,0,0,0,0,s(0),s(0),X7,0,0,0,0,0,s(0)],ef(p(unsafe))).
finite_state_model_check(csm_ex_2,Formula) :- X7 = s(s(s(s(0)))),
     Formula = sat([0,0,0,0,0,s(0),s(0),X7,0,0,0,0,0,s(0)],ef(p(unsafe))).

/* Parametric model checking tasks */
parametric_model_check(X7,Formula) :-
     Formula = sat([0,0,0,0,0,s(0),s(0),X7,0,0,0,0,0,s(0)],ef(p(unsafe))).
     
/* Infinite Model Checking Tasks */
infinite_model_check(csm_1,Formula) :-
     Formula = sat([0,0,0,0,0,s(0),s(0),X7,0,0,0,0,0,s(0)],ef(p(unsafe))).



