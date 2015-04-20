/* Example basicME

vars
    x0 x1 x2 x3 x4

rules
    x0 >= 1,
    x1 >= 1,
    x2 >= 1 ->
		    x0' = x0-1,
		    x2' = x2-1,
		    x3' = x3+1;

    x0 >= 1,
    x1 >= 1,
    x2 >= 1 ->
		    x0' = x0-1,
		    x1' = x1-1,
		    x4' = x4+1;

    x3 >= 1 ->
		    x0' = x0+1,
		    x2' = x2+1,
		    x3' = x3-1;

    x4 >= 1 ->
		    x0' = x0+1,
		    x1' = x1+1,
		    x4' = x4-1;
init
    x0 >= 1, x1 = 1, x2 = 1, x3 = 0, x4 = 0

target
    x3 >= 1, x4 >= 1

prolog target :
forward : sat([X0,X1,X2,s(X3),s(X4)])
backward : p([s(X0),s(0),s(0),0,0])

*/

trans([s(X0),s(X1),s(X2),X3,X4],[X0,s(X1),X2,s(X3),X4]).
trans([s(X0),s(X1),s(X2),X3,X4],[X0,X1,s(X2),X3,s(X4)]).
trans([X0,X1,X2,s(X3),X4],[s(X0),X1,s(X2),X3,X4]).
trans([X0,X1,X2,X3,s(X4)],[s(X0),s(X1),X2,X3,X4]).



start([s(X0),s(0),s(0),0,0]).

unsafe([X0,X1,X2,s(X3),s(X4)]).
unsafe([X0,X1,X2,s(s(X3)),X4]).
unsafe([X0,X1,X2,X3,s(s(X4))]).



forward :- start(X), forward(X).
forward(X) :- unsafe(X).
forward(X) :- trans(X,Y), forward(Y).

backward :- unsafe(X), backward(X).
backward(X) :- start(X).
backward(X) :- trans(Y,X), backward(Y).

