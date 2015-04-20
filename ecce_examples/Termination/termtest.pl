
p(X,X).
p(s(X),Y) :- p(X,s(Y)).

t(X) :-  p(s(s(X)),X),  q(X).
tz(X) :-  p(s(s(X)),X),  z(X).


q(0).
q(s(X)) :- q(X).

z(0).





tz(X,Y) :-  p(s(s(X)),Y),  z(X), z(Y).