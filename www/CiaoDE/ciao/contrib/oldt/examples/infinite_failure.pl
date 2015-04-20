
:- dynamic p/1.

p(X):- p(X).
p(_):- p(a).
p(a):- p(_).

%% :- oldt(user:p(X)).
