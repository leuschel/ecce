
%:- use_package('bf/af').
:- use_package(bf).

p(f(X))<- p(X), q(X).
p(a)<- .

q(b).


sumall([],0)<- .
sumall([X|Xs],S)<- sumall(Xs,S0), S is S0+X.
