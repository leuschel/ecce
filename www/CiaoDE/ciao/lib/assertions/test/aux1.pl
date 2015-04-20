
:- module(aux1,[p/1],[assertions]).

:- pred p(X) : var(X) => ground(X) # "Is also nice.".

p(a).

:- pred q/2 # "Is very nice.".

q(a,b).
