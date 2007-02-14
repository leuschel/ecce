/* small test to check whether RUL package can represent
   even, odd information */

y(s(s(s(X)))) :- z(X).
y(X) :- y(s(s(X))).
z(s(s(X))) :- z(X).
z(0).

test :- y(0).