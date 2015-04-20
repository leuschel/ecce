

p(a,b).
p(a,c).
p(b,c).
p(f(X),Y) :- p(X,Y).

test(X) :- p(X,b).


p(a).
p(b).
p(c(X)) :- p(X).

test2(X) :- p(X).
test3(X) :- p(c(c(X))).


q(a).
q(b).
q(c(X)) :- p(a,X).
test4(X) :- q(X).



t(skip).
t(X) :- int(X), t(s(X)).

int(0).
int(s(X)) :- t(X).

test5 :- t(0).



u(a).
u(X) :- u(f(X)).

test6 :- u(b).  /* should fail */


v(a).
v(X) :- v2(X).
v(X) :- v(f(X)).

v2(a).
v2(f(X)) :- v2(X).

test7 :- v(b). /* should fail */
test8 :- v(f(a)). /* does not fail */



w(X) :- w(f(X)).
w(X) :- X \= b, w2(X).

w2(a).
w2(b).
w2(f(X)) :- X \=b, w2(X).

test9 :- w(a).
test10 :- w(b).