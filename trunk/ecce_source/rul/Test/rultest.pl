

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


t6(skip).
t6(X) :- t6(s(X)).

test6 :- t6(0).