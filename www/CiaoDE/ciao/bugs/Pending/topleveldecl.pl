
/* 

Load this file and:

?- data(X).

no
?- user:data(X).

X = a ? 

Presumably because data/1 is a declaration and interpreted by the top
level. We should issue a warning upon loading a file that defines a
predicate like this so that the user remembers to qualify.

*/


data(a).
