:- module(defineq,[q/2],[andorra]).


:- determinate(q(_X,Y),ground(Y)).

q(1,f(2)):- display(q(1,f(2))),nl.
q(1,f(1)):- display(q(1,f(1))),nl.
