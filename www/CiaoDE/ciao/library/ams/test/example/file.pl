
:- module(file,[p/2],[]).

p(X,Y):- name(X,Y).
p(X,(F,A)):- functor(X,F,A).
p(X,Y):- name(X,[Y|_]).
p(X,Y):- name(X,A), A=[B|_], name(Y,[B|A]).
