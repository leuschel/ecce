
:- module(active,[q/2],[]).

q(X,Y):- name(X,Y).
q(X,(F,A)):- functor(X,F,A).
q(X,Y):- name(X,[Y|_]).
q(X,Y):- name(X,A), A=[B|_], name(Y,[B|A]).
