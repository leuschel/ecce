% regexp(C) :- int(C). % A char
% regexp([E|Es]) :- regexp(E), regexp(Es).
% regexp(*(S)) :- regexp(S).
% regexp(+(S)) :- regexp(S).
% regexp(?(S)) :- regexp(S).
% regexp(any(Chars)) :- 
% regexp(anynot(Chars))
% regexp(or(Alternatives)) :- list(Alternatives, regexp).
% regexp(#(S)) :- regexp(S).

:- use_package(dcg).

match([]) --> [].
match([E|Es]) --> match(E), match(Es).
match(@) --> [_].
match(Ch) --> [Ch].
match(+(E)) --> match(E), match(*(E)).
match(*(E)) --> match(E), match(*(E)).
match(*(_)) --> [].
match(?(E)) --> match(E).
match(?(_)) --> [].
match(;(E,_)) --> match(E).
match(;(_,E)) --> match(E).
