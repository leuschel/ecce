:- module(_, _, [argnamesvv]).

/*
?- myappend1(Xs,[1,2,3,4],Zs).

Ciao interruption (h for help)? a
{ Execution aborted }

:- functor_class '.'/2.
:- functor_class []/0.
*/

:- functor_class []/0.
:- functor_class '.'/2.

_$[]:append1(List, List).
[Head|Tail]$'.':append1(List, [Head|Tail2]) :-
	Tail$_:append1(List, Tail2).

[Head|Tail]$'.':append2(List, [Head|Tail2]) :-
	Tail$_:append2(List, Tail2).
_$[]:append2(List, List).

myappend1(Xs,Ys,Zs) :-
	Xs$_:append1(Ys,Zs).

myappend2(Xs,Ys,Zs) :-
	Xs$_:append2(Ys,Zs).

append1([],Xs,Xs).
append1([X|Xs],Ys,[X|Zs]) :-
	append1(Xs,Ys,Zs).

append2([X|Xs],Ys,[X|Zs]) :-
	append2(Xs,Ys,Zs).
append2([],Xs,Xs).
