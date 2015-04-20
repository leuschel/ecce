:- module(_, _, [argnamesvv]).

/*
?- create_pxy(P,3,4), create_circ(C, 10), show(P), show(C).
I am the point (3,4)
I am the circle with radius 10

C = circle(10),
P = point(3,4) ? 

yes
?- 
*/

is_p(P) :- P$point:{}.
is_circ(P) :- P$point:{}.
create_pxy(P, X, Y) :- P$point:{x=>X, y=>Y}.
create_circ(P, R) :- P$circle:{radius=>R}.
show(P) :- P$_:display.
anyobj(P) :- P$_:{}.
anyobjmet(P,T,M) :- P$T:M.
p_get(X,P,V) :- P$point:{X=>V}.

:- push_prolog_flag(multi_arity_warnings,off).

:- functor_class point/2.

_$point:{}.
point(X,_)$point:{x=>X}.
point(_,Y)$point:{y=>Y}.
This$point:{(A,B)} :-
	This$point:{A},
	This$point:{B}.
point(X,Y)$point:values({x=>X,y=>Y}).
This$point:display :-
	This$point:{x=>X, y=>Y},
	display('I am the point ('),
	display(X),
	display(','),
	display(Y),
	display(')\n').

:- functor_class point3/3.

_$point3:{}.
point3(X,_,_)$point3:{x=>X}.
point3(_,Y,_)$point3:{y=>Y}.
point3(_,_,Z)$point3:{z=>Z}.
This$point3:{(A,B)} :-
	This$point3:{A},
	This$point3:{B}.
point3(X,Y,Z)$point3:values({x=>X,y=>Y,z=>Z}).

:- functor_class circle/1.

_$circle:{}.
circle(R)$circle:{radius=>R}.
This$circle:display :-
	This$circle:{radius=>R},
	display('I am the circle with radius '),
	display(R),
	display('\n').

:- pop_prolog_flag(multi_arity_warnings).

/* MODULOS PARAMETRICOS CON VARIABLES GLOBALES MONOTONAS?? */


u(point, point(X, _), x, X).
u(point, point(_, Y), y, Y).

d(point(X, Y)):- display(punto(X,Y)).
d(circle(X)):- display(circulo(X)).

/*

:- term_class(f).

$(f, {}, T) :-
	T$f:{}.

f(_)$f:{}.

'$argnames_method'(This, Str, Method) :-
	functor(This, Str, _),
	'$argnames_method_2'(Str, This, Method).

'$argnames_method_2'(f, This, Method) :-
	'$argnames_method_f'(Method, This).

'$argnames_method_f'({}, This) :-
	'$argnames_method_f_{}'(This).

*/
