
:- syntax([assertions,types]).

:- typedef foo ::= ^a;^b.

:- typedef bar ::= ^c;^d.

:- prop q/1.

q(a).
q(b).

:- true type p/1 ; "hello".

p(aba).
p(bab).

:- pred main/0.

main :-
	foo(X),
	display(X).
