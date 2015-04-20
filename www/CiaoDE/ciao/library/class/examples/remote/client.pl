
:- module(client,[main/0],[actmods,objects]).

%:- use_module(library('actmods/webbased_locate')).
:- use_module(library('actmods/filebased_locate')).

:- use_class(library('class/examples/stack')). % this should do everything!

main:-
	X new stack,
	display(X), nl,
	X:push(a),
	X:pop(E),
	display(E), nl.
