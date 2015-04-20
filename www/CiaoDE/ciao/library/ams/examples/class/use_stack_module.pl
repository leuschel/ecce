
:- module(use_stack_module,[test/0],[objects]).

:- use_class('stack.orig').

test :-
        X new stack,
        X:push(a),
        X:push(b),
        X:pop(E1), display(E1),
        X:pop(E2), display(E2),
        X:is_empty.
