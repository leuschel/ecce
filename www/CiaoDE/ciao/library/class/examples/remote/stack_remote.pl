:- module(stack_remote,[],[objects]).

:- use_class(library('class/examples/stack')).

:- export(new/2).

new(A,B):- objects_rt:new(A,B).

:- export('class$rused'/2).

'class$rused'(A,B):- 'class$used'(A,B).

:- export('class$rcall'/3).

'class$rcall'(A,B,C):- 'class$call'(A,B,C).
