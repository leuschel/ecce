:- module(_,[],[]).

:- use_module(library(operators)).

:- initialization(define_ops).

define_ops :-
        op(200, fy, [(?),(@),(@@)]).
