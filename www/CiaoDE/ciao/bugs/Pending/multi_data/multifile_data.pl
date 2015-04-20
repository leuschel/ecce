% declaring a prdicate multifile and data does not work
% Comment multifile declaration to check it out.
:- module( _ , _ , [] ).

%:- dynamic aa/1. % this also fails
:- data aa/1.
:- multifile aa/1.

main:- go.

aa( 10 ).

go :- data_facts:retract_fact( aa(10 ) ).
