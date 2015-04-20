:- module(internal_types,
	  [vrml/2,
	   stream/1,
	   viewer/2]).

:- [ciaocompat].

:- include(library(assertions)).
:- include(library(basicmodes)).
:- include(library(types)).
:- include(library(isomodes)).

:- comment(module,"These are the internal data types used in the predicates.
They are only used to simplify this documentation and make it more
understandable.


   Implemented by G@..{o}ran Smedb@..{a}ck").


:- use_module(library(basicprops)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- type labels(Labels) ; "@var{Labels} is a term of labels:
   @includedef{labels/1}".

labels(labels(X,Y,Z)) :-
	atm(X),
	atm(Y),
	atm(Z).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- type fdvar(X) ; "@var{X} is a finite domain variable.".

fdvar(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- type vrml(S,Viewers) ; "@var{S} is a Stream, @var{Viewers} is list of 
                           viewer structures.".

vrml(S, Viewers) :-
	stream(S),
	list(Viewers).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- type stream(X) ; "@var{X} is a stream for input/output.".

stream(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- type viewer(Nr, Status) ; "@var{Nr} is the viewer number and Status 
                              is a status field.".

viewer(Nr, _) :-
	number(Nr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- type command ; "The command consists of a number of different
                               arities, and is used for communication.".
command(A).
command(A,B).
