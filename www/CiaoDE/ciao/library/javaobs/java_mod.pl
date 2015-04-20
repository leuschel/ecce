%%------------------------------------------------------------------------
%%
%%
%% OPERATOR FILE FOR JAVA OBJECTS
%%
%% This file must be included in user programs.
%%
%%------------------------------------------------------------------------

:- use_module(library(lists), [append/3]).
:- use_module(javart).
:- use_module(java_inheritance).

:- op(700, xfy, [new]).
:- op(700, xfy, ['::']).

:- data      '$java_connection'/1.

%%
%% Java object creation.
%% Arguments
%%    -INSTANCE      Java object ID that identifies the created object.
%%    +Constructor   Structure containing the constructor of the java
%%                   object in the format JavaModule(Args).
%%
new(Instance, Constructor) :-
	var(Instance),
	nonvar(Constructor),
         ( '$java_connection'(_Host) -> true         
           ;
           javart:java_connection(clip),
           set_fact('$java_connection'(clip))),
           Constructor =.. [PrologName|Xs],
           append([PrologName|Xs], [Instance], Ys),
           Constructor2 =.. Ys,
           PrologName:Constructor2.

%%
%% Java object access. Provides access to java/prolog object fields and
%% methods.
%%
%% Arguments
%%    +INSTANCE      Java object ID.
%%    +ELEMENT       Method or field structure.
%%

'::'(Instance, Element) :-
	nonvar(Instance),
        java_inheritance:java_instance_of(Instance, PrologName),
        Element =.. [X|Xs],             %%
        append([X, Instance], Xs, Ys),  %% Instance Id concatenation
        Predicate =.. Ys,               %%
        PrologName:Predicate.
