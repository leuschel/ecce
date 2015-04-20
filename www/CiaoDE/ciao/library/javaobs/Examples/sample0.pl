:- module(sample0,[],[objects]).

:- export(sample0/0).

:- use_class(library('javaobs/java/lang/String')).
:- use_class(library('javaobs/java/lang/Class')).
:- use_module(library(format)).

      
sample0:-
	S new 'String'("This is a test"),
	S:substring(5,SubS),
	nl,
	format(">>~s~n",[SubS]).
