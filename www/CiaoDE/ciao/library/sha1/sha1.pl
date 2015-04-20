:- module(sha1, 
	[
	    sha1/2
	], 
	[foreign_interface, assertions, isomodes]).

:- comment(title, "SHA-1 implementation").
:- comment(author, "José Manuel Gómez").

:- comment(module, "This module provides predicates for generating
        hash keys according to SHA-1 specifications").

:- true pred sha1(in(String), go(Key)) :: 
	string * string + (foreign(sha1_c),returns(Key)) #

	"@var{Key} is the corresponding key of string @var{String}
	according to the SHA-1 algorithm.".

:- use_foreign_source(sha1_c).
