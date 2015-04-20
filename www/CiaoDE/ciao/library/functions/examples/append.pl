:- module(_,_,[classic,functions]).

:- op(200,xfy,[::]).

:- function :: /2. 
A :: B := ~append(A,B).

testapp :- 
	set_prolog_flag(write_strings,on),
	X = " my ",
	write("Hello" :: X :: "world!").


