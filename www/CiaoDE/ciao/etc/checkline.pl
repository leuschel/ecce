:- module(checkline,_,[]).

:- use_module(library(format)).
:- use_module(library(file_utils)).
:- use_module(library(lists)).



main(Args) :-
	process_args(Args, Params),
	do_process(Params).

process_args(['-h'|Args], Params) :-
	show_help,
	process_args(Args, Params).

process_args(['-l',ASize|Args], params(FileName, Size)) :-
	atom_codes(ASize, SSize),
	number_codes(Size, SSize),
	process_args(Args, params(FileName, Size)).

process_args([FileName|Args], params(FileName, Size)) :-
	process_args(Args, params(FileName, Size)).

process_args([], params(_FileName, Size)) :-
	(   var(Size) ->
	    Size = 80
	;
	    true
	).

do_process(params(FileName, Size)) :-
	(   var(FileName) ->
	    show_help
	;
	    (
		FileName == '-' ->
		current_input(CI),
		stream_to_string(CI,String)
	    ;
		file_to_string(FileName, String)
	    ),
	    process_string(String, Size, LinesTooLong),
	    (   LinesTooLong \== [] ->
		format("~w:lines too long: ~w \n", [FileName, LinesTooLong])
	    ;
		true
	    )
	).

show_help :-
	current_output(CO),
	format(CO, "Usage: checkline [-l Chars] -|FileName \n"||
	      "Chars is the max number of characters. By default 80.\n"||
	      "FileName is the file name to verify.\n"||
	      "\'-\' Is used to indicate that the standard input will be used.\n",[]).

process_file(FileName, Size, LinesTooLong) :-
	file_to_string(FileName, String),
	process_string(String, Size, LinesTooLong).

process_string(String, Size, LinesTooLong) :-
	process_string_(1, String, Size, [], LinesTooLong).

process_string_(N, String, Size, L0, L) :-
	append(Pre, "\n"||Post, String),!,
%	list_concat([Pre, "\n", Post], String),!,
	process_line(N, Pre, Size, L0, L1),
	N2 is N + 1,
	process_string_(N2, Post, Size, L1, L).
process_string_(N, String, Size, L0, L) :-
	process_line(N, String, Size, L0, L).

process_line(N, Pre, Size, L0, L) :-
	length(Pre, M),
	(   Size < M ->
	    L = [N|L0]
	;
	    L = L0
	).
