%%----------------------------------------------------------------------
%% O'CIAO
%%
%% EXAMPLE OF FILE-BASED PERSISTENT STREAMER
%%
%%----------------------------------------------------------------------

:- class(file_persistent_streamer).

:- implements(library('class/library/persistent_streamer')).

%%----------------------------------------------------------------------

:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(system)).

%%----------------------------------------------------------------------

:- data strm/1.
:- data cterm/1.
:- data file/1.
:- data now_doing/1.

%%----------------------------------------------------------------------
%% CONSTRUCTOR/DESTRUCTOR
%%----------------------------------------------------------------------

file_persistent_streamer(File) :-
	atom(File),
	asserta_fact(file(File)).

destructor :-
	retract_fact(strm(X)),
	close(X),
	!.
destructor.

%%----------------------------------------------------------------------
%% READING 
%%----------------------------------------------------------------------

current_term(Term) :-
	cterm(Term).

next_term :-
	now_doing(reading),
	strm(X),
	retractall_fact(cterm(_)),
	read_term(X,Term,[]),
	Term \== end_of_file,
	set_fact(cterm(Term)).
next_term :-
	cterm(_),
	!,
	next_term.

%%----------------------------------------------------------------------
%% WRITING
%%----------------------------------------------------------------------

write_term(Term) :-
	now_doing(writing),
	strm(X),
	write_term(X,Term,[quoted(true)]),
	write_term(X,'.',[]),nl(X),
	flush_output(X).

flush :-
	now_doing(writing),
	strm(X),
	flush_output(X).

%%----------------------------------------------------------------------
%% START/END OF OPERATION
%%----------------------------------------------------------------------

start_reading :-
	file(File),
	destructor,
	retractall_fact(cterm(_)),
	file_exists(File),
	open(File,read,Strm),
	assertz_fact(strm(Strm)),
	set_fact(now_doing(reading)),
	next_term.

start_writing :-
	file(File),
	destructor,
	retractall_fact(cterm(_)),
	open(File,write,Strm),
	assertz_fact(strm(Strm)),
	set_fact(now_doing(writing)).

end_reading :-
	destructor,
	retractall_fact(cterm(_)).

end_writing :-
	end_reading.
