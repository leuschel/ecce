:- module(file_utils, [file_terms/2, copy_stdout/1, 
	  file_to_string/2,
	  file_to_string/3,
	  string_to_file/2,
	  stream_to_string/2,
	  stream_to_string/3],
        [assertions,isomodes]).

:- use_module(library(read), [read/1]).
:- use_module(library(streams)).
:- use_module(library(strings)).

:- comment(title,"File I/O utilities").

:- comment(author,"The CLIP Group").

:- comment(module,"This module implements the file I/O utilities.").

:- pred file_terms(@File, ?Terms) => sourcename * list 
   # "Transform a file @var{File} to/from a list of terms @var{Terms}.".

:- pred file_terms(File, Terms) : sourcename * var => sourcename * list 
   # "Unifies @var{Terms} with the list of all terms in @var{File}.".

:- pred file_terms(File, Terms) : sourcename * list => sourcename * list 
   # "Writes the terms in list @var{Terms} (including the ending '.')
      onto file @var{File}.".

file_terms(File, Terms) :- var(Terms), !,
        open_input(File, IO),
        read(T),
        read_terms(T, Terms),
        close_input(IO).
file_terms(File, Terms) :-
        open_output(File, IO),
        display_term_list(Terms),
        close_output(IO).        

read_terms(end_of_file, []) :- !.
read_terms(T, [T|Ts]) :-
        read(T1),
        read_terms(T1, Ts).

display_term_list([]).
display_term_list([T|Ts]) :-
        display_term(T),
        display_term_list(Ts).

:- pred copy_stdout(+File) => sourcename 
   # "Copies file @var{File} to standard output.".

copy_stdout(File) :-
 	open_input(File, IO),
	repeat,
	  get_code(Code),
	  ( Code = -1
	  ; put_code(Code),
	    fail
	  ),
	!,
	close_input(IO).

:- pred file_to_string(+FileName, -String) :: sourcename * string
   # "Reads all the characters from the file @var{FileName}
      and returns them in @var{String}.".

file_to_string(File, String) :-
	file_to_string(File, String, []).

:- pred file_to_string(+FileName, -String, ?Tail) :: sourcename *
   string * term # "Reads all the characters from the file
   @var{FileName} and returns them in @var{String}.  @var{Tail} is the
   end of @var{String}.".

file_to_string(File, String, Tail) :-
        open(File, read, Stream),
        stream_to_string(Stream, String, Tail).

string_to_file(String, File) :-
	open(File, write, Stream),
	write_string(Stream, String),
	close(Stream).

:- pred stream_to_string(+Stream, -String) :: stream * string # "Reads
   all the characters from @var{Stream} and returns them in
   @var{String}.".

stream_to_string(Stream, String) :-
	stream_to_string(Stream, String, []).

:- pred stream_to_string(+Stream, -String) :: stream * string # "Reads
   all the characters from @var{Stream} and returns them in
   @var{String}.  @var{Tail} is the end of @var{String}".

stream_to_string(Stream, String, Tail) :-
        current_input(OldIn),
        set_input(Stream),
        read_to_close(String, Tail),
        set_input(OldIn),
        close(Stream).

read_to_close(L, T) :-
        get_code(C),
        read_to_close1(C, L, T).

read_to_close1(-1, T, T) :- !.
read_to_close1(C, [C|L], T) :-
        get_code(C1),
        read_to_close1(C1, L, T).
