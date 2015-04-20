
:- use_package(pillow).

:- use_module(library(file_utils),[file_to_string/2]).
:- use_module(library(read)).
:- use_module(library(write)).

main:-
	repeat,
	   read(Source),
	   ( Source=end_of_file
	   -> true
	    ; ( Source=url(URL)
	      -> fetch_from_url(URL,String)
	       ; fetch_from_file(Source,String)
	      ),
	      show(String),
	      fail
	   ).

fetch_from_url(Source,String):-
	url_info(Source,URL),
	fetch_url(URL,[],Response),
	member(content(String),Response).

fetch_from_file(Source,String):-
	catch(file_to_string(Source,String),_,
	      atom_codes(Source,String) ).

show(Message):-
	prolog_flag(write_strings,X,on),
	writeq(Message), nl,
	prolog_flag(write_strings,_,X).
