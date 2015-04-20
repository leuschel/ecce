%% #!/bin/sh
%% exec ciao-shell $0 "$@"

%% Ciao syntax
:- use_package(assertions).

%% ISO Compat
:- use_module(library(dynamic)).

%% Ciao libraries
:- use_module(library('compiler/c_itf')).
:- use_module(library(compiler),[ use_module/3 ]).

:- comment(title,"Gathering the dependent files for a file").

:- comment(author,"Daniel Cabeza").
:- comment(author,"Manuel Hermenegildo").

:- comment(module,"This simple program takes a single Ciao or Prolog
   source filename (which is typically the main file of an
   application). It prints out the list of all the @concept{dependent
   files}, i.e., all files needed in order to build the application,
   including those which reside in libraries. This is particularly
   useful in Makefiles, for @concept{building standalone
   distributions} (e.g., @concept{.tar files}) automatically.

   The filename should be followed by other arguments which will be taken
   to be library directory paths in which to look for files used by
   the file being analyzed. 

   @section{Usage (get_deps)}

   @begin{verbatim}
   @includefact{usage_text/1}
   @end{verbatim}

").

main(Args) :- 
	handle_args(Args).

handle_args(['-h'|_]) :- !,
	usage.
handle_args(['-u',File|Args]) :-
        use_module(File, all, c_itf),
	handle_args(Args).
handle_args(Args):-
	process_args(Args).

process_args([File|Libs]) :-
	!,
	retractall(library_directory(_)),
	set_lib_dirs(Libs),
	substract_pl(File,Main,_Suffix),
	make_depends([Main], Depends, _IfError),
	print_filenames(Depends).
process_args(Args) :-
	inform_user(['error: invalid arguments ',Args]),
	nl(user_error),
	usage.

usage :-
	usage_text(TextS),
	atom_codes(Text,TextS),
	inform_user(['Usage: ']),
	inform_user([Text]).

usage_text("
	get_deps [-u <filename>] <filename> [lib_dir1] ... [lib_dirN]
           : return dependent files for <filename> 
             found in [lib_dir1] ... [lib_dirN]

	get_deps -h
           : print this information
").

print_filenames([]).
print_filenames([file(Base,_Dir,_Name)|Files]) :-
	display(user_output,Base),
	display(user_output,' '),
	print_filenames(Files).
	
:- multifile library_directory/1.
:- dynamic library_directory/1.

set_lib_dirs([]).
set_lib_dirs([H|T]) :- 
	assertz(library_directory(H)),
	set_lib_dirs(T).

substract_pl(FPL,F,'.pl') :-
	atom_codes(FPL,FPLS),
	append(FS,".pl",FPLS),
	!, %% it ends in .pl
	atom_codes(F,FS).
%% else, it does not end in .pl
substract_pl(F,F,'').

append([],X,X).
append([X|Xs],Ys,[X|Zs]) :- append(Xs,Ys,Zs).
