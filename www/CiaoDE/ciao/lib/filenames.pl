%%-------------------------------------------------------------------------

:- module( filenames, 
	   [ no_path_file_name/2, 
	     file_name_extension/3,
	     basename/2, 
	     extension/2 ],
	   [assertions] ).

%%-------------------------------------------------------------------------

:- use_module(library(lists), [append/3]).

%%-------------------------------------------------------------------------

:- comment(title,"File name manipulation").

:- comment(author, "Daniel Cabeza").
:- comment(author, "Angel Fernandez Pineda").

:- comment(module,
	"This library provides some small utilities to handle file name
         syntax.").

%%-------------------------------------------------------------------------

:- prop atom_or_str(X) + regtype
	# "@var{X} is an atom or a string".

atom_or_str(X) :- string(X).
atom_or_str(X) :- atm(X).

%%-------------------------------------------------------------------------

:- comment(no_path_file_name/2,
	"This predicate will extract the last item (usually the file name)
         from a given path.

         The first argument must be instantiated to a string or atom.
         Whenever the first argument is an atom, the second argument
         will be an atom. Whenever the first argument is a string, the
         second argument will be a string.

         This predicate will fail under any of the following conditions:
         @begin{itemize}
          @item First argument is not an atom, nor a string.
          @item Second argument is not the last given path item 
                (given path is the first argument).
         @end{itemize}

         Those are the most usual usages of no_path_file_name/2:
@begin{verbatim}
?- no_path_file_name(""/home/nexusV/somefile.txt"",K).

K = ""somefile.txt"" ? 

yes
?- no_path_file_name('/home/nexusV/somefile.txt',K).

K = 'somefile.txt' ? 

yes
?- 
@end{verbatim}
	").

:- pred no_path_file_name(Path,FileName) :: atom_or_str * atom_or_str
	# "@var{FileName} is the file corresponding to the given 
           @var{Path}.".

no_path_file_name(P,F) :-
	atom(P),
	atom_codes(P,PasStr),
	!,
	no_path_file_name_str(PasStr,FasStr),
	atom_codes(F,FasStr).

no_path_file_name(P,F) :-
	no_path_file_name_str(P,F).

no_path_file_name_str(P, F) :-
        append(_, [0'/|R], P), !,
        no_path_file_name_str(R, F).

no_path_file_name_str(F, F).


%%-------------------------------------------------------------------------

:- pred file_name_extension(FileName,BaseName,Extension) ::
	atom_or_str * atom_or_str * atom_or_str
        # "Splits a @var{FileName} into its @var{BaseName} and 
           @var{Extension}.". 

:- comment(file_name_extension/3,
	"This predicate may be used in two ways:
         @begin{itemize}

          @item To create a file name from its components: name and extension.
                For instance:

@begin{verbatim}
?- file_name_extension(File,mywork,'.txt').

File = 'mywork.txt' ? 

yes
?- 
@end{verbatim}

          @item To split a file name into its name and extension.
                For Instance:
@begin{verbatim}
?- file_name_extension('mywork.txt',A,B).

A = mywork,
B = '.txt' ? 

yes
?- 
@end{verbatim}
         @end{itemize}

        Any other usage of file_name_extension/3 will cause the predicate
        to fail. Notice that valid arguments are accepted both as atoms or 
        strings.
        ").

file_name_extension(File,Name,Ext) :-
	atom(File),
	!,
	atom_codes(File,FileAsStr),
	file_name_extension_str(FileAsStr,NameAsStr,ExtAsStr),
	atom_codes(Name,NameAsStr),
	atom_codes(Ext,ExtAsStr).

file_name_extension(File,Name,Ext) :-
	atom(Name),
	atom(Ext),
	!,
	atom_concat(Name,Ext,File).

file_name_extension(File,Name,Ext) :-
	file_name_extension_str(File,Name,Ext).

file_name_extension_str(File,Name,Ext) :-
	nonvar(File),
	!,
	(
	    Ext = [0'.|X],
	    append(Name, Ext, File),
	    \+ member(0'., X) -> true
	;
	    Ext = [],
	    Name = File
	).

file_name_extension_str(File,Name,Ext) :-
	nonvar(Name),
	nonvar(Ext),
	!,
	append(Name,Ext,File).

:- comment(basename(FileName,BaseName),"@var{BaseName} is
           @var{FileName} without extension. Equivalent to
           @tt{file_name_extension(FileName,BaseName,_)}. Useful to
           extract the base name of a file using functional syntax.").

:- true pred basename(atom_or_str,atom_or_str).

basename(FileName,BaseName) :-
	file_name_extension(FileName,BaseName,_).

:- comment(extension(FileName,Extension),"@var{Extension} is the
           extension (suffix) of @var{FileName}. Equivalent to
           @tt{file_name_extension(FileName,_,Extension)}. Useful to
           extract the extension of a file using functional syntax.").

:- true pred extension(atom_or_str,atom_or_str).

extension(FileName,Extension) :-
	file_name_extension(FileName,_,Extension).
