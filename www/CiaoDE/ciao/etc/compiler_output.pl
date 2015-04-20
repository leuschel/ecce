:- module(compiler_output, [main/1], [assertions]).

:- comment(title,"Print out WAM code").

:- comment(author,"Manuel Carro").

:- comment(module, "This program prints to standard output a symbolic
   form of the (modified) Wam code the compiler generates for a given
   source file.  Directives are ignored.

   @section{Usage (compiler_output)}

   @begin{verbatim}
   @includefact{usage_text/1}
   @end{verbatim}

   ").

%:- use_module(library(dec10_io)).
:- use_module(library(read)).
:- use_module(library(system)).
:- use_module(library(debugger)).
:- use_module(engine(internals)).
:- use_module(library('compiler/pl2wam')).

message:-
	usage_text(T),
        display_string(T).

usage_text("
        Print (modified, partial) WAM code for a .pl file

        Usage: compiler_output <file.pl>
").

no_file(F):-
        nl,
        display('File '),
        display(F),
        display(' not found!'),
        message.

main([File]):- 
        !,
        (
            file_exists(File) ->
            set_compiler_mode(wam),
            set_compiler_out(user),
            nl,
            nl,
            open(File, read, FS),
            read(FS, Clause),
            emit_wam(Clause, FS),
            close(FS)
        ;
            no_file(File)
        ).
main(_):- message.
            

emit_wam(end_of_file, _FS):- !.
emit_wam((:- _Decl), FS):- !,
        read(FS, Clause),
        emit_wam(Clause, FS).
emit_wam((Head :- Body), FS):- !,
        call_in_module(pl2wam, compile_all(Head, Body, unprofiled)),
        read(FS, Clause),
        emit_wam(Clause, FS).
emit_wam(Fact, FS):- !,
        call_in_module(pl2wam, compile_all(Fact, true, unprofiled)),
        read(FS, Clause),
        emit_wam(Clause, FS).
