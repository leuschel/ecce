:- module(streams, [
        open_null_stream/1,
        open_input/2, close_input/1, open_output/2, close_output/1
        ],[assertions]).

:- use_module(engine(internals)).

:- comment(title,"Structured stream handling").

open_null_stream(S) :-
	'$open'('/dev/null', w, S).

open_input(FileName, i(OldInput, NewInput)) :-
        current_input(OldInput),
        open(FileName, read, NewInput),
        set_input(NewInput).

close_input(i(OldInput, NewInput)) :- !,
        set_input(OldInput),
        close(NewInput).
close_input(X) :-
        throw(error(domain_error(open_input_handler, X), close_input/1-1)).

open_output(FileName, o(OldOutput, NewOutput)) :-
        current_output(OldOutput),
        open(FileName, write, NewOutput),
        set_output(NewOutput).

close_output(o(OldOutput, NewOutput)) :- !,
        set_output(OldOutput),
        close(NewOutput).
close_output(X) :-
        throw(error(domain_error(open_output_handler, X), close_output/1-1)).
