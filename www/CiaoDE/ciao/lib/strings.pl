:- module(strings,
        [get_line/2, get_line/1,
         write_string/2, write_string/1,
         whitespace/2, whitespace0/2,
         string/3
        ],
        [dcg,assertions,isomodes]).
% Utilities taken out from html.pl

:- comment(title, "String processing").
:- comment(author, "Daniel Cabeza").

:- comment(module, "This module provides predicates for doing
   input/output with strings (character code lists) and for including in
   grammars defining strings.").

:- comment(get_line(Stream, Line), "Reads from @var{Stream} a line of
   text and unifies @var{Line} with it.  The end of the line can have
   UNIX [10] or MS-DOS [13 10] termination, which is not included in
   @var{Line}.  At EOF, the term end_of_file is returned.").

:- true pred get_line(+stream, ?line).

get_line(Stream, Line) :-
        current_input(OldIn),
        set_input(Stream),
        get_line(Line),
        set_input(OldIn).

:- comment(get_line(Line), "Behaves like @tt{current_input(S),
   get_line(S,Line)}.").

:- true pred get_line(?line).

get_line(Line) :-
        get_code(C),
        ( C = -1 -> Line = end_of_file
        ; get_line_after(C, Cs),
          Line = Cs
        ).

get_line_after(-1,[]) :- !, % EOF
        current_input(S), clearerr(S).
get_line_after(10,[]) :- !. % Newline
get_line_after(13, R) :- !, % Return, delete if at end of line
        get_code(C),
        get_line_after(C, Cs),
        ( Cs = [] ->
              R = []
        ; R = [13|Cs]
        ).
get_line_after(C, [C|Cs]) :-
        get_code(C1),
        get_line_after(C1, Cs).

:- comment(doinclude,line/1).

:- prop line/1.

line(L) :- string(L).
line(end_of_file).

:- comment(write_string(Stream, String), "Writes @var{String} onto
   @var{Stream}.").

:- true pred write_string(+stream, +string).

write_string(Stream, S) :-
        current_output(OldOut),
        set_output(Stream),
        write_string(S),
        set_output(OldOut).

:- comment(write_string(String), "Behaves like @tt{current_input(S),
   write_string(S, String)}.").

:- true pred write_string(+string).

write_string(V) :- var(V), !,
        throw(error(instantiation_error,write_string/1-1)).
write_string([]).
write_string([C|Cs]) :- put_code(C), write_string(Cs).

:- comment(whitespace(String, Rest), "In a grammar rule, as
   @tt{whitespace/0}, represents whitespace (a positive
   number of space (32), tab (9), newline (10) or return (13)
   characters).  Thus, @var{Rest} is a proper suffix of @var{String}
   with one or more whitespace characters removed.  An example of use
   would be:
@begin{verbatim}
   attrs([]) --> """"
   attrs([N|Ns]) -->
       whitespace,
       attr(N),
       attrs(Ns).
@end{verbatim}
").

:- true pred whitespace(+string,?string).

whitespace --> whitespace_char, whitespace0.

:- comment(whitespace0(String, Rest), "In a grammar rule, as
   @tt{whitespace0/0}, represents possible whitespace (any number of
   space (32), tab (9), newline (10) or return (13) characters). Thus,
   @var{Rest} is @var{String} or a proper suffix of @var{String} with
   one or more whitespace characters removed.  An example of use would
   be:

@begin{verbatim}
   assignment(N,V) -->
       variable_name(N), whitespace0, ""="", whitespace0, value(V).
@end{verbatim}
").

:- true pred whitespace0(+string,?string).

whitespace0 --> whitespace_char, whitespace0.
whitespace0 --> [].

whitespace_char --> [10]. % newline
whitespace_char --> [13]. % return
whitespace_char --> [32]. % space
whitespace_char --> [9].  % tab

:- comment(string(String, Head, Tail), "In a @concept{grammar rule}, as
   @tt{string/1}, represents literally @var{String}.  An example of use
   would be:

@begin{verbatim}
double(A) -->
        string(A),
        string(A).
@end{verbatim}
").

:- true pred string(?string,?string,?string).

string([]) --> "".
string([C|Cs]) -->
        [C],
        string(Cs).
