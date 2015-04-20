:- module(io_basic, [
        get_code/2, get_code/1, get1_code/2, get1_code/1,
        peek_code/2, peek_code/1, skip_code/2, skip_code/1,
        skip_line/1, skip_line/0,
        put_code/2, put_code/1, nl/1, nl/0, tab/2, tab/1,
        code_class/2, getct/2, getct1/2, display/2, display/1,
        displayq/2, displayq/1],
        [assertions, isomodes]).

:- impl_defined([
        get_code/2, get_code/1, get1_code/2, get1_code/1,
        peek_code/2, peek_code/1, skip_code/2, skip_code/1,
        skip_line/0, skip_line/1,
        put_code/2, put_code/1, nl/1, nl/0, tab/2, tab/1,
        code_class/2, getct/2, getct1/2, display/2, display/1,
        displayq/2, displayq/1]).

:- comment(title, "Basic input/output").

:- comment(author, "Daniel Cabeza").
:- comment(author, "Mats Carlsson").

:- comment(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- comment(module, "This module provides predicates for character
   input/output and for canonical term output.  From the
   @concept{ISO-Prolog} predicates for character input/output, only the
   @tt{_code} versions are provided, the rest are given by
   @lib{library(iso_byte_char)}, using these.  Most predicates are
   provided in two versions: one that specifies the input or output
   stream as the first argument and a second which omits this argument
   and uses the current input or output stream.").

:- comment(get_code(Stream, Code), "Reads from @var{Stream} the next
   character and unifies @var{Code} with its character code.  At end of
   stream, unifies @var{Code} with the integer -1.").

:- true pred get_code(+stream, ?int) + (iso, native).

:- comment(get_code(Code), "Behaves like @tt{current_input(S),
   get_code(S,Code)}.").

:- true pred get_code(?int) + (iso, native).

:- comment(get1_code(Stream, Code), "Reads from @var{Stream} the next
   non-layout character (see @pred{code_class/2}) and unifies @var{Code}
   with its character code.  At end of stream, unifies @var{Code} with
   the integer -1.").

:- true pred get1_code(+stream, ?int) + native.

:- comment(get1_code(Code), "Behaves like @tt{current_input(S),
   get1_code(S,Code)}.").

:- true pred get1_code(?int) + native.

:- comment(peek_code(Stream, Code), "Unifies @var{Code} with the
   character code of the next character of @var{Stream}, leaving the
   stream position unaltered.  At end of stream, unifies @var{Code} with
   the integer -1.").

:- true pred peek_code(+stream, ?int) + iso.

:- comment(peek_code(Code), "Behaves like @tt{current_input(S),
   peek_code(S,Code)}.").

:- true pred peek_code(?int) + iso.

:- comment(skip_code(Stream, Code), "Skips just past the next character
   code @var{Code} from @var{Stream}.").

:- true pred skip_code(+stream, +int).

:- comment(skip_code(Code), "Behaves like @tt{current_input(S),
   skip_code(S,Code)}.").

:- true pred skip_code(+int).

:- comment(skip_line(Stream), "Skips from @var{Stream} the remaining
   input characters on the current line.  If the end of the stream is
   reached, the stream will stay at its end.  Portable among different
   operating systems.").

:- true pred skip_line(+stream).

:- comment(skip_line, "Behaves like @tt{current_input(S), skip_line(S)}.").

% :- true pred skip_line.

:- comment(put_code(Stream, Code), "Outputs to @var{Stream} the
   character corresponding to character code @var{Code}.").

:- true pred put_code(+stream, +int) + (iso, native).

:- comment(put_code(Code), "Behaves like @tt{current_output(S),
   put_code(S,Code)}.").

:- true pred put_code(+int) + (iso, native).

:- comment(nl(Stream), "Outputs a newline character to @var{Stream}.
   Equivalent to @tt{put_code(Stream, 0'\\n)}.").

:- true pred nl(+stream) + (iso, native).

:- comment(nl, "Behaves like @tt{current_output(S), nl(S)}.").

:- true pred nl + (iso, native).

:- comment(tab(Stream,Num), "Outputs @var{Num} spaces to @var{Stream}.").

:- true pred tab(+stream,+int) + native.

:- comment(tab(Num), "Behaves like @tt{current_output(S), tab(S,Num)}.").

:- true pred tab(+int) + native.

:- comment(code_class(Code,Class), "Unifies @var{Class} with an integer
   corresponding to the lexical class of the character whose code is
   @var{Code}, with the following correspondence:
   @begin{verbatim}
    0 - layout (includes space, newline, tab)
    1 - small letter
    2 - capital letter (including '_')
    3 - digit
    4 - graphic (includes #$&*+-./:<=>?@@^\\`~ )
    5 - punctuation (includes !;""'%(),[]@{|@} )
   @end{verbatim}
   Note that in @concept{ISO-Prolog} the back quote @tt{`} is a punctuation
   character, whereas in Ciao it is a graphic character.  Thus, if
   compatibility with @concept{ISO-Prolog} is desired, the programmer should
   not use this character in unquoted names.").

:- true pred code_class(+int,?int).

:- comment(getct(Code, Type), "Reads from the current input stream the
   next character, unifying @var{Code} with its character code, and
   @var{Type} with its lexical class.  At end of stream, unifies both
   @var{Code} and @var{Type} with the integer -1.  Equivalent to
   @begin{verbatim}
   get(Code), (Code = -1 -> Type = -1 ; code_class(Code,Type))
   @end{verbatim}").

:- true pred getct(?int, ?int).

:- comment(getct1(Code, Type), "Reads from the current input stream the
   next non-layout character, unifying @var{Code} with its character
   code, and @var{Type} with its lexical class (which will be nonzero).
   At end of stream, unifies both @var{Code} and @var{Type} with the
   integer -1.  Equivalent to
   @begin{verbatim}
   get1(Code), (Code = -1 -> Type = -1 ; code_class(Code,Type))
   @end{verbatim}").

:- true pred getct1(?int, ?int).

:- comment(display(Stream, Term), "Displays @var{Term} onto
   @var{Stream}.  Lists are output using list notation, the other
   compound terms are output in functional notation.  Similar to
   @tt{write_term(Stream, Term, [ignore_ops(ops)])}, except that 
   curly bracketed notation is not used with @tt{@{@}/1}, and
   the @tt{write_strings} flag is not honored.").

:- true pred display(+stream,@term) + native.

:- comment(display(Term), "Behaves like @tt{current_output(S),
   display(S,Term)}.").

:- true pred display(@term) + native.

:- comment(displayq(Stream, Term), "Similar to @tt{display(Stream, Term)},
   but atoms and functors that can't be read back by @pred{read_term/3}
   are quoted.  Thus, similar to
   @tt{write_term(Stream, Term, [quoted(true), ignore_ops(ops)])}, with the
   same exceptions as @pred{display/2}.").

:- true pred displayq(+stream,@term).

:- comment(displayq(Term), "Behaves like @tt{current_output(S),
   displayq(S,Term)}.").

:- true pred displayq(@term).
