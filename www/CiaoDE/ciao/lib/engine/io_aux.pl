:- module(io_aux, [
        message/2, message_lns/4,
        error/1, warning/1, note/1, message/1, debug/1,
        inform_user/1, display_string/1, display_list/1, display_term/1],
        [assertions]).

:- comment(title,"Message printing primitives").

:- comment(author,"Daniel Cabeza").

:- comment(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- comment(module,"This module provides predicates for printing in a
   unified way informational messages, and also for printing some terms
   in a specific way.").


:- use_module(engine(internals), ['$quiet_flag'/2]).

:- import(write, [write/1, writeq/1]).

:- comment(message(Type, Message), "Output to standard error
   @var{Message}, which is of type @var{Type}. The @tt{quiet}
   @index{prolog flag} (see @ref{Changing system behaviour and various
   flags}) controls which messages are actually output, depending on its
   type. Also, for @tt{error}, @tt{warning} and @tt{note} messages, a
   prefix is output which denotes the severity of the message.
   @var{Message} is an item or a list of items from this list:
@begin{description}

@item{@tt{$$(String)}} @tt{String} is a string, which is output with
   @pred{display_string/1}.

@item{@tt{''(Term)}} @tt{Term} is output quoted.  If the module
   @lib{write} is loaded, the term is output with @pred{writeq/1}, else
   with @pred{displayq/1}.

@item{@tt{~~(Term)}} @tt{Term} is output unquoted.  If the module
   @lib{write} is loaded, the term is output with @pred{write/1}, else
   with @pred{display/1}.

@item{@tt{[](Term)}} @tt{Term} is recursively output as a message, can
   be an item or a list of items from this list.

@item{@tt{Term}} Any other term is output with @pred{display/1}.
@end{description} ").

:- true pred message(Type, Message)
        : (atm(Type), member(Type, [error,warning,note,message,debug])).

:- comment(message_lns(Type, L0, L1, Message), "Output to standard error
   @var{Message}, which is of type @var{Type}, and occurs between lines
   @var{L0} and @var{L1}.  This is the same as @pred{message/2}, but
   printing the lines where the message occurs in a unified way (this is
   useful because automatic tools such as the emacs mode know how to
   parse them).").

:- true pred message_lns(Type, L0, L1, Message)
        : (atm(Type), member(Type, [error,warning,note,message,debug])).

% Auxiliary IO predicates:

:- comment(error/1, "Defined as @includedef{error/1}.").
:- comment(warning/1, "Defined as @includedef{warning/1}.").
:- comment(note/1, "Defined as @includedef{note/1}.").
:- comment(message/1, "Defined as @includedef{message/1}.").
:- comment(debug/1, "Defined as @includedef{debug/1}.").

error(Message)   :- message(error, Message).
warning(Message) :- message(warning, Message).
note(Message)    :- message(note, Message).
message(Message) :- message(message, Message).
debug(Message)   :- message(debug, Message).

message(Type, Message) :-
        '$quiet_flag'(Q, Q),
        allowed_type(Q,Type), !,
        add_head(Type, Message, MessL),
        current_output(S),
        set_output(user_error),
        output_message(MessL), nl,
        set_output(S).
message(_,_).

message_lns(Type, L0, L1, Message) :-
        '$quiet_flag'(Q, Q),
        allowed_type(Q,Type), !,
        add_lines(L0, L1, Message, Messlns),
        add_head(Type, Messlns, MessL),
        current_output(S),
        set_output(user_error),
        output_message(MessL), nl,
        set_output(S).
message_lns(_,_,_,_).

output_message([M|Ms]) :- !,
        output_item(M),
        output_message(Ms).
output_message([]) :- !.
output_message(M) :-
        output_item(M).

output_item(V) :- var(V), !, display(V).
output_item($$(M)) :- !, display_string(M).
output_item(''(M)) :- !, (current_module(write) -> writeq(M); displayq(M)).
output_item(~~(M)) :- !, (current_module(write) -> write(M); display(M)).
output_item([](M)) :- !, output_message(M).
output_item(M) :- display(M).

allowed_type(error, error) :- !.
allowed_type(warning, error) :- !.
allowed_type(warning, warning) :- !.
allowed_type(off, error) :- !.
allowed_type(off, warning) :- !.
allowed_type(off, note) :- !.
allowed_type(off, message) :- !.
allowed_type(debug,_).

add_head(message, Mess, Mess) :- !.
add_head(debug, Mess, Mess) :- !.
add_head(Type, Mess, NewMess) :-
        label(Type, Label),
        NewMess = [Label|Mess].

label(error, 'ERROR: ').
label(warning, 'WARNING: ').
label(note, 'Note: ').

add_lines(L0, L1, Message, ['(lns ',L0,'-',L1,') '|Message]).

:- comment(inform_user(Message), "Similar to @pred{message/1}, but
   @var{Message} is output with @pred{display_list/1}.  This predicate
   is obsolete, and may disappear in future versions.").

inform_user(MessL) :-
        '$quiet_flag'(Q, Q),
        allowed_type(Q,message), !,
        current_output(S),
        set_output(user_error),
        display_list(MessL), nl,
        set_output(S).
inform_user(_).

:- comment(display_list(List), "Outputs @var{List}.  If @var{List} is a
   list, do @pred{display/1} on each of its elements, else do
   @pred{display/1} on @var{List}.").

display_list([M|Ms]) :- !,
        display(M),
        display_list(Ms).
display_list([]) :- !.
display_list(M) :-
        display(M).

:- comment(display_term(Term), "Output @var{Term} in a way that a
   @pred{read/1} will be able to read it back, even if operators
   change.").

display_term(T) :- displayq(T), display(' .\n').

:- comment(display_string(String), "Output @var{String} as the sequence
   of characters it represents.").

:- true pred display_string(String) : string.

display_string([]).
display_string([C|Cs]) :- put_code(C), display_string(Cs).

:- comment(bug, "@pred{message/2} assumes that a module with name 'write'
   is library(write).").
