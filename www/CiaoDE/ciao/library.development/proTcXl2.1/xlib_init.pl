%
% ECLiPSe version of the Prolog Tk interface
%
%	This file takes care of the Xlib macros from X.h.
%	X.h is parsed using the DCG grammar below, and all its macros
%	are defined to be Prolog macros as well using an 'x' prefix,
%	so e.g. MapNotify becomes xMapNotify.
%	The macros are produced into a file rather than to define
%	dynamically, because they have to be defined in every module
%	that includes the xlib module.
%
% Author: Micha Meier
% Date:   August 95
%

%
% sccsid("@(#)xlib_init.pl	1.2          96/02/07").
% sccscr("@(#)  Copyright 1995 ECRC GmbH ").
%

%% :- begin_module(tk).

%% :- global xlib_init/0.

%
% Define all macros from X.h as ECLiPSe macros
%
xlib_init :-
    x_include_path(Path),
    concat_string([Path, "/X11/X.h"], XFile),
    scan(XFile),
    assert_tr_proc,
    findall(define_macro_(Name/0, tr_xlib/2, [], M), tr_xlib(Name, _),
	Macros),
    open('tr_xlib.pl', write, output),
    listing(tr_xlib/2),
    printf("\n\n:- tool(xlib_define_macros/0, xlib_define_macros_body/1).\n", []),
    printf("\nxlib_define_macros_body(M) :-\n", []),
    output_list(Macros),
    close(output).

output_list([]) :-
    printf("    true.\n", []).
output_list([A|B]) :-
    printf("    %w,\n", [A]),
    output_list(B).

x_include_path(Path) :-
    x_include(P),
    atom_string(P, S),
    (substring(S, "-I", 1) ->
	L is string_length(S) - 2,
	substring(S, 3, L, Path)
    ;
	Path = "/usr/local/include"
    ).

assert_tr_proc :-
    recorded_macro(NameString, Value),
    concat_atom([x, NameString], Name),
    assert(tr_xlib(Name, Value)),
    fail; true.
    

%
% A short grammar for a subset of CPP to parse the X.h file
%

:- dynamic recorded_macro/2.

scan(File) :-
    open(File, read, S),
    phrase(c_file, S-0, S-End),
    printf("End = %d\n", End),
    at_eof(S),
    close(S).

% To be able to backtrack over file reading
%% :- local 'C'/3.

'C'(S-Pos, Token, S-Pos1) :-
    seek(S, Pos),
    read_token(S, Token, _Type),
    at(S, Pos1).

c_file -->
    statement, c_file
    ;
    [end_of_file].

statement -->
    [#], macro ; typedef.

macro -->
    [define], lhs(Lhs), rhs(Rhs), {process_macro(Lhs, Rhs)}
    ;
    simple_macro, [_]
    ;
    [endif].

simple_macro -->
    [ifdef] ; [ifndef] ; [define].

%
% This is the key predicate, it gets the macro name and its value
% for each macro defined in the file
%
process_macro(Lhs, Rhs) :-
    % We have to record it for recursive macros
    assert(recorded_macro(Lhs, Rhs)).

%
% Typedefs
%
typedef -->
    [typedef], type, id(ID), [;],
    {record_type(ID)}.

type -->
    spec, proper_type.

spec -->
    [] ; [unsigned].

proper_type -->
    [T], {to_atom(T, TA), recorded_type(TA)}.

id(IDA) -->
    [ID], {to_atom(ID, IDA)}.

record_type(ID) :-
    to_atom(ID, IDA),
    assert(recorded_type(IDA)).

to_atom(A, A) :-
    atom(A).
to_atom(S, A) :-
    string(S),
    atom_string(A, S).

to_string(S, S) :-
    string(S).
to_string(A, S) :-
    atom(A),
    atom_string(A, S).

:- dynamic recorded_type/1.
recorded_type(char).
recorded_type(long).
recorded_type(int).

%
% Defines
%
lhs(IDS) -->
    [ID], {to_string(ID, IDS)}.

rhs(Value) -->
    integer(Value)
    ;
    integer(V1), binop(Op), integer(V2),
	{OpFunc =.. [Op, V1, V2, Value],
	call(OpFunc)}
    ;
    ["("], rhs(Value), [")"]
    ;
    cast, rhs(Value)
    ;
    defined_macro(Value).

integer(Value) -->
    simple_integer(Value), int_size.

int_size -->
    ["L"] ; [].

simple_integer(Value) -->
    [0], [BasedVal], {hex_integer(BasedVal, Value)}
    ;
    [Value], {integer(Value)}.

% Process 0x... We have to convert it to the Prolog-based notation
hex_integer(BasedVal, Value) :-
    to_string(BasedVal, BasedValS),
    c_bases(Char, Base),
    substring(BasedValS, Char, 1),
    string_length(BasedValS, L),
    L1 is L - 1,
    substring(BasedValS, 2, L1, XS),
    concat_string([Base, XS], PrologNumber),
    term_string(Value, PrologNumber).

c_bases("x", "16'").
c_bases("b", "2'").

cast -->
    ["("], type, [")"].

defined_macro(Value) -->
    [Name], {recorded_macro(Name, Value)}.

binop(<<) -->
    [<<].

:- xlib_init.
