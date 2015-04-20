/*  Copyright (C) 1996-2002 UPM-CLIP */

% Tokeniser for CIAO Prolog.

/*
Diffs. with ISO:

- ` is a graphic char, there are not back_quoted_strings
- \ followed by any layout char (not only new_line) in a string is a
  continuation_escape_sequence 
- \^ starts a control_escape_char in a string
- \c skips layout in a string
- \e = ESC, \d = DEL, \s = SPACE
- 13'23 is 23 in base 13 (same for other bases)
- 0'' is accepted as 0''' (if not followed by ')

*/

:- module(tokenize, [read_tokens/2],[assertions]).

:- use_module(engine(internals)).
:- use_module(library(read), [second_prompt/2]).
:- use_module(library(dict)).

:- set_prolog_flag(multi_arity_warnings, off).

:- multifile define_flag/3.

define_flag(character_escapes, [iso, sicstus, off], iso).

% Character classes
% -1 - end of file                    
%  0 - layout                         
%  1 - small letter                   
%  2 - capital letter (including '_') 
%  3 - digit                          
%  4 - graphic                        
%  5 - punctuation

% Existing tokens:
%    atom(atom)
%    badatom(string)
%    number(number)
%    string(string)
%    var(term,string)
%    '/* ...'
%    ',' | '(' | ' (' | ')' | '[' | ']' | '|' | '{' | '}'
%    '.' % end of term 

read_tokens(TokenList, Dictionary) :-
        getct1(Ch, Type),
        second_prompt(SP, SP),
	'$prompt'(Old, SP),
        read_tokens(Type, Ch, Dictionary, TokenList),
	'$prompt'(_, Old).

% The only difference between read_tokens_after_layout(Typ, Ch, D, Tokens)
% and read_tokens/4 is what they do when Ch is "(".  The former finds the
% token to be ' (', while the later finds the token to be '('.  This is
% how the parser can tell whether <atom> <paren> must be an operator
% application or an ordinary function symbol application.

read_tokens_after_layout(5, 0'(, Dict, Tokens) :- !,
        getct1(NextCh, NextTyp),
        ( NextCh = 0') ->
            Tokens = [atom('()')|Tokens_],
            getct(NextCh2, NextTyp2),
            read_tokens(NextTyp2, NextCh2, Dict, Tokens_)
        ;
            Tokens = [' ('|Tokens_],
            read_tokens(NextTyp, NextCh, Dict, Tokens_)
        ).
read_tokens_after_layout(Typ, Ch, Dict, Tokens) :-
	read_tokens(Typ, Ch, Dict, Tokens).

read_tokens(-1, _, _, []).                      % end of file
read_tokens(0, _, Dict, Tokens) :-              % layout
        getct1(NextCh, NextTyp),
        read_tokens_after_layout(NextTyp, NextCh, Dict, Tokens).
read_tokens(1, Ch0, Dict, [Atom|Tokens]) :-     % small letter: atom
        getct(Ch, Typ),
        read_name(Typ, Ch, S0, NextCh, NextTyp),
        atom_token([Ch0|S0], Atom),
        read_tokens(NextTyp, NextCh, Dict, Tokens).
read_tokens(2, Ch0, Dict, [var(Var,S)|Tokens]) :- % capital letter: variable
        S = [Ch0|S0],
        getct(Ch, Typ),
        read_name(Typ, Ch, S0, NextCh, NextTyp),
        ( S = "_" ->                            % anonymous variable
              true
        ; dic_lookup(Dict, S, Node),            % lookup/enter in dictionary
          check_singleton(Node, Var)
        ),
        read_tokens(NextTyp, NextCh, Dict, Tokens).
read_tokens(3, Ch, Dict, [number(N)|Tokens]) :-  % number
        read_number(Ch, N, Dict, Tokens).	 % reads continuation too.
read_tokens(4, 0'/, Dict, Tokens) :- !,          % comment if an '*' follows
        getct(NextCh, NextTyp),
        read_possible_comment(NextTyp, NextCh, Dict, Tokens).
read_tokens(4, 0'., Dict, Tokens) :- !,          % end token or graphic atom
        getct(NextCh, NextTyp),         
        read_fullstop(NextTyp, NextCh, Dict, Tokens).
read_tokens(4, Ch, Dict, [Atom|Tokens]) :-       % graphic atom
        getct(AnotherCh, Typ),
        read_symbol(Typ, AnotherCh, Chars, NextCh, NextTyp),
        atom_token([Ch|Chars], Atom),
        read_tokens(NextTyp, NextCh, Dict, Tokens).
read_tokens(5, Ch, Dict, Tokens) :-
        read_tokens_solo(Ch, Dict, Tokens).

read_tokens_solo(0'!, Dict, [atom(!)|Tokens]) :-
        getct(NextCh, NextTyp),
        read_tokens(NextTyp, NextCh, Dict, Tokens).
read_tokens_solo(0';, Dict, [atom(;)|Tokens]) :-
        getct(NextCh, NextTyp),
        read_tokens(NextTyp, NextCh, Dict, Tokens).
read_tokens_solo(0'%, Dict, Tokens) :-          % comment
        skip_line,
        getct1(NextCh, NextTyp),
        read_tokens_after_layout(NextTyp, NextCh, Dict, Tokens).
read_tokens_solo(0'(, Dict, Tokens) :-
        getct1(NextCh, NextTyp),
        ( NextCh = 0') ->
            Tokens = [atom('()')|Tokens_],
            getct(NextCh2, NextTyp2),
            read_tokens(NextTyp2, NextCh2, Dict, Tokens_)
        ;
            Tokens = ['('|Tokens_],
            read_tokens(NextTyp, NextCh, Dict, Tokens_)
        ).
read_tokens_solo(0'), Dict, [')'|Tokens]) :-
        getct1(NextCh, NextTyp),
        read_tokens(NextTyp, NextCh, Dict, Tokens).
read_tokens_solo(0',, Dict, [','|Tokens]) :-
        getct1(NextCh, NextTyp),
        read_tokens(NextTyp, NextCh, Dict, Tokens).
read_tokens_solo(0'[, Dict, ['['|Tokens]) :-
        getct1(NextCh, NextTyp),
        read_tokens(NextTyp, NextCh, Dict, Tokens).
read_tokens_solo(0'], Dict, [']'|Tokens]) :-
        getct1(NextCh, NextTyp),
        read_tokens(NextTyp, NextCh, Dict, Tokens).
read_tokens_solo(0'{, Dict, ['{'|Tokens]) :-
        getct1(NextCh, NextTyp),
        read_tokens(NextTyp, NextCh, Dict, Tokens).
read_tokens_solo(0'|, Dict, ['|'|Tokens]) :-
        getct1(NextCh, NextTyp),
        read_tokens(NextTyp, NextCh, Dict, Tokens).
read_tokens_solo(0'}, Dict, ['}'|Tokens]) :-
        getct1(NextCh, NextTyp),
        read_tokens(NextTyp, NextCh, Dict, Tokens).
read_tokens_solo(0'", Dict, [string(S)|Tokens]) :-  % "string"
        getct(Ch, Typ),
        read_string(Typ, Ch, S, 0'", NextTyp, NextCh),
        read_tokens(NextTyp, NextCh, Dict, Tokens).
read_tokens_solo(0'', Dict, [Atom|Tokens]) :- % 'atom'
        getct(Ch, Typ),
        read_string(Typ, Ch, S, 0'', NextTyp, NextCh),
        atom_token(S, Atom),
        read_tokens(NextTyp, NextCh, Dict, Tokens).

% read_name(Typ, Char, String, LastCh, LastTyp)
% reads a sequence of letters, digits, and underscores, and returns
% them as String.  The first character which cannot join this sequence
% is returned as LastCh.

read_name(1, Char, String, LastCh, LastTyp) :- !,
        String = [Char|Chars],
        getct(NextCh, NextTyp),
        read_name(NextTyp, NextCh, Chars, LastCh, LastTyp).
read_name(2, Char, String, LastCh, LastTyp) :- !,
        String = [Char|Chars],
        getct(NextCh, NextTyp),
        read_name(NextTyp, NextCh, Chars, LastCh, LastTyp).
read_name(3, Char, String, LastCh, LastTyp) :- !,
        String = [Char|Chars],
        getct(NextCh, NextTyp),
        read_name(NextTyp, NextCh, Chars, LastCh, LastTyp).
read_name(LastTyp, LastCh, [], LastCh, LastTyp).

% read_symbol(Typ, Ch, String, NextCh, NextTyp)
% reads the other kind of atom which needs no quoting: one which is
% a string of "symbol" characters.

read_symbol(4, Char, String, LastCh, LastTyp) :- !,
        String = [Char|Chars],
        getct(NextCh, NextTyp),
        read_symbol(NextTyp, NextCh, Chars, LastCh, LastTyp).
read_symbol(LastTyp, LastCh, [], LastCh, LastTyp).


% read_possible_comment(Typ, Ch, Dict, Tokens)
% checks to see whether / + Ch is a / + * comment or a symbol.  If the
% former, it skips the comment.  If the latter it just calls read_symbol.

read_possible_comment(4, 0'*, Dict, Tokens) :- !,
        skip_comment_text(Dict, Tokens).
read_possible_comment(Typ, Ch, Dict, [Atom|Tokens]) :-
        read_symbol(Typ, Ch, Chars, NextCh, NextTyp), % might read 0 chars
        atom_token([0'/|Chars], Atom),
        read_tokens(NextTyp, NextCh, Dict, Tokens).

skip_comment_text(Dict, Tokens) :-
        ( skip_code_prot(0'*) ->
            getct(Ch, Typ),
            read_end_comment(Typ, Ch, Dict, Tokens)
        ; % end of file
          Tokens = ['/* ...']
        ).
        
read_end_comment(4, 0'/, Dict, Tokens) :- !,
        getct1(NextCh, NextTyp),
        read_tokens_after_layout(NextTyp, NextCh, Dict, Tokens).
read_end_comment(4, 0'*, Dict, Tokens) :- !,
        getct(Ch, Typ),
        read_end_comment(Typ, Ch, Dict, Tokens).
read_end_comment(_, _, Dict, Tokens) :-
        skip_comment_text(Dict, Tokens).

% read_fullstop(Typ, Char, Dict, Tokens)
% looks at the next character after a full stop.  If the next character is
% an end of file, a layout character or %, this is a clause terminator, else
% this is just an ordinary symbol and we call read_symbol to process it.

read_fullstop(-1, _, _, [.]) :- !.              % end of file
read_fullstop(0, _, _, [.]) :- !.               % END OF CLAUSE
read_fullstop(5, 0'%, _, [.]) :- !,             % END OF CLAUSE,
        skip_line.                              % skip newline
read_fullstop(Typ, Ch, Dict, [Atom|Tokens]) :-
        read_symbol(Typ, Ch, S, NextCh, NextTyp),   % symbol
        atom_token([0'.|S], Atom),
        read_tokens(NextTyp, NextCh, Dict, Tokens).

% read_string(Typ, Ch, Chars, Quote, NextCh, NextTyp)
% reads the body of a string delimited by Quote characters.
% The result is a list Chars of ASCII codes.

read_string(-1, _, [], _, -1, -1) :- !.         % end of file
read_string(5, Quote, Chars, Quote, NextTyp, NextCh) :- !,
        getct(Ch, Typ),                     % closing or doubled quote
        read_end_string(Typ, Ch, Quote, Chars, NextTyp, NextCh).
read_string(4, 92, Chars, Quote, NextTyp, NextCh) :-
        current_prolog_flag(character_escapes, CEF),     % escaped character
        CEF \== off,
        !,
        getct(Ch, Typ),                
        escape_sequence(Typ, Ch, CEF, Chars, RestChars, OtTyp, OtCh),
        read_string(OtTyp, OtCh, RestChars, Quote, NextTyp, NextCh).
read_string(_, Char, [Char|Chars], Quote, NextTyp, NextCh) :-
        getct(Ch, Typ),                     % ordinary character
        read_string(Typ, Ch, Chars, Quote, NextTyp, NextCh).

read_end_string(5, Quote, Quote, [Quote|Chars], NextTyp, NextCh) :- !,
        getct(Ch, Typ),                     % doubled quote
        read_string(Typ, Ch, Chars, Quote, NextTyp, NextCh).
read_end_string(NextTyp, NextCh, _, [], NextTyp, NextCh).

escape_sequence(0, _, _, Chars, Chars, Typ, Ch) :- !,
        getct(Ch, Typ).                     % continuation escape sequence
escape_sequence(4, 0'^, _, [Char|Chars], Chars, NextTyp, NextCh) :-
        getct(Ch, Typ),
        ( control_character(Typ, Ch, CC) ->
              Char = CC,
              getct(NextCh, NextTyp)
        ; Char = 0'^,
          NextTyp = Typ,
          NextCh = Ch
        ).
escape_sequence(3, D, CEF, [Char|Chars], Chars, NextTyp, NextCh) :-
        D =< 0'7, !,                         % octal escape sequence
        getct(Ch, Typ),
        ( CEF = sicstus ->
            read_two_octal(Typ, Ch, Ds, NextTyp, NextCh)
        ; read_octal_iso(Typ, Ch, Ds),
          getct(NextCh, NextTyp)
        ),
        number_codes(Char, 8, [D|Ds]).
escape_sequence(1, 0'x, CEF, [Char|Chars], Chars, NextTyp, NextCh) :- !,
        getct(Ch, Typ),                     % hexadecimal escape sequence
        ( CEF = sicstus ->
            read_two_hexa(Typ, Ch, Ds, NextTyp, NextCh)
        ; read_hexa_iso(Typ, Ch, Ds),
          getct(NextCh, NextTyp)
        ),
        number_codes(Char, 16, [0'0|Ds]).
escape_sequence(1, 0'c, _, Chars, Chars, NextTyp, NextCh) :- !,
        getct1(NextCh, NextTyp).              % skip layout
escape_sequence(1, L, _, [Char|Chars], Chars, NextTyp, NextCh) :-
        symbolic_control_char(L, Char), !,   % control escape sequence
        getct(NextCh, NextTyp).
escape_sequence(_, Ch, _, [Ch|Chars], Chars, NextTyp, NextCh) :-
        getct(NextCh, NextTyp).

control_character(4, 0'?, 127).
control_character(4, 0'@, 0).
control_character(2, C, CC) :-    % This includes also "_"
        CC is C mod 32.
control_character(1, C, CC) :-
        CC is C mod 32.
control_character(4, C, CC) :-
        C >= 0'[, C =< 0'^,
        CC is C mod 32.

symbolic_control_char(0'a, 7).
symbolic_control_char(0'b, 8).
symbolic_control_char(0't, 9).
symbolic_control_char(0'n,10).
symbolic_control_char(0'v,11).
symbolic_control_char(0'f,12).
symbolic_control_char(0'r,13).
symbolic_control_char(0'e,27).
symbolic_control_char(0's,32).
symbolic_control_char(0'd,127).

read_two_octal(3, D, [D|Ds], EndTyp, EndCh) :-
        D =< 0'7, !,
        getct(Ch, Typ),
        read_one_octal(Typ, Ch, Ds, EndTyp, EndCh).
read_two_octal(EndTyp, EndCh, [], EndTyp, EndCh).

read_one_octal(3, D, [D], EndTyp, EndCh) :-
        D =< 0'7, !,
        getct(EndCh, EndTyp).
read_one_octal(EndTyp, EndCh, [], EndTyp, EndCh).

read_octal_iso(4, 92, []).  % ends in \
read_octal_iso(3, D, [D|Ds]) :-
        D =< 0'7, !,
        getct(Ch, Typ),
        read_octal_iso(Typ, Ch, Ds).
read_octal_iso(_, _, Ds) :- % ignore other characters
        getct(Ch, Typ),
        read_octal_iso(Typ, Ch, Ds).

read_two_hexa(TD, D, [D|Ds], EndTyp, EndCh) :-
        hexa_digit(TD, D), !,
        getct(Ch, Typ),
        read_one_hexa(Typ, Ch, Ds, EndTyp, EndCh).
read_two_hexa(EndTyp, EndCh, [], EndTyp, EndCh).

read_one_hexa(TD, D, [D], EndTyp, EndCh) :-
        hexa_digit(TD, D), !,
        getct(EndCh, EndTyp).
read_one_hexa(EndTyp, EndCh, [], EndTyp, EndCh).

read_hexa_iso(4, 92, []).  % ends in \
read_hexa_iso(TD, D, [D|Ds]) :-
        hexa_digit(TD, D), !,
        getct(Ch, Typ),
        read_hexa_iso(Typ, Ch, Ds).
read_hexa_iso(_, _, Ds) :- % ignore other characters
        getct(Ch, Typ),
        read_hexa_iso(Typ, Ch, Ds).

hexa_digit(3, _).
hexa_digit(2, D) :- D =< 0'F.
hexa_digit(1, D) :- D =< 0'f.

% read_number reads an unsigned integer or float. This is the most difficult
% part of the tokenizer. There are seven forms of number:
%   <digits>                                    integer in decimal
%   <base> ' <base-digits>                      integer in other base (2..36)
%   <digits> . <digits>                         float
%   <digits> . <digits> (e|E) (-|+| ) <digits>  float with exponent
%   0.Nan                                       Not-a-number value
%   0.Inf                                       Infinite
%   0 ' <character>                             ascii code of the character
%   0 b <bin-digits>                            binary integer
%   0 o <oct-digits>                            octal integer
%   0 x <hex-digits>                            hexadecimal integer
%

read_number(0'0, N, Dict, Tokens) :- !,
        getct(Ch, Typ),
        read_after_0(Typ, Ch, N, Dict, Tokens).
read_number(D, N, Dict, Tokens) :-
        getct(Ch, Typ),
        read_digits(Typ, Ch, S, [D|S], N, Dict, Tokens).

read_after_0(3, D, N, Dict, Tokens) :-
        getct(Ch, Typ),
        read_digits(Typ, Ch, S, [D|S], N, Dict, Tokens).
read_after_0(4, 0'., N, Dict, Tokens) :- !,
        getct(Ch, Typ),
        read_after_period(Typ, Ch, S, [0'0|S], N, Dict, Tokens).
read_after_0(1, 0'b, N, Dict, Tokens) :- !,
        read_based_int(2, S, EndTyp, EndCh),
        based_int_or_atom(S, 2, 0'b, N, EndTyp, EndCh, Dict, Tokens).
read_after_0(1, 0'o, N, Dict, Tokens) :- !,
        read_based_int(8, S, EndTyp, EndCh),
        based_int_or_atom(S, 8, 0'o, N, EndTyp, EndCh, Dict, Tokens).
read_after_0(1, 0'x, N, Dict, Tokens) :- !,
        read_based_int(16, S, EndTyp, EndCh),
        based_int_or_atom(S, 16, 0'x, N, EndTyp, EndCh, Dict, Tokens).
read_after_0(5, 0'', N, Dict, Tokens) :- !,
        getct(Ch, Typ),
        read_quoted_character(Typ, Ch, N, Dict, Tokens).
read_after_0(Typ, Ch, 0, Dict, Tokens) :-
        read_tokens(Typ, Ch, Dict, Tokens).

read_digits(3, D, [D|S], S0, N, Dict, Tokens) :- !,
        getct(Ch, Typ),
        read_digits(Typ, Ch, S, S0, N, Dict, Tokens).
read_digits(4, 0'., S, S0, N, Dict, Tokens) :- !,
        getct(Ch, Typ),
        read_after_period(Typ, Ch, S, S0, N, Dict, Tokens).
read_digits(5, 0'', [], S0, N, Dict, Tokens) :-
        number_codes(Base, S0),
        Base >= 2,
	Base =< 36, !,
        read_based_int(Base, S1, EndTyp, EndCh),
        based_int_or_quoted(S1, Base, N, EndTyp, EndCh, Dict, Tokens).
read_digits(Typ, Ch, [], S0, N, Dict, Tokens) :-
        number_codes(N, S0),
        read_tokens(Typ, Ch, Dict, Tokens).

read_after_period(3, D, [0'.,D|S], S0, N, Dict, Tokens) :- !,
        getct(Ch, Typ),
        read_after_float(Typ, Ch, S, S0, N, Dict, Tokens).
read_after_period(2, 0'N, [], "0", Nan, Dict, Tokens) :- !,
        getct(Ch, Typ),
        read_after_dot_N(Ch, Typ, Nan, Dict, Tokens).
% next lines added by Edison Mera to handle infinite correctly
read_after_period(2, 0'I, [], "0", Inf, Dict, Tokens) :- !,
	getct(Ch, Typ),
        read_after_dot_I(Ch, Typ, Inf, Dict, Tokens).
read_after_period(Typ, Ch, [], S0, N, Dict, Tokens) :-
        number_codes(N, S0),
        read_fullstop(Typ, Ch, Dict, Tokens).

read_after_float(3, D, [D|S], S0, N, Dict, Tokens) :- !,
        getct(Ch, Typ),
        read_after_float(Typ, Ch, S, S0, N, Dict, Tokens).
read_after_float(1, 0'e, S, S0, N, Dict, Tokens) :- !,
        getct(Ch, Typ),
        read_after_float_e(Typ, Ch, 0'e, S, S0, N, Dict, Tokens).
read_after_float(2, 0'E, S, S0, N, Dict, Tokens) :- !,
        getct(Ch, Typ),
        read_after_float_e(Typ, Ch, 0'E, S, S0, N, Dict, Tokens).
read_after_float(Typ, Ch, [], S0, N, Dict, Tokens) :-
        number_codes(N, S0),
        read_tokens(Typ, Ch, Dict, Tokens).

read_after_float_e(3, D, E, [E,D|S], S0, N, Dict, Tokens) :- !,
        getct(Ch, Typ),
        read_after_exp(Typ, Ch, S, S0, N, Dict, Tokens).
read_after_float_e(4, 0'+, E, S, S0, N, Dict, Tokens) :- !,
        getct(Ch, Typ),
        read_after_float_e_sign(Typ, Ch, E, 0'+, S, S0, N, Dict, Tokens).
read_after_float_e(4, 0'-, E, S, S0, N, Dict, Tokens) :- !,
        getct(Ch, Typ),
        read_after_float_e_sign(Typ, Ch, E, 0'-, S, S0, N, Dict, Tokens).
read_after_float_e(Typ, Ch, E, [], S0, N, Dict, Tokens) :-
        number_codes(N, S0),
        token_start_e(E, Typ, Ch, Dict, Tokens).

read_after_float_e_sign(3, D, E, Sign, [E,Sign,D|S], S0, N, Dict, Tokens):-!,
        getct(Ch, Typ),
        read_after_exp(Typ, Ch, S, S0, N, Dict, Tokens). 
read_after_float_e_sign(Typ, Ch, E, Sign, [], S0, N, Dict, Tokens) :-
        number_codes(N, S0),
        token_start_e_sign(E, Sign, Typ, Ch, Dict, Tokens).

read_after_exp(3, D, [D|S], S0, N, Dict, Tokens) :- !,
        getct(Ch, Typ),
        read_after_exp(Typ, Ch, S, S0, N, Dict, Tokens).
read_after_exp(Typ, Ch, [], S0, N, Dict, Tokens) :-
        number_codes(N, S0),
        read_tokens(Typ, Ch, Dict, Tokens).

token_start_e(0'e, Typ, Ch, Dict, [Atom|Tokens]) :-
        read_name(Typ, Ch, S0, NextCh, NextTyp),
        atom_token([0'e|S0], Atom),
        read_tokens(NextTyp, NextCh, Dict, Tokens).
token_start_e(0'E, Typ, Ch, Dict, [var(Var,S)|Tokens]) :-
        S = [0'E|S0],
        read_name(Typ, Ch, S0, NextCh, NextTyp),
        dic_lookup(Dict, S, Node),
        check_singleton(Node, Var),
        read_tokens(NextTyp, NextCh, Dict, Tokens).

token_start_e_sign(0'e, Sign, Typ, Ch, Dict, [atom(e)|Tokens]) :-
        token_start_sign(Sign, Typ, Ch, Dict, Tokens).
token_start_e_sign(0'E, Sign, Typ, Ch, Dict, [var(Var,[0'E])|Tokens]) :-
        dic_lookup(Dict, [0'E], Node),
        check_singleton(Node, Var),
        token_start_sign(Sign, Typ, Ch, Dict, Tokens).

token_start_sign(Sign, Typ, Ch, Dict, [Atom|Tokens]) :- 
        read_symbol(Typ, Ch, Chars, NextCh, NextTyp),
        atom_token([Sign|Chars], Atom),
        read_tokens(NextTyp, NextCh, Dict, Tokens).

read_after_dot_N(0'a, 1, Nan, Dict, Tokens) :- !,
        getct(Ch, Typ),
        read_after_dot_Na(Ch, Typ, Nan, Dict, Tokens).
read_after_dot_N(Ch, Typ, 0, Dict, [atom(.),var(Var,S)|Tokens]) :-
        S = [0'N|S0],
        read_name(Typ, Ch, S0, NextCh, NextTyp),
        dic_lookup(Dict, S, Node),
        check_singleton(Node, Var),
        read_tokens(NextTyp, NextCh, Dict, Tokens).

read_after_dot_Na(0'n, 1, Nan, Dict, Tokens) :- !,
	Nan is 0/0,
        getct(Ch, Typ),
        read_tokens(Typ, Ch, Dict, Tokens).
read_after_dot_Na(Ch, Typ, 0, Dict, [atom(.),var(Var,S)|Tokens]) :-
        S = [0'N,0'a|S0],
        read_name(Typ, Ch, S0, NextCh, NextTyp),
        dic_lookup(Dict, S, Node),
        check_singleton(Node, Var),
        read_tokens(NextTyp, NextCh, Dict, Tokens).

% next lines added by Edison Mera to handle infinite correctly
read_after_dot_I(0'n, 1, Inf, Dict, Tokens) :- !,
        getct(Ch, Typ),
        read_after_dot_In(Ch, Typ, Inf, Dict, Tokens).
read_after_dot_I(Ch, Typ, 0, Dict, [atom(.),var(Var,S)|Tokens]) :-
        S = [0'I|S0],
        read_name(Typ, Ch, S0, NextCh, NextTyp),
        dic_lookup(Dict, S, Node),
        check_singleton(Node, Var),
        read_tokens(NextTyp, NextCh, Dict, Tokens).

read_after_dot_In(0'f, 1, Inf, Dict, Tokens) :- !,
        Inf is 1/0,
        getct(Ch, Typ),
        read_tokens(Typ, Ch, Dict, Tokens).
read_after_dot_In(Ch, Typ, 0, Dict, [atom(.),var(Var,S)|Tokens]) :-
        S = [0'I,0'n|S0],
        read_name(Typ, Ch, S0, NextCh, NextTyp),
        dic_lookup(Dict, S, Node),
        check_singleton(Node, Var),
        read_tokens(NextTyp, NextCh, Dict, Tokens).

read_based_int(Base, S, EndTyp, EndCh) :-
        MaxDigit is 0'0+Base-1,
        MaxLetter is 0'A+Base-11,
        getct(Ch, Typ),
        read_based_int_digits(Typ, Ch, MaxDigit, MaxLetter, S, EndTyp, EndCh).

read_based_int_digits(3, D, MaxDigit, MaxLetter, [D|Ds], EndTyp, EndCh) :-
        D =< MaxDigit, !,
        getct(Ch, Typ),
        read_based_int_digits(Typ, Ch, MaxDigit, MaxLetter, Ds, EndTyp, EndCh).
read_based_int_digits(2, D, MaxDigit, MaxLetter, [D|Ds], EndTyp, EndCh) :-
        D =< MaxLetter, !,
        getct(Ch, Typ),
        read_based_int_digits(Typ, Ch, MaxDigit, MaxLetter, Ds, EndTyp, EndCh).
read_based_int_digits(1, D, MaxDigit, MaxLetter, [D|Ds], EndTyp, EndCh) :-
        D =< MaxLetter+0'a-0'A, !,
        getct(Ch, Typ),
        read_based_int_digits(Typ, Ch, MaxDigit, MaxLetter, Ds, EndTyp, EndCh).
read_based_int_digits(Typ, Ch, _, _, [], Typ, Ch).

based_int_or_atom([], _, L, 0, Typ, Ch, Dict, [Atom|Tokens]) :- !,
        read_name(Typ, Ch, S0, NextCh, NextTyp),    % not based int, start of
        atom_token([L|S0], Atom),                   % atom with letter L      
        read_tokens(NextTyp, NextCh, Dict, Tokens).
based_int_or_atom(S, Base, _, N, Typ, Ch, Dict, Tokens) :-
        number_codes(N, Base, S),
        read_tokens(Typ, Ch, Dict, Tokens).

based_int_or_quoted([], Base, Base, Typ, Ch, Dict, [Atom|Tokens]) :- !,
        read_string(Typ, Ch, S, 0'', NextTyp, NextCh),  % not based int, start
        atom_token(S, Atom),                            % of quoted atom
        read_tokens(NextTyp, NextCh, Dict, Tokens).
based_int_or_quoted(S, Base, N, Typ, Ch, Dict, Tokens) :-
        number_codes(N, Base, S),
        read_tokens(Typ, Ch, Dict, Tokens).

read_quoted_character(4, 92, N, Dict, Tokens) :- % backslash
        current_prolog_flag(character_escapes, CEF),
        CEF \== off,
        !,
        getct(Ch, Typ),
        escape_sequence(Typ, Ch, CEF, Chars, [], OtTyp, OtCh),
        continue_quoted_character(Chars, N, OtTyp, OtCh, Dict, Tokens).
read_quoted_character(5, 0'', 0'', Dict, Tokens) :-
        current_prolog_flag(character_escapes, iso),
        !,
        getct(Ch, Typ),
        read_another_quote(Typ, Ch, Dict, Tokens).
read_quoted_character(_, N, N, Dict, Tokens) :-
        getct(NextCh, NextTyp),
        read_tokens(NextTyp, NextCh, Dict, Tokens).

continue_quoted_character([], N, Typ, Ch, Dict, Tokens) :- !, % was null
        read_quoted_character(Typ, Ch, N, Dict, Tokens).
continue_quoted_character([N], N, Typ, Ch, Dict, Tokens) :-
        read_tokens(Typ, Ch, Dict, Tokens).

read_another_quote(5, 0'', Dict, Tokens) :- !,
        getct(Ch, Typ),
        read_tokens(Typ, Ch, Dict, Tokens).
read_another_quote(Typ, Ch, Dict, Tokens) :- % Accept also only a "'"
        read_tokens(Typ, Ch, Dict, Tokens).

check_singleton(Node, Var) :-
        var(Node), !, Node = [Var|_].
check_singleton([Var|[]], Var). % The [] marks it is not singleton

atom_token(String, atom(Atom)) :-
        atom_codes(Atom, String), !.
atom_token(String, badatom(String)).

skip_code_prot(C) :- catch(skip_code(C), _, fail).
