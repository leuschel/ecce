:- module(_, [string2term/2, parse_term/3], []).

string2term(S, T) :- parse_term(S, T, []).

parse_term([], '', []).
parse_term([C|S], T, S_) :-
        parse_term_(C, S, T, S_).

parse_term_(0'[, [C|S], T, S_) :-
        parse_list(C, S, T, S_).
parse_term_(0''',[C|S], T, S_) :-
        parse_quoted(C, S, T, S_).
parse_term_(V,   [C|S], _T, S_) :-
        code_class(V, 2), !, % Capital letter or _
        parse_name(C, S, _Name, S_).
parse_term_(D,   [C|S], _T, S_) :-
        parse_name(C, S, Name, S1),
        atom_codes(A, [D|Name]),
        parse_after_atom
        
atom
'atom'
[]
[T,T]
[T|T]
functor(T,T)
Variable
