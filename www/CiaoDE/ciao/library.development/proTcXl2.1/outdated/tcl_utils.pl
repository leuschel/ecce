 %% tcl_utils.pl -- 
 %% AFSID           : $__Header$
 %% Author          : Micha Meier
 %% Created On      : ???
 %% Last Modified By: Manuel Carro
 %% Last Modified On: Thu May 16 16:21:52 1996
 %% Update Count    : 9
 %% Status          : Not working

%% HISTORY 
 %% 14-May-1996		Manuel Carro	
 %%    Last Modified: Tue May 14 19:12:38 1996 #1 (Manuel Carro)
 %%    Adapted to SICStus Prolog

:- ensure_loaded(string_utils).


% Convert Prolog lists to Tcl lists

tcl_string(X, String) :-
    nonvar(X),
    X = [H|T],
    !,
    tcl_element(H, El),
    tcl_list_elements(T, List),
    concat_string([' {', El|List], String).
tcl_string(A, String) :-
    tcl_element(A, String).

tcl_element(Var, S) :-
    var(Var),
    !,
    error(4, tcl_string(Var, S)).
tcl_element([], "{}") :- !.
tcl_element([H|T], String) :-
    !,
    tcl_string([H|T], String).
 %% tcl_element(H, String) :-
 %%     rational(H),
 %%     !,
 %%     F is float(H),
 %%     number_string(F, String).
 %% tcl_element(N, String) :-
 %%     bignum(N),
 %%     !,
 %%     F is float(N),
 %%     number_string(F, String).
tcl_element(N, String) :-
    number(N),
    !,
    number_string(N, String).
tcl_element(A, String) :-
    atom(A),
    !,
    atom_string(A, S),
    tcl_element(S, String).
tcl_element(S, String) :-
    string(S),
    !,
    (substring(S, " ", _) ->
	concat_string(['{', S, '}'], String)
    ;
	String = S
    ).
tcl_element(Struct, Tcl) :-
    term_string(Struct, S),
    tcl_element(S, Tcl).

tcl_list_elements(X, S) :-
    var(X),
    !,
    error(4, tcl_string(X, S)).
tcl_list_elements([], ['}']) :- !.
tcl_list_elements([H|T], [' ', String|L]) :-
    !,
    tcl_element(H, String),
    tcl_list_elements(T, L).
tcl_list_elements(S, _) :-
    error(5, tcl_string([_|S], _)).
