 %% tk_events_sicstus.pl -- tcl/tk event handling for SICStus, after
 %% Micha Meier's sources
 %% Author          : Manuel Carro
 %% Created On      : Fri May 17 16:16:20 1996
 %% Last Modified By: Manuel Carro
 %% Last Modified On: Fri May 17 20:34:36 1996
 %% Update Count    : 86
 %% Status          : Unknown, Use with caution!

:- ensure_loaded(string_utils).

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% tk_do_one_event_atom(Mask, Atom): waits for the next
 %% event available. Returns '' if there are events, but no Prolog
 %% events. Returns the atom with the Tcl list representing the event
 %% otherwise. 
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 %% Written in C.

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% tk_do_one_event(Mask, Atom): gets the next (prolog) event and
 %% returns a true list. This is done by parsing the atom returned by
 %% tk_do_one_event_atom/2. 
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tk_do_one_event(Mask, PrologList):-
        tk_do_one_event_atom(Mask, TclAtom),
        make_prolog_list_from_tcl_atom(TclAtom, PrologList).

tk_do_one_event(Mask):-
        tk_do_one_event_atom(Mask, TclAtom),
        TclAtom \== '$no_event_available'.

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% make_prolog_list_from_tcl_atom(TclAtom, PrologList): an ancillary
 %% predicate to get a PrologList from a Prolog atom which represents
 %% a Tcl list.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_prolog_list_from_tcl_atom('$no_event_available', _):- !, fail.
make_prolog_list_from_tcl_atom(Atom, PrologList):-
        name(Atom, ListChars),
        skip_blanks(ListChars, StrippedListChars),
        tcl_l_to_prolog_l(StrippedListChars, PrologList).

skip_blanks([], []).
skip_blanks([0' |R], S):- !, skip_blanks(R, S).
skip_blanks(R, R):- R = [X|_], X \== 0' .

tcl_l_to_prolog_l([], []):- !.
tcl_l_to_prolog_l(Tcl, [Item|Items]):-
        tcl_item_to_prolog(Tcl, Item, RestTcl),
        skip_blanks(RestTcl, StrRestTcl),
        tcl_l_to_prolog_l(StrRestTcl, Items).

tcl_item_to_prolog([0'{|Rl], Item, RestTcl):- !,
        skip_blanks(Rl, RSk),
        tcl_l_to_prolog(RSk, Item, RestTcl).
tcl_item_to_prolog(List, Atom, RestTcl):-
        first_atom(List, ListChars, RestTcl),
        name(Atom, ListChars).

first_atom([], [], []):- !.
first_atom([0' |Rest], [], Rest):- !.
first_atom([0'}|Rest], [], [0'}|Rest]):- !.
first_atom([Char|Chars], [Char|CharsAtom], Rest):-
        first_atom(Chars, CharsAtom, Rest).

tcl_l_to_prolog([0'}|R], [], R):- !.
tcl_l_to_prolog(Chars, [Item|Atoms], Rest):-
        skip_blanks(Chars, StrChars),
        tcl_item_to_prolog(StrChars, Item, RestChars),
        skip_blanks(RestChars, SkpRestChars),
        tcl_l_to_prolog(SkpRestChars, Atoms, Rest).



%
% Wait for the next Prolog event to occur, serve all other events
%

tk_next_event(List):- tk_next_event(16'1e, List).

tk_next_event(Mask, List) :-
    M is Mask /\ 16'fe,
    (tk_do_one_event(M, L) -> true; L = []),
    (
        tk_num_main_windows(0) ->
	List = [exit]
    ;
        (
            L = [] ->					% no Prolog event
            tk_next_event(Mask, List)
        ;
            List = L
        )
    ).


%
% Process all events currently present in the queue, return the first Prolog
% one if available, else fail
%

tk_get_event(List):- tk_get_event(16'1f, List).

tk_get_event(Mask, List) :-
    M is Mask \/ 1,				% don't wait
    tk_do_one_event(M, L),		% fails if no events
    (
        tk_num_main_windows(0) ->
	List = [exit]
    ;
        (
            L = [] ->				% no Prolog event
            tk_get_event(Mask, List)
        ;
            List = L
        )
    ).
