%
% SICStus version of ProTcl
%
% Author: Micha Meier, Manuel Carro
% Date:   September 93, May 1996
%

%
% sccsid("@(#)stk.pl	1.6          94/09/19").
% sccscr("@(#)  Copyright 1993 ECRC GmbH ").
%

% augmented with more primitives from the eclipse sources. MCL

:- ensure_loaded(foreign).
:- ensure_loaded(tk_common).
:- ensure_loaded(tcl).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% tk_do_one_event_atom(Mask, Atom): waits for the next
 %% event available. Returns '' if there are events, but no Prolog
 %% events. Returns the atom with the Tcl list representing the event
 %% otherwise. In case of Tk error, returns the atom
 %% '$Tk_DoOneEvent_Failed' 
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 %% Written in C.


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% tk_do_one_event_atom_list(Mask, List): waits for the next
 %% event available. Returns a list of Prolog atoms, or [] if there
 %% are events, but no Prolog events. Fails in case of Tk failure.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tk_do_one_event_atom_list(Mask, PrologList):-
        tk_do_one_event_atom(Mask, TclAtom),
        TclAtom \== '$Tk_DoOneEvent_Failed',
        make_prolog_list_from_tcl_atom(TclAtom, PrologList, atoms).



 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% tk_do_one_event(Mask, Atom): wais for the next Maskable event and
 %% returns a true list, or [] if there is no event available. Fail in
 %% case of Tk failure.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tk_do_one_event(Mask, PrologList):-
        tk_do_one_event_atom(Mask, TclAtom),
        TclAtom \== '$Tk_DoOneEvent_Failed',
        make_prolog_list_from_tcl_atom(TclAtom, PrologList, strings).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% tk_do_one_event(Mask): awaits for the next Maskable event. Fails
 %% in case of Tk failure.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tk_do_one_event(Mask):-
        tk_do_one_event_atom(Mask, TclAtom),
        TclAtom \== '$Tk_DoOneEvent_Failed'.


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% make_prolog_list_from_tcl_atom(TclAtom, PrologList, Format): an ancillary
 %% predicate to get a (nested) PrologList from a Prolog atom which represents
 %% a Tcl list. The items of the list are atoms if Format is 'atoms'
 %% or a free variable, and strings otherwise.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_prolog_list_from_tcl_atom(Atom, PrologList, AtomsOrStrings):-
        name(Atom, ListChars),
        skip_blanks(ListChars, StrippedListChars),
        tcl_l_to_prolog_l(StrippedListChars, PrologList, AtomsOrStrings).

skip_blanks([], []).
skip_blanks([0' |R], S):- !, skip_blanks(R, S).
skip_blanks(R, R):- R = [X|_], X \== 0' .

tcl_l_to_prolog_l([], [], _AtomsOrStrings):- !.
tcl_l_to_prolog_l(Tcl, [Item|Items], AtomsOrStrings):-
        tcl_item_to_prolog(Tcl, Item, RestTcl, AtomsOrStrings),
        skip_blanks(RestTcl, StrRestTcl),
        tcl_l_to_prolog_l(StrRestTcl, Items, AtomsOrStrings).

tcl_item_to_prolog([0'{|Rl], Item, RestTcl, AtomsOrStrings):- !,
        skip_blanks(Rl, RSk),
        tcl_l_to_prolog(RSk, Item, RestTcl, AtomsOrStrings).
tcl_item_to_prolog(List, AtomOrString, RestTcl, AtomsOrStrings):-
        first_atom(List, ListChars, RestTcl),
        (
            AtomsOrStrings = atoms ->
            name(AtomOrString, ListChars)
        ;
            AtomOrString = ListChars
        ).

first_atom([], [], []):- !.
first_atom([0' |Rest], [], Rest):- !.
first_atom([0'}|Rest], [], [0'}|Rest]):- !.
first_atom([Char|Chars], [Char|CharsAtom], Rest):-
        first_atom(Chars, CharsAtom, Rest).

tcl_l_to_prolog([0'}|R], [], R, _AtomsOrStrings):- !.
tcl_l_to_prolog(Chars, [Item|Atoms], Rest, AtomsOrStrings):-
        skip_blanks(Chars, StrChars),
        tcl_item_to_prolog(StrChars, Item, RestChars, AtomsOrStrings),
        skip_blanks(RestChars, SkpRestChars),
        tcl_l_to_prolog(SkpRestChars, Atoms, Rest, AtomsOrStrings).
