:- module(curses, [
        initscr/0,closescr/0,cls/0,moveto/2,getyx/2,scrput/1
        ],
        [assertions, basimodes, foreign_interface]).

:- true pred initscr + foreign.
:- true pred closescr + foreign.
:- true pred cls + foreign.
:- true pred moveto(in(Y),in(X)) :: int * int + foreign.
:- true pred getyx(go(Y),go(X)) :: int * int + foreign.
:- true pred scrputatom(in(A)) :: atm + foreign.

foreign(cls, cls).
foreign(tmove, tmove(+integer, +integer)).      % C type: long
foreign(tget, tget(-integer, -integer)).        % C type: long *
foreign(twrite_prolog, twrite_prolog(+string)). % C type: char *
foreign(init_term, init_term).
foreign(close_term, close_term).

twrite(X):-
        atom(X),!,                              % Atoms are directly printed
        twrite_prolog(X).
twrite(X):-
        integer(X),!,
        name(X, CharsofInteger),
        atom_chars(AtomOfInteger, CharsofInteger),
        twrite_prolog(AtomOfInteger).
twrite(X):-
        X = [_|_],
        atom_chars(Atom, X), !,
        twrite_prolog(Atom).
twrite(_):-
        write(user, 'twrite/1 found unknown format'),
        nl(user).

:- 
        interface_curses_name(File), 
        load_foreign_files([File], ['-lcurses', '-ltermcap']).
