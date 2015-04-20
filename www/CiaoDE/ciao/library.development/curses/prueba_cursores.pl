:- ensure_loaded(curses).

do:-
        init_term,
        cls,
        my_write('xxx', 50),
        my_write("yyyyy", 50),
        close_term.


my_write(_, 0):- !.
my_write(String, Where):-
        tmove(Where, Where),
        twrite(String),
        twrite('ohoho'),
        NewWhere is Where - 1,
        my_write(String, NewWhere).
