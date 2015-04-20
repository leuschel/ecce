:- ensure_loaded(tk).
:- ensure_loaded(xlib).

e0:-
        tk([]),
        tcl(update),
        xlib_init(., Xid),
        xlib_foreground(Xid, purple),
        xlib_fill_rectangle(Xid, 10, 10, 100, 100),
        xlib_foreground(Xid, black),
        xlib_rectangle(Xid, 10, 10, 100, 100).
