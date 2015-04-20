:- export op(150,xfx,'$').
:- export op(150,fx,'$').
:- set_chtab(0'`, string_quote).
:- set_chtab(0'", list_quote).
:- op(1150,fx,multifile).
multifile(_).
meta_predicate(_).
:- local select/3.
