:- include(library('dclp/and/syntax')).
:- use_module(library('dclp/and/and_rt')).

:- use_package(fd).

:- load_compilation_module(library('dclp/and/and_tr')).
:- add_goal_trans(and_tr/2).
