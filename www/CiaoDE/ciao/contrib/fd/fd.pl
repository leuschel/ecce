:- include(library('fd/fd_syntax')).
:- use_module(library('fd/fd_rt')).
:- include(library('fd/fd_translation')).

:- load_compilation_module(library('fd/fd_tr')).
:- add_goal_trans(fd_tr/2).
