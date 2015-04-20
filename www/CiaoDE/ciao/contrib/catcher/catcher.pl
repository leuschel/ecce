% A Catcher package for catch exceptions.

:- load_compilation_module(library('catcher/catcher_tr')).

:- use_module(library('catcher/catcher_rt')).

:- add_sentence_trans(catcher_def/3).

%:- comment(bug,"The catcher is not compatible with interrupt/3").
