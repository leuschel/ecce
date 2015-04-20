% The Truster package.

:- load_compilation_module(library('truster/truster_tr')).

% :- use_module(library('truster/truster_rt')).

:- add_sentence_trans(truster_def/3).

:- op(1150, fx, [truster]).

:- op(1050, xfy, ['->']).
