:- load_compilation_module(library('debugpred/debugpred_tr')).

%:- use_module(library('debugpred/debugpred_rt')).

% :- add_term_trans(debugpred_term_tr/3).

:- add_sentence_trans(debugpred_sentence_tr/3).

:- add_goal_trans(debugpred_goal_tr/3).

:- use_module(library(lists)).

:- op(1150, fx, [debugpred]).

:- debugpredstatus(on).
