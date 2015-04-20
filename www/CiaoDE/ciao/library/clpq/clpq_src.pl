:- use_module(library('clpq/eval_q')).
:- load_compilation_module(library('clpq/expand_q')). % May be expand_real or expand_rational
:- add_goal_trans(expand_q:expand/2).
