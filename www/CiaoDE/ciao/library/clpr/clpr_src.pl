:- use_module(library('clpr/eval_r')).
:- load_compilation_module(library('clpr/expand_r')). % May be expand_real or expand_rational
:- add_goal_trans(expand_r:expand/2).
