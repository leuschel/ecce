:- include(library('clpqr-common/ops')).
:- use_module(library('clpq/clpq_rt')).
:- use_module(library('clpq/solver_q'),
        [solve_abs/2,solve_mix/4,solve_mult/3,solve_pow/3,solve_trig/3]).

%% :- use_module(library('clpq/clpq_dump')).
:- multifile dump/3.

:- use_module(library('clpq/clpq_meta')).


:- load_compilation_module(library('clpq/clpqtr')).
% :- add_term_trans(clpqtr:translate_hash/2).
:- add_goal_trans(clpqtr:translate_clp/2).
