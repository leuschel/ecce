:- module(_, [
        nonzero/1,
        clpr_freeze/2,
        solve_generic_0/2,
        solve_generic_1/4,
        solve_generic_2/6,
        solve_generic_3/8,
        solve_generic_4/10,
        solve_generic_5/12,
        solve_generic_n/4], ['clpr/clpr_src',assertions]).

:- use_module(library('clpr/solver_r')).
:- use_module(library('clpr/clpr_dump')). % For the toplevel hook dump/3
:- use_module(library('clpr/clpr_attr')).

:- include('../clpqr-common/clp_rt').
