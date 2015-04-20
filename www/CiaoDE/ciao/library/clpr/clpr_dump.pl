:- module(_, [dump_internal/3], [
        dcg,
        'clpqr-common/ops',
        'clpr/clpr_src']).

:- use_module(library('clpr/solver_r')).

:- include('../clpqr-common/clp_dump').
:- include('../clpqr-common/fourier_motzkin').
