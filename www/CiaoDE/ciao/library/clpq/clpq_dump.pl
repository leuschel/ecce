:- module(_, [dump_internal/3], [
        dcg,
        'clpqr-common/ops',
        'clpq/clpq_src']).

:- use_module(library('clpq/solver_q')).

:- include('../clpqr-common/clp_dump').
:- include('../clpqr-common/fourier_motzkin').
