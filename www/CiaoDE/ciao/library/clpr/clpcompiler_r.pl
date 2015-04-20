:- module(_, [compile_constr/4],[
        dcg,
        'clpr/clpr_src']).

:- use_module(library('clpr/solver_r')).
:- use_module(library('clpr/clpr_dump'), [dump_internal/3]).

:- include('../clpqr-common/clpcompiler').
