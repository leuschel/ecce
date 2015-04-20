:- module(_, [translate_clp/2
          % , translate_hash/2
             ],['clpqr-common/ops']).

:- use_module(library('clpq/clpcompiler_q'), [compile_constr/4]).
:- use_module(library('clpq/clpq_attr')). % To define attribute hooks

:- include('../clpqr-common/clptr').
