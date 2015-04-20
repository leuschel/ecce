:- module(_, [translate_clp/2
          % , translate_hash/2
             ],['clpqr-common/ops']).

:- use_module(library('clpr/clpcompiler_r'), [compile_constr/4]).
:- use_module(library('clpr/clpr_attr')). % To define attribute hooks

:- include('../clpqr-common/clptr').
