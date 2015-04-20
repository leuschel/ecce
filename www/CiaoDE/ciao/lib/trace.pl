:- use_module(library('debugger/debugger_lib'), [debug_module/1, spy/1]).
:- use_module(library('debugger/embedded_rt')).

:- initialization((this_module(M), debug_module(M), trace)).

 
:- new_declaration(spy/1).
:- op(900, fx, [(spy)]).

:- load_compilation_module(library('debugger/embedded_tr')).

:- add_clause_trans(srcdbg_expand/4).
:- add_sentence_trans(srcdbg_expand_decl/3).
