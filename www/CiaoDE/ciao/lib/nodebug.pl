:- load_compilation_module(library('debugger/embedded_tr')).
:- use_module(library('debugger/debugger_lib'), [spy/1]).

:- new_declaration(spy/1).
:- op(900, fx, [(spy)]).

:- add_clause_trans(srcdbg_no_expand/4).
:- add_sentence_trans(srcdbg_no_expand_decl/3).

:- initialization(nodebug).

:- use_module(library('debugger/embedded_rt')).
