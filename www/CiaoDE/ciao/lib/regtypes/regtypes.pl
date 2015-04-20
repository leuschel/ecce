:- load_compilation_module(library('regtypes/regtypes_tr')).
:- add_sentence_trans(expand_regtypes/2).

:- new_declaration(regtype/1).
:- new_declaration(regtype/2).

:- op(1150, fx,(regtype)).      
:- op(1150,xfx,(regtype)).

%% in basic_props: :- meta_predicate regtype(goal).
