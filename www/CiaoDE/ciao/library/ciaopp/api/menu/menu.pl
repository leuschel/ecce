:- load_compilation_module( library('ciaopp/api/menu/menu_tr') ).

:- add_sentence_trans(menu_term_expansion/3).

:- include( library( 'ciaopp/api/menu/menu_op' ) ).


functor1( A , F ) :- functor( A , F , _ ).

% EXAMPLE: 
% ana , title # flag - option : pre_action :: post_action <- guard.
