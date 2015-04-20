:- load_compilation_module( library( 'rtchecks/rtchecks_tr' ) ).

:- add_sentence_trans( transform_as_saved/3 ).
:- add_goal_trans( transform_check/3 ).

:- use_module( library( 'rtchecks/rtchecks_mod' ) ).

:- use_package( assertions ).

:- trust pred '$saved_assertion'( X , Y , Z ).

:- multifile '$saved_assertion'/3.
