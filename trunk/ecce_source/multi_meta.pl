:- meta_predicate pre_condition(  goal ).
:- meta_predicate post_condition( goal ).
:- meta_predicate self_check(     goal ).

:- multifile pre_condition/1.
:- multifile post_condition/1.
:- multifile ecce_type/2.
:- multifile self_check/1.

:- dynamic pre_condition/1.
:- dynamic post_condition/1.
:- dynamic ecce_type/2.
:- dynamic self_check/1.

