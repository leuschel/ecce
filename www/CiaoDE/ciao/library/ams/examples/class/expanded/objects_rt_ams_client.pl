:- module(objects_rt,[shut_down/0,new/2,static_new/2,instance_of/2,derived_from/2,interface/2,instance_codes/2,destroy/1,use_class/1,constructor/1,class_name/1,interface_name/1,instance_id/1,class_source/1,interface_source/1,method_spec/1,virtual_method_spec/1]).
:- use_module(library('ams/amsrt')).
:- multifile '$ams$actmod'/3,'$ams$app'/1.
:- initialization(start_up("http://clip.dia.fi.upm.es/~clip/objects_rt.pl")).
:- on_abort(shut_down).
:- data my_sid/1.

start_up(Mid):-
    '$ams$app'(App),
    '$ams$actmod'(objects_rt,Mode,Host),
    absolute_file_name(App,FilePath),
    atom_concat('clip.dia.fi.upm.es',FilePath,Suffix1),
    atom_concat('file://',Suffix1,Aid),
    ams_startup(Host,Aid,objects_rt,Mid,Mode,Sid),
    asserta_fact(my_sid(Sid)).

shut_down:-
    my_sid(Sid),
    ams_shutdown(Sid).

new(_5049,_5050) :-
    my_sid(Sid),
    ams_call(Sid,new(_5049,_5050)).

static_new(_5309,_5310) :-
    my_sid(Sid),
    ams_call(Sid,static_new(_5309,_5310)).

instance_of(_5569,_5570) :-
    my_sid(Sid),
    ams_call(Sid,instance_of(_5569,_5570)).

derived_from(_5829,_5830) :-
    my_sid(Sid),
    ams_call(Sid,derived_from(_5829,_5830)).

interface(_6089,_6090) :-
    my_sid(Sid),
    ams_call(Sid,interface(_6089,_6090)).

instance_codes(_6349,_6350) :-
    my_sid(Sid),
    ams_call(Sid,instance_codes(_6349,_6350)).

destroy(_6609) :-
    my_sid(Sid),
    ams_call(Sid,destroy(_6609)).

use_class(_6866) :-
    my_sid(Sid),
    ams_call(Sid,use_class(_6866)).

constructor(_7123) :-
    my_sid(Sid),
    ams_call(Sid,constructor(_7123)).

class_name(_7380) :-
    my_sid(Sid),
    ams_call(Sid,class_name(_7380)).

interface_name(_7637) :-
    my_sid(Sid),
    ams_call(Sid,interface_name(_7637)).

instance_id(_7894) :-
    my_sid(Sid),
    ams_call(Sid,instance_id(_7894)).

class_source(_8151) :-
    my_sid(Sid),
    ams_call(Sid,class_source(_8151)).

interface_source(_8408) :-
    my_sid(Sid),
    ams_call(Sid,interface_source(_8408)).

method_spec(_8665) :-
    my_sid(Sid),
    ams_call(Sid,method_spec(_8665)).

virtual_method_spec(_8922) :-
    my_sid(Sid),
    ams_call(Sid,virtual_method_spec(_8922)).
