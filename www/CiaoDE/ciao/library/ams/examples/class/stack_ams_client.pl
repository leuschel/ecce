:- module(stack,[shut_down/0,'obj$push'/2,'obj$pop'/2,'obj$top'/2,'obj$is_empty'/1,'$goalis_empty0'/1,'$goaltop1'/1,'$goalpop1'/1,'$goalpush1'/1,'$goalstorage1'/1,'$factstorage1'/1,'class$goalcalling'/3]).
:- use_module(library('ams/amsrt')).
:- multifile '$ams$actmod'/3,'$ams$app'/1.
:- initialization(start_up("http://clip.dia.fi.upm.es/~clip/stack.pl")).
:- on_abort(shut_down).
:- data my_sid/1.

start_up(Mid):-
    '$ams$app'(App),
    '$ams$actmod'(stack,Mode,Host),
    absolute_file_name(App,FilePath),
    atom_concat('clip.dia.fi.upm.es',FilePath,Suffix1),
    atom_concat('file://',Suffix1,Aid),
    ams_startup(Host,Aid,stack,Mid,Mode,Sid),
    asserta_fact(my_sid(Sid)).

shut_down:-
    my_sid(Sid),
    ams_shutdown(Sid).

:- use_module(library('class/class_rt')).
:- use_module(library('class/virtual')).
:- new_declaration(inherit_class/1,on).
:- new_declaration(implements/1,on).
:- new_declaration(inheritable/1,on).
:- new_declaration(public/1,on).
:- new_declaration(virtual/1,on).
:- new_declaration(persistent/1,on).
:- new_declaration(method/1,on).
:- new_declaration(attribute/1,on).
:- new_declaration(super/1,on).
:- op(1150,fx,[public,inheritable,virtual,method]).
:- op(900,fy,[inherited]).
:- dynamic storage/1.
:- multifile '$class$'/1.
:- multifile 'class$super'/2.
:- multifile 'class$call'/3.
:- multifile 'class$initial_state'/3.
:- multifile 'class$virtual'/6.
:- multifile 'class$attr_template'/4.
:- multifile 'class$default_cons'/1.
:- multifile 'class$constructor'/4.
:- multifile 'class$destructor'/3.
:- multifile 'class$implements'/2.
:- use_module(engine(internals),[last_module_exp/5,'$meta_call'/1]).
:- redefining(mod_exp/5).
:- public(is_empty/0).
:- public(top/1).
:- public(pop/1).
:- public(push/1).
:- inheritable(push/1).
:- inheritable(pop/1).
:- inheritable(top/1).
:- inheritable(is_empty/0).
:- attribute(storage/1).
:- method(is_empty/0).
:- method(top/1).
:- method(pop/1).
:- method(push/1).
:- redefining('obj$is_empty'/1).
:- redefining('obj$top'/2).
:- redefining('obj$pop'/2).
:- redefining('obj$push'/2).

'obj$push'(_4922,_4923) :-
    my_sid(Sid),
    ams_call(Sid,'obj$push'(_4922,_4923)).

'obj$pop'(_5182,_5183) :-
    my_sid(Sid),
    ams_call(Sid,'obj$pop'(_5182,_5183)).

'obj$top'(_5442,_5443) :-
    my_sid(Sid),
    ams_call(Sid,'obj$top'(_5442,_5443)).

'obj$is_empty'(_5702) :-
    my_sid(Sid),
    ams_call(Sid,'obj$is_empty'(_5702)).

'$goalis_empty0'(_5959) :-
    my_sid(Sid),
    ams_call(Sid,'$goalis_empty0'(_5959)).

'$goaltop1'(_6216) :-
    my_sid(Sid),
    ams_call(Sid,'$goaltop1'(_6216)).

'$goalpop1'(_6473) :-
    my_sid(Sid),
    ams_call(Sid,'$goalpop1'(_6473)).

'$goalpush1'(_6730) :-
    my_sid(Sid),
    ams_call(Sid,'$goalpush1'(_6730)).

'$goalstorage1'(_6987) :-
    my_sid(Sid),
    ams_call(Sid,'$goalstorage1'(_6987)).

'$factstorage1'(_7244) :-
    my_sid(Sid),
    ams_call(Sid,'$factstorage1'(_7244)).

'class$goalcalling'(_7501,_7502,_7503) :-
    my_sid(Sid),
    ams_call(Sid,'class$goalcalling'(_7501,_7502,_7503)).

'$class$'(_7764) :-
    my_sid(Sid),
    ams_call(Sid,'$class$'(_7764)).

'class$super'(_8021,_8022) :-
    my_sid(Sid),
    ams_call(Sid,'class$super'(_8021,_8022)).

'class$call'(_8281,_8282,_8283) :-
    my_sid(Sid),
    ams_call(Sid,'class$call'(_8281,_8282,_8283)).

'class$initial_state'(_8544,_8545,_8546) :-
    my_sid(Sid),
    ams_call(Sid,'class$initial_state'(_8544,_8545,_8546)).

'class$virtual'(_8807,_8808,_8809,_8810,_8811,_8812) :-
    my_sid(Sid),
    ams_call(Sid,'class$virtual'(_8807,_8808,_8809,_8810,_8811,_8812)).

'class$attr_template'(_9079,_9080,_9081,_9082) :-
    my_sid(Sid),
    ams_call(Sid,'class$attr_template'(_9079,_9080,_9081,_9082)).

'class$default_cons'(_9345) :-
    my_sid(Sid),
    ams_call(Sid,'class$default_cons'(_9345)).

'class$constructor'(_9602,_9603,_9604,_9605) :-
    my_sid(Sid),
    ams_call(Sid,'class$constructor'(_9602,_9603,_9604,_9605)).

'class$destructor'(_9868,_9869,_9870) :-
    my_sid(Sid),
    ams_call(Sid,'class$destructor'(_9868,_9869,_9870)).

'class$implements'(_10131,_10132) :-
    my_sid(Sid),
    ams_call(Sid,'class$implements'(_10131,_10132)).
