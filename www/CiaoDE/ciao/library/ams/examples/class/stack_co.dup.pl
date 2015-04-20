%:- module(_stack,[],ciaopp).
:- module(stack,
	[ 'obj$push'/2,
	  'obj$pop'/2,
	  'obj$top'/2,
	  'obj$is_empty'/1,
	  '$goalis_empty0'/1,
	  '$goaltop1'/1,
	  '$goalpop1'/1,
	  '$goalpush1'/1,
	  '$goalstorage1'/1,
	  '$factstorage1'/1,
	  'class$goalcalling'/3
	],[]).

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

'obj$push'(Item,_1) :-
        nonvar(Item),
        class_rt:asserta_attr(':stack::storage'(Item),_1).

'obj$pop'(Item,_1) :-
        var(Item),
        class_rt:retract_attr(':stack::storage'(Item),_1).

'obj$top'(Top,_1) :-
        class_rt:current_attr(':stack::storage'(Top),_1),
        !.

'obj$is_empty'(_1) :-
        class_rt:current_attr(':stack::storage'(_2),_1),
        !,
        fail.

'obj$is_empty'(_1).

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

'$class$'(stack).

'$force$runtime$info$'(_1) :-
        call(class_rt(_1):_1).

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

'$end$$of$$expansion$'.

'class$attr_template'(stack,':stack::storage',1,dynamic).

'class$implements'(stack,stack).

'class$default_cons'(stack).

'fact$exp'(storage(_3),_1,_2) :-
        class_rt:functor_concat(_1,':stack::storage'(_3),_2).

'goal$exp'(is_empty,_1,'stack:obj$is_empty'(_1)).

'goal$exp'(_1,_2,_3) :-
        stack:'fact$exp'(_1,_2,_3).

'$goalis_empty0'(_1).

'$goaltop1'(_1).

'$goalpop1'(_1).

'$goalpush1'(_1).

'$goalstorage1'(stack).

'$factstorage1'(stack).

mod_exp(goal,is_empty,_1,_2,'stack:obj$is_empty'(_2)) :-
        stack:'$goalis_empty0'(_1).

mod_exp(goal,top(_3),_1,_2,'stack:obj$top'(_3,_2)) :-
        stack:'$goaltop1'(_1).

mod_exp(goal,pop(_3),_1,_2,'stack:obj$pop'(_3,_2)) :-
        stack:'$goalpop1'(_1).

mod_exp(goal,push(_3),_1,_2,'stack:obj$push'(_3,_2)) :-
        stack:'$goalpush1'(_1).

mod_exp(goal,storage(_4),_1,_2,_3) :-
        stack:'$factstorage1'(_1),
        class_rt:functor_concat(_2,':stack::storage'(_4),_3).

mod_exp(fact,storage(_4),_1,_2,_3) :-
        stack:'$factstorage1'(_1),
        class_rt:functor_concat(_2,':stack::storage'(_4),_3).

mod_exp(_1,_2,_3,_4,_5) :-
        throw(error(existence_error(object_goal,_2),_3)),
        !,
        fail.

'class$goalcalling'(is_empty,_1,_2) :-
        stack:'$goalis_empty0'(_2),
        !,
        stack:'obj$is_empty'(_1).

'class$goalcalling'(top(_3),_1,_2) :-
        stack:'$goaltop1'(_2),
        !,
        stack:'obj$top'(_3,_1).

'class$goalcalling'(pop(_3),_1,_2) :-
        stack:'$goalpop1'(_2),
        !,
        stack:'obj$pop'(_3,_1).

'class$goalcalling'(push(_3),_1,_2) :-
        stack:'$goalpush1'(_2),
        !,
        stack:'obj$push'(_3,_1).

'class$goalcalling'(storage(_3),_1,_2) :-
        stack:'$factstorage1'(_2),
        class_rt:functor_concat(_1,':stack::storage'(_3),_4),
        !,
        '$meta_call'(_4).

'class$goalcalling'(_1,_2,_3) :-
        throw(error(existence_error(object_goal,_1),_3)).

'class$call'(stack(_3),_1,_2) :-
        stack:'class$goalcalling'(_1,_3,_2).
