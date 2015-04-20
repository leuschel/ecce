:- module(use_stack,[test/0],ciaopp).

%changed :- use_module(library('objects/objects_rt')).
%notworking???:- use_module(library('ams/examples/class/expanded/objects_rt')).
:- use_module(objects_rt_opt).

:- op(700,xfx,[new,instance_of,derived_from,interface]).

:- op(900,fx,[destroy]).

:- multifile 'class$used'/2.

:- new_declaration(use_class/1,on).

:- new_declaration(instance_of/2,on).

:- use_module(stack,[]).

:- redefining(_5579/_5580).

:- use_class(stack).

test :-
        objects_rt:new(X,stack),
        'class$call'(X,push(a),use_stack_module),
        'class$call'(X,push(b),use_stack_module),
        'class$call'(X,pop(E1),use_stack_module),
        display(E1),
        'class$call'(X,pop(E2),use_stack_module),
        display(E2),
        'class$call'(X,is_empty,use_stack_module).

'$static_instance_creation$' :-
        intercept(true,_1,inform_user(['Static instances declared on ',use_stack_module,'could not be created due to exception: ',_1])).

:- multifile 'class$call'/3.

'$force$rt$info$'(_1) :-
        call(_1).
