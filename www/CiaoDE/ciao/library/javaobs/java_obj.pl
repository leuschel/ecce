
%% --------------------------------------------------------------------------
%%
%% JAVA internal classes in O'CIAO
%% This file includes the java_obj class, the internal class that communicates
%% to the java object server via sockets.
%% This class must not be accessed out of the automatically generated code
%% implementing java classes.
%%
%% --------------------------------------------------------------------------

:- class('java_obj',[],[objects]).

:- export(java_constructor/1).
:- export(java_invoke_method/1).
:- export(java_get_value/1).
:- export(java_set_value/1).
:- export(java_add_listener/2).
:- export(java_remove_listener/2).
:- export(java_delete_object/0).
%:- export(java_set_interface/1).
:- export(get_java_id/1).
:- reexport(library('javall/javart'),[java_disconnection/0]).

:- set_prolog_flag(multi_arity_warnings,off).
:- use_module(library('javall/javart')).
:- use_module(library(lists),[append/3]).
:- use_module(engine(internals),[term_to_meta/2, module_concat/3]).

%% --------------------------------------------------------------------------
%%  Attributes.
%% --------------------------------------------------------------------------

:- concurrent '$java_id'/1.
:- data      '$java_id'/1.
:- multifile '$java_connection'/1.
:- data      '$java_connection'/1.
:- data      '$java_interface'/1.

'$java_interface'(java_obj).

%% --------------------------------------------------------------------------
%% Constructors. New object creation.
%% --------------------------------------------------------------------------

java_constructor(Constructor) :-
	nonvar(Constructor),
        ( '$java_connection'(_) -> true
          ;
          javart:java_connection,
          set_fact('$java_connection'(clip))),
          javart:java_create_object(Constructor, Id),
          set_fact('$java_id'(Id)).

%% --------------------------------------------------------------------------
%% Methods.
%% --------------------------------------------------------------------------

%% 
%% Method invocation. Argument must be a structure MethodName(Args).
%% In the Args sequence of terms, the last term  should be a variable
%% to return the value returned by the java method.
%%
java_invoke_method(JavaMethod) :-
        '$java_id'(JavaObj),
        javart:java_invoke_method(JavaObj,JavaMethod).

%%
%% Field unification. Argument must be a structure FieldName(FieldValue).
%%
java_get_value(Field) :-
        '$java_id'(JavaObj),
        javart:java_get_value(JavaObj, Field).

%%
%% Field set. Argument must be a structure FieldName(FieldValue).
%%
java_set_value(Field) :-
        '$java_id'(JavaObj),
        javart:java_set_value(JavaObj,Field).

%%
%% Event listening.
%%
%%%%%%%%%%:- meta_predicate java_add_listener(?,goal).

java_add_listener(Event, Goal) :-
        '$java_id'(JavaObj),
	Goal =..[':', Module, Goal2],
	module_concat(Module,Goal2,Goal3),
	term_to_meta(Goal3,RealGoal),
        javart:java_add_listener(JavaObj, Event, RealGoal).

%%
%% Listener removing.
%%
java_remove_listener(Event, Predicate) :-
        '$java_id'(JavaObj),
        javart:java_remove_listener(JavaObj, Event, Predicate).

%%
%% Object deletion.
%%
java_delete_object :-
        '$java_id'(JavaObj),
        javart:java_delete_object(JavaObj).

%%
%% Java identifier. Used to refer the arguments of a 
%% java method that are java objects themselves.
%%
get_java_id(JavaId) :-
	var(JavaId),
	'$java_id'(JavaId).

%%
%% setting of interface information.
%%
%java_set_interface(Interface) :-
%	assertz_fact('$java_interface'(Interface)).

%%
%% java instantiation information.
%%
%java_instance_of(Interface) :-
%	'$java_interface'(Interface).

%java_instance_of(java_obj).

:- set_prolog_flag(multi_arity_warnings,on).
