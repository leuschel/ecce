%%
%% Class sun.io.CharToByteConverter
%% NOTICE: Do not edit this file. This file has been generated from:
%% java.awt.event.ActionEvent
%%

:- class('CharToByteConverter').
:- use_package(objects).

ojo> es publico
:- export('CharToByteConverter'/0).
:- export(byteOff/1).
ojo> es publico
:- export(canConvert/2).
ojo> es publico
:- export(convert/7).
ojo> es publico
:- export(convertAll/2).
ojo> es publico
:- export(convertAny/7).
ojo> es publico
:- export(flush/4).
ojo> es publico
:- export(flushAny/4).
ojo> es publico
:- export(getBadInputLength/1).
ojo> es publico
:- export(getCharacterEncoding/1).
ojo> es publico
:- export(getConverter/2).
ojo> es publico
:- export(getDefault/1).
ojo> es publico
:- export(getMaxBytesPerChar/1).
ojo> es publico
:- export(nextByteIndex/1).
ojo> es publico
:- export(nextCharIndex/1).
ojo> es publico
:- export(reset/0).
ojo> es publico
:- export(setSubstitutionBytes/1).
ojo> es publico
:- export(setSubstitutionMode/1).
:- export(subBytes/1).
ojo> es publico
:- export(toString/1).

%%--------------------------------------------------
%% Interface Information.
%%--------------------------------------------------



%%--------------------------------------------------
%% Inheritance information.
%%--------------------------------------------------

:- inherit_class(library('javaobs/java/lang/Object')).

%%--------------------------------------------------
%% Declared classes.
%%--------------------------------------------------

:- use_class(library('javaobs/java/lang/String')).


%%--------------------------------------------------
%% Miscelanea.
%%--------------------------------------------------

:- redefining(_).
:- set_prolog_flag(multi_arity_warnings,off).

:- discontiguous java_assert/2.
:- use_module(library(lists)).

:- public(java_constructor/1).
:- public(java_invoke_method/1).
:- public(java_get_value/1).
:- public(java_set_value/1).
:- public(java_add_listener/2).
:- public(java_remove_listener/2).
:- public(java_delete_object/0).
:- public(get_java_id/1).
:- use_class(library('javaobs/java_obj')).

%%--------------------------------------------------
%% Destructor.
%%--------------------------------------------------

destructor :-
    java_delete_object.

%%--------------------------------------------------
%% Java fields.
%%--------------------------------------------------


%%--------------------------------------------------
%% Constructors.
%%--------------------------------------------------

'CharToByteConverter' :-
    java_constructor('sun.io.CharToByteConverter').



%%--------------------------------------------------
%% Methods.
%%--------------------------------------------------

canConvert(_V0, Result) :-
    var(Result),
    integer(_V0),
    java_invoke_method(canConvert(_V0, Result)).

convertAll(_V0, Result) :-
    var(Result),
    list(_V0),
    java_invoke_method(convertAll(_V0, Result)).

convertAny(_V0, _V1, _V2, _V3, _V4, _V5, Result) :-
    var(Result),
    list(_V0),
    integer(_V1),
    integer(_V2),
    list(_V3),
    integer(_V4),
    integer(_V5),
    java_invoke_method(convertAny(_V0, _V1, _V2, _V3, _V4, _V5, Result)).

flushAny(_V0, _V1, _V2, Result) :-
    var(Result),
    list(_V0),
    integer(_V1),
    integer(_V2),
    java_invoke_method(flushAny(_V0, _V1, _V2, Result)).

getBadInputLength(Result) :-
    var(Result),
    java_invoke_method(getBadInputLength(Result)).

getConverter(_V0, Result) :-
    var(Result),
    atom(_V0),
    java_invoke_method(getConverter(_V0, Result)).

getDefault(Result) :-
    var(Result),
    java_invoke_method(getDefault(Result)).

nextByteIndex(Result) :-
    var(Result),
    java_invoke_method(nextByteIndex(Result)).

nextCharIndex(Result) :-
    var(Result),
    java_invoke_method(nextCharIndex(Result)).

setSubstitutionBytes(_V0) :-
    list(_V0),
    java_invoke_method(setSubstitutionBytes(_V0, _)).

setSubstitutionMode(_V0) :-
    interface(_V0, boolean), interface(_V0, java_obj), _V0:get_java_id(_V0OBJ),
    java_invoke_method(setSubstitutionMode(_V0OBJ, _)).

toString(Result) :-
    var(Result),
    java_invoke_method(toString(Result)).


:- set_prolog_flag(multi_arity_warnings,on).
