%%------------------------------------------------------------------------
%%
%% O'Ciao: Object Oriented Programming in Ciao/Prolog
%%
%% SYNTAX FILE FOR OBJECT USAGE
%%
%% AUTHOR : Angel Fernandez Pineda
%% DATE   : March 1999
%%
%%------------------------------------------------------------------------

:- use_module(library('objects/objects_rt')).

%%------------------------------------------------------------------------

:- op(700,xfx,[(new),(instance_of),(derived_from),(interface)]).

:- op(900,fx,[(destroy)]).

%%------------------------------------------------------------------------

:- multifile 'class$used'/2.

%%------------------------------------------------------------------------

:- new_declaration(use_class/1,on).
:- new_declaration(instance_of/2,on).

%%------------------------------------------------------------------------

:- load_compilation_module(library('objects/objects_tr')).

:- add_sentence_trans(obj_sentence_trans/3).
:- add_clause_trans(obj_clause_trans/3).
