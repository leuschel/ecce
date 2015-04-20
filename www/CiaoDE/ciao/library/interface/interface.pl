%%------------------------------------------------------------------------
%%
%% O'CIAO: Object Oriented Programming in CIAO/Prolog
%%
%% SYNTAX FILE FOR INTERFACE IMPLEMENTATION 
%%
%% AUTHOR : Angel Fernandez Pineda
%% DATE   : July 1999
%%
%%------------------------------------------------------------------------

:- op(1150,fx,[(public),(inheritable),(method)]).

:- op(900,fy,[(inherited)]).

:- new_declaration(public/1,on).
:- new_declaration(method/1,on).
:- new_declaration(attribute/1,on).
:- new_declaration(implements/1,on).

%%------------------------------------------------------------------------

:- load_compilation_module(library('interface/interface_tr')).

:- add_clause_trans(interface_clause_trans/3).
:- add_sentence_trans(interface_sentence_trans/3).
