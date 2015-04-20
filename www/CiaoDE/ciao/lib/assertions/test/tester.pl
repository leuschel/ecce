
%% :- module(tester,[current_host/1,hoopla/3,hobble/2],[assertions,isomodes]).
%% :- module(tester,[],[assertions,isomodes]).

:- use_package([assertions,regtypes]).
%% :- ensure_loaded(tester_aux3).

:- comment(filetype,user).

:- use_module(tester_aux1).
:- comment(doinclude,current_host/1).
%% current_host(X) :- tester_aux1:current_host(X).

current_host(foo).

foo(bar).

:- impl_defined([cucu/1,whoa/3]).

:- pred whoa/3.

:- dynamic hoopla/3.

:- impl_defined(hoopla/3).

:- data hobble/2.
