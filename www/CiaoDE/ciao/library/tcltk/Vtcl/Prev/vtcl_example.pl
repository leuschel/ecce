%%
%% This code will be called from the Tcl Script:
%%
%% vtcl_example.tcl
%%
%% Enjoy !

:- module(vtcl_example,
	[data_base/2,add_data/2,who_in_database/1]).

:- include(library(persdb)).
:- use_module(library(lists),[append/3]).

:- multifile persistent_dir/2.
:- data persistent_dir/2.

:- persistent(data_base/2,mydb).

persistent_dir(mydb,dbdir).

add_data(X,Y) :- 
	atom(X),
	atom(Y),
	passertz_fact(data_base(X,Y)).

data_base('John','555-6161').
data_base('George','555-3232').
data_base('Ringo','555-6252').
data_base('Paul','555-3276').


who_in_database(String) :-
	findall(X,data_base(X,_),String).
%	list_to_tclstring(L,String).

%list_to_tclstring([],"").

%list_to_tclstring([Term|Nt],String) :-
%	list_to_tclstring(Nt,Rest),
%	append(Term," ",Aux),
%	append(Aux,Rest,String).
