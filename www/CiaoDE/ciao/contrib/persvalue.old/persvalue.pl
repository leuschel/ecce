:- module(persvalue, [query_opt/3, query_opts/1, query_group_opts/1,
   configure_value/4, add_predefined_value/2, get_name_value/2,
   init_persvalue/1, group_name/2, default_group_opts/1,
   show_group_opts/1, get_string/1, get_string/2, get_atom/1,
   get_atom/2], [iso, persdb]).

:- use_module(library(system)).
:- use_module(library(distutils)).

% :- use_package(iso).
% :- use_package(persdb).

% persistent_dir(dbdir, '~/.ciao.d').

%:- persistent(name_value/2, dbdir).
:- data name_value/2.

:- data predefined_value/2.
:- data name_value_tmp/2.
:- data group_name/2.
:- data name_help/2.

% predefined will be used in the main parameters processer for skip
% pre-defined options.

% name_value(X,Y) :-
% 	persdbrt:current_fact(name_value_dyn(X,Y)).
get_name_value(X,Y) :-
	current_fact(name_value(X,Y)) -> true
 ;
	message(error,['Could not get value ', Y, ' for name ', X]),
	fail.

init_persvalue(Dir) :-
	persdbrt:asserta_fact(persistent_dir(persvaluedir,Dir)),
	data(name_value/2),
	make_persistent(name_value/2, persvaluedir).

add_predefined_value(Name, Value) :-
	data_facts:assertz_fact(predefined_value(Name, Value)).

configure_value(Group, Name, Value, Help) :-
	(   group_name(Group,Name) -> true
	;
	    data_facts:assertz_fact(group_name(Group,Name))
	),
	(   name_help(Name, _) ->
	    data_facts:retract_fact(name_help(Name, _))
	;
	    true
	),
	data_facts:assertz_fact(name_help(Name,Help)),
	(
	    name_value(Name, _) -> true
	;
	    persdbrt:assertz_fact(name_value(Name,Value))
	).

%	    persdbrt:retract_fact(name_value(Name,_))

% set_default_env_value(Group, Name, EnvName, Value) :-
% 	current_env(EnvName,EnvValue) ->
% 	set_default_value(Group, Name, EnvValue)
%  ;
% 	set_default_value(Group, Name, Value).

query_opt(Name, Value0, Value) :-
	(   predefined_value(Name, Value) ->
	    display('Predefined: '), display(Name), display('=['), display(Value), display('].'),nl
	;
	    name_help(Name,Help),
	    display(Help),nl,
	    display(Name),display('=['), display(Value0), display('] ? '), get_atom(Value1),
	    (   Value1 = '' ->
		Value = Value0
	    ;
		Value = Value1
	    )
	).

query_opts([]).
query_opts([Group|Groups]) :-
	query_group_opts(Group),
	!,
	query_opts(Groups).

query_group_opts(Group) :-
	just_query_group_opts(Group),
	update_values(Group).

just_query_group_opts(Group) :-
	display('*** Getting options for '),display(Group),display(' ***\n'),
	group_name(Group,Name),
	name_value(Name,Value0),
	query_opt(Name,Value0,Value),
	data_facts:assertz_fact(name_value_tmp(Name,Value)),
	fail.
just_query_group_opts(_).

update_values(Group) :-
	group_name(Group,Name),
	name_value_tmp(Name,Value),
	persdbrt:retract_fact(name_value(Name,_)),
	persdbrt:assertz_fact(name_value(Name,Value)),
	data_facts:retract_fact(name_value_tmp(Name,Value)),
	fail.
update_values(_).

default_group_opts(Group) :-
	group_name(Group,Name),
	(   predefined_value(Name, Value) ->
	    true
	;
	    name_value(Name,Value)
	),
	data_facts:assertz_fact(name_value_tmp(Name,Value)),
	fail.
default_group_opts(Group) :-
	update_values(Group).

show_group_opts(Group) :-
	display('*** --------------------------------------------------------\n'),
	display('*** Showing options for '),display(Group),display(' ***\n'),
	display('*** --------------------------------------------------------\n'),
	group_name(Group,Name),
	name_value(Name,Value),
	display(Name),display(' ='),display(Value),nl,fail.
show_group_opts(_).

get_atom(Atom) :-
	current_input(S),
	get_atom(S, Atom).
get_atom(Stream, Atom) :-
	get_string(Stream, String),
	atom_codes(Atom, String).

get_string(Stream, String) :-
	get_code(Stream, Code),
	(   "\n" = [Code] ->
	    String = []
	;
	    String = [Code|String2],
	    get_string(Stream, String2)
	).
get_string(String) :-
	current_input(S),
	get_string(S, String).
