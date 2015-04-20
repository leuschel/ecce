:- module(autoconfig, _, []).

:- use_module(library(system)).
:- use_module(library(format)).
:- use_module(library(compiler), [use_module/1]).
:- use_module(library('autoconfig/autoconfig_base')).

:- initialization(autoconfig_components).

:- multifile component_description/4.

show_components :-
	format("Name\tPack\tType\tPath\n", []),
	format("-------\t-------\t-------\t-------\n", []),
	(
	    component_description(Name, Pack, Type, Path),
	    format("~w\t~w\t~w\t~w\n", [Name, Pack, Type, Path]),
	    fail
	;
	    true
	).

autoconfig_components :-
	load_components_sysavail(all),
	load_components_sysavail(user).

load_components_sysavail(SysAvail) :-
	component_setup_dir(SysAvail, SetupDir),
	(
	    file_exists(SetupDir) ->
	    directory_files(SetupDir, Files),
	    component_list(Files, Components),
	    use_modules(Components)
	;
	    true
	).

use_modules([]).
use_modules([M|Ms]) :-
	use_module(M),
	use_modules(Ms).

component_list([], []).
component_list([File|Files], [library(Component)|Components]) :-
	atom_concat(BaseFile,'.pl',File),
	atom_concat('autoconfig/components/', BaseFile, Component),
	!,
	component_list(Files, Components).

component_list([_File|Files], Components) :-
	component_list(Files, Components).
