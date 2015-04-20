:- module(autoconfig_base, _, []).

component_setup_dir(all, SetupDir) :-
	absolute_file_name(library(autoconfig), File),
	atom_concat(RootDir, 'autoconfig.pl', File),
	atom_concat(RootDir, '/components/', SetupDir).
component_setup_dir(user, '~/.ciao.d/components/').

