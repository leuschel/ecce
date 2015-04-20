:- module(persdbtr, [persistent_tr/2,persistent_term_tr/3], []).

persistent_tr((:- persistent(F/A,D)),
	[(:- data(F/A)),
	 '$is_persistent'(F/A,'$persistent_module':D)]).

persistent_tr(persistent_dir(D,Dir),
	[persistent_dir('$persistent_module':D,Dir,default,default)]).
persistent_tr((persistent_dir(D,Dir) :- Body),
	[(persistent_dir('$persistent_module':D,Dir,default,default) :- Body)]).

persistent_tr(persistent_dir(D,Dir,DirPerms,FilePerms),
	[persistent_dir('$persistent_module':D,Dir,DirPerms,FilePerms)]).
persistent_tr((persistent_dir(D,Dir,DirPerms,FilePerms) :- Body),
	[(persistent_dir('$persistent_module':D,Dir,DirPerms,FilePerms) :- Body)]).



% kludge to get the name of the module being compiled for persistent_tr/2.
% This predicate replaces '$persistent_module' set by persistent_tr/2.
persistent_term_tr('$persistent_module', Module, Module).
