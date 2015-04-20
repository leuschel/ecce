:- module(make_rt,[make/1,target/1,make_option/1,
	           verbose_message/1,
	           verbose_message/2,
		   dot_concat/2,
		   call_unknown/1,
		   all_values/2,
		   get_value/2,
		   get_value_def/3,
		   name_value/2,
		   get_name_value/3,
		   get_name_value_string/3,
		   add_name_value/2,
		   find_file/2,
		   find_file/4,
		   vpath/1,
		   add_vpath/1,
		   bold_message/1,
		   bold_message/2,
		   bolder_message/1,
		   bolder_message/2,
%                   fancy_display/1,
%% Not used any more
%%		   dyn_load_cfg_file_into_make/1,
	register_module/1,
   	           dyn_load_cfg_module_into_make/1],['make/make_com',hiord]).

%% ---------------------------------------------------------------------------
:- use_package(assertions).
:- use_package(regtypes).

:- comment(title,"Predicates Available When Using The Make Package").

:- comment(author, "Manuel Hermenegildo").

:- comment(usage,"This module is loaded automatically when the
   @lib{make} library package is used.").

:- comment(module,"This is the run-time module which implements the
   predicates which are provided when using the @lib{make} library
   package in a given application. For example, they are used
   internally by @apl{lpmake}.").

%% Needs fixing the English... MH
:- comment(bug, "The current handle of help messages is defficient.
   It must be in a standar form, and the user of this library only
   must be allowed to add messages, not procedures to print it.").

:- comment(bug, "target_comment/1 does not work, why? :-(.").

%% ---------------------------------------------------------------------------

%% ISO Prolog-like modules
%% :- use_module(library(compiler), [ensure_loaded/1,use_module/1]).
:- use_module(library(compiler), [use_module/1]).

%% CIAO libraries
:- use_module(library(filenames),[file_name_extension/3]).
:- use_module(library(terms),    [atom_concat/2]).
:- use_module(library(read),     [read_term/3]).
:- use_module(library(system),   [file_exists/1,modif_time/2,modif_time0/2]).
:- use_module(library(messages), [simple_message/2,warning_message/2]).
:- use_module(library(format),   [format/3, format_control/1]).
:- use_module(library(lists),    [list_concat/2]).
%% :- use_module(library('assertions/assrt_lib'),[set_libs/2]).


:- data name_value/2.
:- data vpath/1.

register_module(A) :- dyn_load_cfg_module_into_make(A).

add_name_value(Name, Value) :-
	data_facts:assertz_fact( name_value(Name, Value) ).	

add_vpath(Path) :-
	data_facts:assertz_fact(vpath(Path)).

dot_concat(A, A) :-
	A == '',
	!.
dot_concat(A, A) :-
	atom_codes( A , [ 0'. |_]),
	!.
dot_concat(T, TSuffix) :-
	atom_concat('.', T, TSuffix).


:- regtype target(T) # "@var{T} is a Makefile target.".

target(X) :- atm(X).

verb_target_comment(Target) :-
	make_option('-v') ->
	show_target_comment(Target)
 ;
	true.

show_target_comment(Target) :-
	call_unknown(_:target_comment(Target)) -> true
 ;
	(   target_comment(Target, Comment, Args) ->
	    bold_message([0'~,0'w,0':,0' |Comment], [Target|Args])
	;
	    true
	).

:- push_prolog_flag(multi_arity_warnings,off).

bold_message(Mess) :-
	bold_message(Mess,[]).

bold_message(Mess,Args) :-
	format(user_error,
"*** ----------------------------------------------------------------------",
	[]),
        replace_nl_with_stars([0'\n|Mess],NMess),
	format(user_error,NMess,Args),
	format(user_error,"~n",[]),
	format(user_error,
"*** ----------------------------------------------------------------------~n",
	[]).

bolder_message(Mess) :-
	bolder_message(Mess,[]).

bolder_message(Mess,Args) :-
	format(user_error,
"*** ======================================================================",
	[]),
        replace_nl_with_stars([0'\n|Mess],NMess),
	format(user_error,NMess,Args),
	format(user_error,"~n",[]),
	format(user_error,
"*** ======================================================================~n",
	[]).

replace_nl_with_stars([],[]).
replace_nl_with_stars([0'\n|R],[0'\n, 0'*,0'*,0'*,0' |NR]) :-
	!,
	replace_nl_with_stars(R,NR).
replace_nl_with_stars([C|R],[C|NR]) :-
	!,
	replace_nl_with_stars(R,NR).

:- pop_prolog_flag(multi_arity_warnings).


:- pred make(TargetList) : list(target) 

   # "This is the main entry point to the make library. It makes the
      list of targets one by one as well as any intermediate targets
      needed as dictated by the dependency rules.".

make(A) :-
	dependency_list(A, R, [], R, []),
	show_verbose_dependency_list(R),
	make_dep_list(R).

% dependency_list(A, R) :-
% 	dependency_list(A, R, [], R, []).

make_dep_list([]).
make_dep_list([L|Ls]) :-
	make_dep(L),
	make_dep_list(Ls).

make_dep(do_target(Target)) :-
	show_verbose_processing(Target),
	verb_target_comment(Target),
	(
	    do_target(Target) -> true
	;
	    throw(make_error("fail when making target ~w", [Target]))
	).
make_dep(do_dependency(Target, TSuffix, SSuffix, FileBase)) :-
	do_show_dependency_comment(TSuffix, SSuffix, FileBase),
	(
	    do_dependency(TSuffix, SSuffix, FileBase) -> true
	;
	    throw(make_error("fail when making dependency ~w <- ~w~w",
	        [Target, FileBase, SSuffix]))
	).

do_show_dependency_comment(TSuffix, SSuffix, FileBase) :-
	(
	    call_unknown(_:dependency_comment(SSuffix,TSuffix,FileBase)) ->
	    true
	;
	    show_dependency_comment(SSuffix,TSuffix,FileBase)
	).

% ----------------------------------------------------------------------------

is_member(_,L) :-
	var(L),
	!,
	fail.
is_member(X,[Y|_]) :-
	X=Y,
	!.
is_member(X,[_|L]) :-
	is_member(X, L).

dependency_list([], _R0, _Postconditions, R, R) :-
	!.
dependency_list([Target|Targets], R0, Postconditions, R1, R) :-
	!,
	dependency_list(Target, R0, Postconditions, R1, R2),
	dependency_list(Targets, R0, Postconditions, R2, R).
dependency_list(Target, R0, Postconditions, R1, R) :-
	is_member(Target, Postconditions) ->
	show_warning_circular_reference(Target, Postconditions),
	R1 = R
 ;
	target_exists(Target) ->
	dependency_target(Target, R0, Postconditions, R1, R)
 ;
	file_name_extension(Target, FileBase, TSuffix),
	dependency_exists(TSuffix, SSuffix) ->
	dependency_suffix(Target, FileBase, TSuffix, SSuffix, R0, Postconditions, R1, R)
 ;
	find_file(Target, PathTarget) ->
	show_verbose_unconditional_target_exists(PathTarget),
	R1 = R
 ;
	throw(make_error("Could not complete ~w.  Verify that the elements in ~w "||
			"exists in a known path or that they are valid options.",
		        [Target, [Target|Postconditions]])).

dependency_target(Target, R0, Postconditions, R1, R) :-
	E = do_target(Target),
	(
	    is_member(E, R0) ->
	    show_verbose_ignoring_already_added_target(Target),
	    R1 = R
	;
	    (
		target_deps(Target, Deps) ->
		show_verbose_conditional_target(Target, Deps),
		dependency_list(Deps, R0, [Target|Postconditions], R1, R2)
	    ;
		show_verbose_unconditional_target(Target),
		R1 = R2
	    ),
	    insert_dependency_element(Target, Deps, E, R0, R2, R)
	).

dependency_suffix(Target, FileBase, TSuffix, SSuffix, R0, Postconditions, R1, R) :-
	show_verbose_checking(FileBase, TSuffix, SSuffix),
	atom_concat(FileBase, SSuffix, Dep),
	(
	    dependency_precond(TSuffix, SSuffix, Deps0) ->
	    Deps = [Dep,Deps0]
	;
	    Deps = Dep
	),
	E = do_dependency(Target, TSuffix, SSuffix, FileBase),
	(
	    is_member(E, R0) ->
	    show_verbose_ignoring_already_added_dependency(FileBase, TSuffix, SSuffix),
	    R1 = R
	;
	    dependency_list(Deps, R0, [Target|Postconditions], R1, R2),
	    show_verbose_dependency(FileBase, TSuffix, SSuffix),
	    insert_dependency_element(Target, Deps, E, R0, R2, R)
	).

already_added([], _) :-
	!.
already_added([Dep|Deps], R) :-
	!,
	already_added(Dep, R),
	already_added(Deps, R).

already_added(Dep, R) :-
	is_member(do_target(Dep), R),
	!.
already_added(Dep, R) :-
	is_member(do_dependency(Dep, _, _, _), R).

insert_dependency_element(Target, Deps, E, R0, R1, R) :-
	already_added(Deps, R0) ->
	R1 = [E|R]
 ;
	find_file(Target, PathTarget),
	newer(Deps, PathTarget) ->
	show_verbose_up_to_date(Target),
	R1 = R
 ;
	R1 = [E|R].

show_verbose_dependency_list(R) :-
	verbose_message("Dependency list is ~w", [R]).
show_verbose_ignoring_already_added_target(Target) :-
	verbose_message("Ignoring already added target ~w",[Target]).
show_verbose_unconditional_target(Target) :-
	verbose_message("Making unconditional target ~w",[Target]).
show_verbose_conditional_target(Target, Deps) :-
	verbose_message("Adding conditional target ~w < ~w", [Target,Deps]).
show_verbose_unconditional_target_exists(PathTarget) :-
	verbose_message("Unconditional target ~w exists",[PathTarget]).
show_warning_circular_reference(Target, Postconditions) :-
	warning_message("Ignoring circular reference ~w -> ~w",[Target, Postconditions]).
show_verbose_up_to_date(Target) :-
	verbose_message("~w is up to date",[Target]).
show_verbose_processing(Target) :-
	verbose_message("Processing ~w",[Target]).
show_verbose_dependency(FileBase, TSuffix, SSuffix) :-
	verbose_message("Found that ~w~w can be generated from ~w~w",
	    [FileBase, TSuffix, FileBase, SSuffix]).
show_verbose_checking(FileBase, TSuffix, SSuffix) :-
	verbose_message("Checking if ~w~w should be generated from ~w~w",
	    [FileBase, TSuffix, FileBase, SSuffix]).
%% This is what make calls for dependency targets
show_dependency_comment(SSuffix,TSuffix,FileBase) :- 
	verbose_message("Generating ~w~w from ~w~w",
	    [FileBase,TSuffix,FileBase,SSuffix]).
show_verbose_ignoring_already_added_dependency(FileBase, TSuffix, SSuffix) :-
	verbose_message("Ignoring already added dependency ~w~w from ~w~w",
	    [FileBase,TSuffix,FileBase,SSuffix]).

%% ---------------------------------------------------------------------------
%% Procesing target dependencies
%% ---------------------------------------------------------------------------

compose_handler( T , make_error( Text , [ Args ] ) ) :-
	( list( Args ) -> NArgs = [T|Args] ; NArgs = [T,Args] ),
	throw( make_error( Text, [ NArgs ] ) ).
compose_handler( _T , E ) :-
	throw( E ).

newer([],_Target) :- !.
newer([File|Files],Target) :-
	!,
	newer(File,Target),
	newer(Files,Target).
newer(File,Target) :-
	verbose_message("Checking if ~w is newer",[File]),	
	find_file(File,PathFile),
	needs_processing(Target,PathFile).

%% Assumes that source file exists
needs_processing(Target,Source) :-
	\+ (
	    modif_time(Source, SourceTime),
	    modif_time0(Target, TargetTime),
	    SourceTime > TargetTime
	   ).


%% ---------------------------------------------------------------------------
%% Procesing suffix dependencies
%% ---------------------------------------------------------------------------

find_file(File,File) :-
	file_exists(File).
find_file(File,PathFile) :-
	vpath(Path),
	atom_concat([Path,'/',File],PathFile),
	file_exists(PathFile).

:- meta_predicate find_file(?, pred(1), ?, ?).

find_file(File, _VPath,   '',     File) :-
	file_exists(File).
find_file(File,  VPath, Path, PathFile) :-
	VPath(Path),
	atom_concat([Path,'/',File],PathFile),
	file_exists(PathFile).

%% ---------------------------------------------------------------------------
%% Support code
%% ---------------------------------------------------------------------------

:- pred dyn_load_cfg_module_into_make(ConfigFile) : sourcename 

   # "Used to load dynamically a module (typically, a @file{Makefile})
      into the make library from the application using the library.".

dyn_load_cfg_module_into_make(ConfigFile) :-
	use_module(ConfigFile).

:- pred make_option(Option) : atm 

   # "Asserting/retracting facts of this predicate sets/clears library 
      options. Default is no options (i.e., the predicate is undefined). The 
      following values are supported:
@begin{verbatim}
make_option('-v'). % Verbose: prints progress messages (useful 
                   % for debugging rules).
@end{verbatim}
  ".

:- data make_option/1.

%% Default is silent. Typically asserted by 
%% make_option('-v').

:- pred verbose_message(Text,ArgList) : format_control * list 

   # "The text provided in @var{Text} is printed as a message, using
     the arguments in @var{ArgList}, if @tt{make_option('-v')} is
     defined. Otherwise nothing is printed.".

:- push_prolog_flag(multi_arity_warnings, off).

verbose_message(Text) :-
	verbose_message(Text,[]).

verbose_message(Mess,Args) :-
	(   make_option('-v') ->
% 	(   call_unknown(make_option('-v')) ->
	    simple_message(Mess,Args)
	;
	    true
	).

:- pop_prolog_flag(multi_arity_warnings).

call_nofail(G) :-
	call(G),
	!.
call_nofail(_G).

% :- meta_predicate call_unknown_nofail(goal).
% call_unknown_nofail(G) :-
% 	call_unknown(G),
% 	!.
% call_unknown_nofail(_G).


%% :- meta_predicate call_unknown(goal).

:- use_package(library(hiord)).
:- use_module(library(terms)).
:- use_module(library(aggregates)).

call_unknown(G) :-
	prolog_flag(unknown,Old,fail),
	prolog_flag(quiet,QOld,error),
	(
	    call(G),
	    prolog_flag(unknown,_,Old),
	    prolog_flag(quiet,_,QOld)
	;   prolog_flag(unknown,_,Old),
	    prolog_flag(quiet,_,QOld),
	    fail
	).

all_pred_values(Name,Values) :-
	findall(Value,
	(
	    Pred =.. [Name, Value],
	    call_unknown(_:Pred)
	),
	Values).

all_name_values(Name, Values) :-
	findall(Value, name_value(Name, Value), Values).

% read all values
all_values(Name, Values) :-
	all_name_values(Name, Values0),
	(
	    Values0 == [] ->
	    all_pred_values(Name, Values)
	;
	    Values = Values0
	).

get_name_value(NameValue, Name, Value) :-
	get_name_value_string(NameValue, Name, ValueS),
	atom_codes(Value,ValueS).

get_name_value_string(NameValue, Name, ValueS) :-
	atom_codes(NameValue, NameValueS),
	list_concat([NameS, "=", ValueS], NameValueS),!,
	atom_codes(Name,NameS).

get_value(Name, Value) :-
	name_value(Name, _) ->
	name_value(Name, Value)
 ;
	get_pred_value(Name, Value).

get_pred_value(Name, Value) :-
	Pred =.. [Name, Value],
	call_unknown(_:Pred).

get_value_def(Name, DefValue, Value) :-
	name_value(Name, Value) -> true
 ;
	get_pred_value_def(Name, DefValue, Value).

get_pred_value_def(Name, _DefValue, Value) :-
	Pred =.. [Name, Value],
	catch(call_unknown(_:Pred), _Error, false),
	!.
get_pred_value_def(_PredName, DefValue, DefValue).

