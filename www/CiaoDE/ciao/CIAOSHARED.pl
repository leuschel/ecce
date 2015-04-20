:- module(_,_,[make, functions, assertions, hiord]).
:- use_module(library(filenames),[no_path_file_name/2,basename/2]).
%:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(lists)).
:- use_module(library('make/system_extra')).
:- use_module(library('make/make_rt')).
:- use_module(library(distutils)).
:- use_module(library(persvalue)).
:- use_module(library(messages)).
:- use_module(ciaosrc('CIAOSETTINGS'),[config_entry/3, settings_set_value/1]).
:- use_module(ciaodesrc('CIAODESHARED')).

:- use_module(lpdocsrc('LPDOCSHARED'), [lpdocsrc/1]).

% Be carefull with get_vers and get_patch: when uninstall, the patch
% version may differ with the version that we try to uninstall.

:- initialization(ciaoinitvals).

% ----------------------------------------------------------------------------
% Utilities that are used only with SETTINGS and SHARED files:
% ----------------------------------------------------------------------------

configlevel('1', default).
configlevel('2', minimum).
configlevel('3', extended).

autoconfig :-
	name_value('AUTOCONFIG','1').


settings_auto := ~atom_concat(~ciaosrc, '/SETTINGS_AUTO').

ciaoinitvals :-
	load_file(ciaosrcsettings, ~settings_auto).

menuconfig :-
	( autoconfig -> note_message("Doing automatic configuration.") ; true ),
	config_values,
	save_group(ciaosrcsettings),
	copy_file(~settings_auto, ~atom_concat(~ciaosrc, '/SETTINGS'), [overwrite]),
	display_string("\nOptions saved in file 'ciao/SETTINGS_AUTO'.\n\n"),
	gmake([_Dir, GMake]),
	bolder_message(
          "Ciao compilation and installation configuration completed.\n\n" || 
          "Please check the messages and paths above. If all is OK\n" || 
          "run \"~w all\" to compile and \"~w install\" to install the\n"||
	  "system, else you can change any value running the\n"||
	  "configure utility again with the --menu option.",
	            [GMake, GMake]),
	!.

verify_deps([], _Group).
verify_deps([(Name, Value)|Deps], Group) :-
	current_value(Group, Name, Value),
	verify_deps(Deps, Group).

config_values :-
	config_entry(Group, Name, Properties),
	config_value(Group, Name, Properties),
	fail
 ;      true.

config_value(Group, Name, Properties) :-
	(   member(depend_of(DepList),Properties) ->
	    verify_deps(DepList, Group)
	;   true
	),
	(   member(set_value(Value), Properties) ->
	    true
	;   member(set_value(ValMethod, Value), Properties),
	    settings_set_value(ValMethod) ->
	    true
	;
	    ( member(valid_values(ValidValues), Properties) -> true ; true ),
	    (   name_value(Name, Value) ->
		(
		    member(Value, ValidValues) ->
		    display_list(['{NOTE: Predefined ',Name,'=[',Value,']}\n'])
		;
		    display_list(['{ERROR: Invalid value [',Value,'] for ',
		    Name,'. Valid values are ', ValidValues,
		    '. Correct the problem and try again}\n']),
		    halt(1)
		)
	    ;   (   (\+ member(noprevious, Properties)),
		    current_value(Group, Name, Value0) -> true
		;   member(default(Value0), Properties) -> true
		;   member(default(DefMethod, Value0), Properties) ->
		    settings_set_value(DefMethod)
		;   true
		),
		(   member(query(Help, ConfigLevels), Properties),
		    (   current_value(Group, 'CONFIGLEVEL', NConfigLevel) ->
			configlevel(NConfigLevel, ConfigLevel)
		    ;   ConfigLevel = default
		    ),
		    \+ (autoconfig),
		    member(ConfigLevel, ConfigLevels) ->
		    query_value(Help, Name, ValidValues, Value0, Value)
		;
		    Value = Value0
		)
	    )
	),
	(   member(show(ShowHelp, ShowConfigLevels), Properties),
	    (   current_value(Group, 'CONFIGLEVEL', NShowConfigLevel) ->
		configlevel(NShowConfigLevel, ShowConfigLevel)
% 	    ;
% 		fail
	    ),
	    member(ShowConfigLevel, ShowConfigLevels) ->
	    (
		pretty_display(ShowHelp, Name, Value)
	    )
	;
	    true
	),
	(
	    member(nosave,Properties) ->
	    true
	;   ground(Value) ->
	    set_value(Group, Name, Value)
	;
	    atom_codes(Name, NameS),
	    warning_message("Undefined value for "||NameS)
	),
	!.

repeated_display(_Term, 0) :- !.
repeated_display(Term, Times) :-
	Times > 0,
	Times2 is Times - 1,
	display(Term),
	repeated_display(Term, Times2).

pretty_display(ShowHelp, Name, Value) :-
	( ShowHelp == "" -> atom_codes(Name, ShowName) ; ShowName = ShowHelp ),
	length(ShowName, N),
	N2 is 30 - N,
	( N2 < 0 -> N3 is 0 ; N3 is N2 ),
	display_string(ShowName),
	display(' : '),
	repeated_display('.', N3),
	display_list([' ', Value, '\n']).

query_value(Help, Name, ValidValues, Value0, Value) :-
	query_value_(Help, Name, Value0, Value),
	member(Value, ValidValues) ->
	true
    ;
	warning_message("Value not allowed for this menu option, try again\n"),
	display_list(['Valid values are ', ValidValues, '\n\n']),
	query_value(Help, Name, ValidValues, Value0, Value).

query_value_(Help, Name, DefaultValue, Value) :-
	nl,
	display_string(Help),nl,
	display_list(['\n', Name, '=[', DefaultValue, '] ? ']),
	get_atom(Value1),
	(   Value1 = '' ->
	    Value = DefaultValue
	;
	    Value = Value1
	).

% ----------------------------------------------------------------------------
% Settings which are common to makefiles below
% but which you should not need to change
%-----------------------------------------------------------------------------

:- reexport(ciaodesrc('CIAODESETTINGS'), [docformat/1]).

staticcompname := 'ciaoc'.

% Define this to be the permissions for installed execs/dirs and data files:
execmode(Perm) :-
	number_codes(Number, 8, ~atom_codes(~current_value(ciaosrcsettings,
	    'EXECMODE'))),
	convert_permissions(Perm, Number).
datamode(Perm) :-
	number_codes(Number, 8, ~atom_codes(~current_value(ciaosrcsettings,
	    'DATAMODE'))),
	convert_permissions(Perm, Number).

% Define this to be the main file name (with suffix if there is one???).
mainname := 'ciao'.
% Headers for the executable at startup
header := 'Ciao'.
copyrt := 'UPM-CLIP'.
actualdir := ~no_path_file_name(~ciaosrc).
codes_atom(A, B) :- atom_codes(B, A).

basemain    := ~no_path_file_name(~mainname).
vers        := ~vers(~basemain).
patch       := ~patch(~basemain).
versionmain := ~versionmain(~basemain).
vpmain      := ~vpmain(~basemain).
win32vpmain := ~win32vpmain(~basemain).

% The Ciao version used
prolog  := ~versionmain.
home := ~codes_atom(~getenvstr('HOME')).
srcbindir := ~atom_concat(~ciaodesrc, '/bin').
ciaoc     := ~atom_concat([~srcbindir,  '/ciaoc-',  ~vers, ~get_ciao_ext]).
ciaosh    := ~atom_concat([~ciaobindir, '/ciaosh-', ~vers, ~get_ciao_ext]).
lpdoclib := ~atom_concat([~lpdocsrc,'/lib']).
rpm_build_root(R) :-
	(   current_env('RPM_BUILD_ROOT', S) -> true
	;   S = ''
	),
	S=R.

% ============================================================================
% Constants defined using the configuration values
% ============================================================================

settings_value(Name) :=	~current_value(ciaosrcsettings, Name).

:- comment(bug, "gmake must be replaced by lpmake.").

gmake := [~settings_value('MAKEDIR'),
	  ~settings_value('MAKENAME')].

:- comment(bug, "Relation between rpm_build_root/1 and docdir/1 must
   be checked").

docdir := ~atom_concat(~rpm_build_root, ~settings_value('DOCDIR')).

%lpdoclib := ~atom_concat([~settings_value('LIBROOT'),'/', 'lpdoc-2.0']).
install_prolog_name := ~settings_value('INSTALL_PROLOG_NAME').
compress_libs := ~settings_value('COMPRESS_LIBS').

lpmake([~atom_concat(~srcbindir,'/'), 'lpmake.sta']).
lpdoc2([~atom_concat(~srcbindir,'/'), 'lpdoc']).

libdir           := ~settings_value('LIBDIR').
ciaolibroot      := ~settings_value('LIBROOT').
ciaobinroot      := ~settings_value('BINROOT').
ciaobindir       := ~settings_value('BINDIR').
ciaodocdir       := ~settings_value('DOCDIR').
ciaodocroot      := ~settings_value('DOCROOT').

instype          := ~settings_value('INSTYPE').
defaulttype      := ~settings_value('DEFAULTTYPE').
reallibdir       := ~settings_value('REALLIBDIR').
install_emacs_support
                 := ~settings_value('INSTALL_EMACS_SUPPORT').

update_bashrc    := ~settings_value('UPDATE_BASHRC').
update_cshrc     := ~settings_value('UPDATE_CSHRC').
update_emacs     := ~settings_value('UPDATE_EMACS').
update_xemacs    := ~settings_value('UPDATE_XEMACS').
dotbashrc        := ~settings_value('DOTBASHRC').
dotcshrc         := ~settings_value('DOTCSHRC').
dotemacs         := ~settings_value('DOTEMACS').
dotxemacs        := ~settings_value('DOTXEMACS').
webimagespath    := ~settings_value('WEB_IMAGES_PATH').
with_mysql       := ~settings_value('WITH_MYSQL').
mysql_client_directory
	         := ~settings_value('MYSQL_CLIENT_DIRECTORY').
with_java_interface
                 := ~settings_value('WITH_JAVA_INTERFACE').
includeroot      := ~settings_value('INCLUDEROOT').

buildreallibdir  := ~atom_concat(~rpm_build_root, ~reallibdir).
buildlibdir      := ~atom_concat(~rpm_build_root, ~libdir).
installedincludedir
	         := ~atom_concat([~reallibdir, '/include/',~get_os,
		     ~get_arch]).
use_ppl          := ~settings_value('USE_PPL').
ppl_interface    := ~settings_value('PPL_INTERFACE').
bibfiles         := ~settings_value('BIBFILES').
bibfile          := ~atomdelim(~bibfiles,',').
install_log      := ~atom_concat(~ciaodesrc,'/install.log').
compile_ciaoppcl := ~settings_value('COMPILE_CIAOPPCL').

:- multifile component_description/4.

ciaosrc          := ~component_description(ciao, _, _).
objdir           := ~atom_concat([~ciaosrc,'/bin/',~get_os, ~get_arch,
	~get_debug]).
setlocalciao     := ~atom_concat(['CIAOALIASPATH= CIAOLIB=', ~ciaosrc,
	' CIAOENGINE=', ~objdir,'/ciaoengine',~get_exec_ext]).

