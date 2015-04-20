:- module('CIAOSETTINGS',_,[functions,assertions,iso]).
:- use_module(library('make/make_rt'),[find_file/4]).
:- use_module(library('make/system_extra'),[do/2, readf/2, writef_list/2,
	convert_permissions/2]).

:- use_module(library(system),
	[current_env/2,
	 getenvstr/2,
	 copy_file/3,
	 extract_paths/2,
	 file_exists/1,
	 delete_file/1]).
:- use_module(library(persvalue)).
:- use_module(library(distutils)).
%:- use_module(library(terms)).
:- use_module(library(lists)).
:- use_module(library(messages)).
:- use_module(ciaosrc('CIAOSHARED')).
:- use_module(ciaodesrc('CIAODESHARED'), [ciaodesrc/1]).

:- reexport(ciaosrc('CIAOSHARED')).

:- comment(module, "This module contains definitions for all
   customizable CIAO options.").

settings_set_value(ValMethod) :-
	call(ValMethod).

% ============================================================================
% Predicates for detecting several default configuration values:
% ============================================================================

% def_sys_avail(all)  :- isadmin,!.
% def_sys_avail(user).

def_sys_avail(all).

get_prefix(all,  ins, '/usr/local').
get_prefix(all,  src, ~ciaodesrc).
get_prefix(user, ins, ~get_home).
get_prefix(user, src, ~ciaodesrc).

get_home(H) :-
	usersrc(UserSrc) ->
	absolute_dir_name(~atom_concat('~', UserSrc), H)
    ;
	absolute_dir_name('~', H).

get_paths(APath) :-
	getenvstr('PATH', Path),
	extract_paths(Path, PathList),
	member(SPath, PathList),
	atom_codes(APath, SPath).

get_bashrc(all, F) :-
        member(F, ['/etc/bash.bashrc', '/etc/bashrc']),
        file_exists(F) -> true
    ;
	F = '/etc/bashrc'.
get_bashrc(user) := ~atom_concat(~get_home, '/.bashrc').

get_cshrc(all, F) :-
	member(F, ['/etc/csh.cshrc', '/etc/tcsh.tcshrc']),
	file_exists(F) -> true
    ;
	F = '/etc/csh.cshrc'.
get_cshrc(user)   := ~get_cshrc_name.

get_cshrc_name(C) :-
	get_home(H),
	(
	    member(F, ['/.tcshrc', '/.cshrc']),
	    atom_concat(H, F, C),
	    file_exists(C) -> true
	;
	    atom_concat(H, '/.cshrc', C)
	).

get_emacs(user)   := ~atom_concat(~get_home, '/.emacs').
get_xemacs(user)   := ~atom_concat(~get_home, '/.xemacs/init.el').

get_docroot(all)  := '/var/www/html/ciao'.
get_docroot(user) := ~atom_concat(~get_home, '/public_html/CiaoDE').

get_web_images_path(all)  := '/var/www/html/images/'.
get_web_images_path(user) := ~atom_concat(~get_home, '/public_html/images/').

get_web_images_url(all)  := ~get_all_url.
get_web_images_url(user) := ~get_user_url.

get_all_url := '/images/'.

get_user_url(Url) :-
	atom_concat(['/~', ~get_user, '/images/'], Url) -> true
    ;
	get_all_url(Url).

get_user(User) :-
	(
	    current_env('USER', User)
	;
	    current_env('USERNAME', User)
	),
	!.

verify_emacs(yes) :-
	emacs_installed,
	!,
	pretty_display("_Emacs is present_",_,yes).
verify_emacs(no) :-
	pretty_display("_Emacs is present_",_,no).

verify_xemacs(yes) :-
	xemacs_installed,
	!,
	pretty_display("_XEmacs is present_",_,yes).
verify_xemacs(no) :-
	pretty_display("_XEmacs is present_",_,no).

install_emacs_or_xemacs_support(yes, _,   yes) :- !.
install_emacs_or_xemacs_support(_,   yes, yes) :- !.
install_emacs_or_xemacs_support(_,   _,   no)  :-
	pretty_display("_Emacs or XEmacs are present_",_,no),
	warning_verify_emacs('Emacs or XEmacs are').

warning_verify_emacs(App) :-
	warning_message(
	    "~w not installed.  Is is hightly recommended that\n"||
	    "you stop the Ciao configuration and install it first."|| 
            "It required for the graphical development environment.",
	    [App]).

possible_emacs_site_start :=
	'/usr/share/emacs/site-lisp/site-start.d' |
	'/etc/emacs/site-start.d'.

emacs_site_start(SiteStart) :-
	possible_emacs_site_start(SiteStart),
	file_exists(SiteStart),
	!.

possible_xemacs_site_start :=
	'/usr/share/xemacs/site-packages/lisp/site-start.d'.

xemacs_site_start(SiteStart) :-
	possible_xemacs_site_start(SiteStart),
	file_exists(SiteStart),
	!.

update_emacs(SysAvail, InsType, VerifyEmacs, no) :-
	SysAvail    == 'all',
	InsType     == 'ins',
	VerifyEmacs == 'yes',
	emacs_site_start(_),
	!.
update_emacs(_,       _InsType, no,          no) :-
	!.
update_emacs(_,        _,        _,           yes).

update_xemacs(user, _, yes, yes).
update_xemacs(all,  _, _,   no).

get_ciao_mode_init_dir('all', 'ins', 'yes', _RealLibDir, Value) :-
	emacs_site_start(Value),
	!.
get_ciao_mode_init_dir(_,     _,     _,      RealLibDir, RealLibDir).

get_xemacs_init_dir('all',  'ins', 'yes', _          , Value) :-
	xemacs_site_start(Value),
	!.
get_xemacs_init_dir(_,     _,     _,       RealLibDir, RealLibDir).

verify_mysql(yes) :-
	mysql_installed,
	pretty_display("_Mysql is present_",_,yes),
	!.
verify_mysql(no) :-
	pretty_display("_Mysql is present_",_,no),
	warning($$(
	    "MySQL has not been detected.  If you would like to use the\n"||
	    "Ciao-MySQL interface it is highly recommended that you stop\n"||
	    "the Ciao configuration now and install MySQL first.")).

verify_java(yes) :-
	(   javac_installed ->
	    pretty_display("_Javac is present_",_,yes)
	;
	    pretty_display("_Javac is present_",_,no),
	    fail
	),
	(   javadoc_installed ->
	    pretty_display("_javadoc is present_",_,yes)
	;
	    pretty_display("_javadoc is present_",_,no),
	    fail
	),
	!.
verify_java(no) :-
	warning($$(
	 "JAVA has not been detected. If you would like to use the\n"||
         "utilities for the Java interface it is highly recommended that\n"||
	 "you stop the Ciao configuration now and install Java first.")).

% In the future, the detection of ppl must be implemented here.

verify_ppl(no).

emacs_base  := emacs.
xemacs_base := xemacs.

add_exec_ext(App) := ~atom_concat(App,~get_exec_ext).

emacs_app   := ~add_exec_ext(~emacs_base).
xemacs_app  := ~add_exec_ext(~xemacs_base).
mysql_app   := ~add_exec_ext(mysql).
mysql_app   := ~add_exec_ext(mysql).
javac_app   := ~add_exec_ext(javac).
javadoc_app := ~add_exec_ext(javadoc).

emacs_installed  :- find_file(~emacs_app,  get_paths, _, _).
emacs_installed  :- find_file(~emacs_base, get_paths, _, _).

xemacs_installed :- find_file(~xemacs_app, get_paths, _, _), !.
xemacs_installed :- find_file(xemacs_base, get_paths, _, _).

mysql_installed :-  find_file( ~mysql_app, get_paths, _, _).

javac_installed :-  find_file( ~javac_app, get_paths, _, _).

javadoc_installed :-
	find_file(~javadoc_app, get_paths, _, _).

do_str(Command, Fail, String) :-
	do(~append(Command, [' > ciaostr.tmp']), Fail),
	file_exists('ciaostr.tmp') ->
	readf('ciaostr.tmp', String),
        delete_file('ciaostr.tmp').
get_mysql_dir(MySQL) :-
	do_str(['locate libmysqlclient.a'], nofail, S),
	append(MySQLS,"/libmysqlclient.a\n",S),
	atom_codes(MySQL, MySQLS),
	!.
get_mysql_dir('/usr/lib/mysql') :-
	warning_message(
            "Unable to determine where the MySQL client library is " ||
	    "installed.\nCurrent value (/usr/lib/mysql) is only a guess.").

get_installgroup(InstallGroup) :-
	do_str(['groups | cut -f 1 -d \' \''], nofail, S),
	append(InstallGroupS, "\n", S),
	atom_codes(InstallGroup, InstallGroupS).

config_entry(ciaosrcsettings, Name, HowToGetValue) :-
	ciao_config_entry(Name, HowToGetValue).

makeop := gmake | make.

get_make(MakeDir, MakeName) :-
	makeop(MakeName),
	find_file(MakeName, get_paths, Path, _) ->
	atom_concat(Path, '/', MakeDir)
 ;
	error_message("Unable to determine the make utility used."),
	fail.

get_libdir('src', LibRoot, LibRoot).
get_libdir('ins', LibRoot, Value) :-
	atom_concat(LibRoot,'/ciao',Value).

get_reallibdir('src', _LibDir, Value) :-
	ciaosrc(Value).
get_reallibdir('ins',  LibDir, Value) :-
	atom_concat([LibDir, '/', ~versionmain], Value).

get_enginedir('src', RealLibDir, Value) :-
	atom_concat(RealLibDir, '/bin', Value).
get_enginedir('ins', RealLibDir, Value) :-
	atom_concat(RealLibDir, '/engine', Value).

get_libroot('src', Prefix, Value) :-
	atom_concat(Prefix, '/ciao',Value).
get_libroot('ins', Prefix, Value) :-
	atom_concat(Prefix, '/lib', Value).

get_includeroot('src', Prefix, Value) :-
	atom_concat(Prefix, '/ciao/include', Value).
get_includeroot('ins', Prefix, Value) :-
	atom_concat(Prefix, '/include', Value).

get_ppl_interface('/usr/local/src/ppl-0.7/interfaces/Prolog/Ciao/ppl_ciao.pl').

% ----------------------------------------------------------------------------
% Facts defining the menu behavior.
%
% The order in the following clauses is the order of the menu.
% ciao_config_entry(Name, HowToGetValue).
% ----------------------------------------------------------------------------

ciao_config_entry('CIAODESRC',   [set_value(~ciaodesrc)]).
ciao_config_entry('MAIN',        [set_value(~mainname)]).
% ciao_config_entry('ABSSRC',      [set_value(~absolute_dir_name('.'))]).
ciao_config_entry('CONFIGLEVEL',
	[
	    noprevious,
	    default('1'),
	    valid_values(['1','2','3']),
	    query(
    "\nWelcome to the CiaoDE compilation and installation configuration.\n"||
    "You will now be asked some installation questions.\n"||
    "Defaults are given in square brackets. \n"||
    "Just hit [Enter] to accept the default values.\n\n"||

    "Please select the configuration level you prefer:\n\n"||
    "        1 --  Use default options.\n"||
    "        2 --  Ask for minimum options (recommended).\n"||
    "        3 --  Ask for extended options.",
		[default,minimum,extended])
	]).
ciao_config_entry('SYSAVAIL',
	[
	    default(~def_sys_avail),
	    valid_values(['all', 'user']),
	    query(
    "Select the system availability:\n\n"||
    "        all  --  Make the system available for all users.\n"||
    "                 Typically you will need to complete the\n"||
    "                 installation as root.\n"||
    "        user --  Make the system available only for the current user\n"||
    "                 (configure it in the user's home directory).\n",
	[minimum, extended]),
	    show("System availability is", [default])
	]).
ciao_config_entry('INSTYPE',
	[
	    default('ins'),
	    valid_values(['ins','src']),
	    query(
    "Select the type of installation:\n\n"||
    "        ins  --  Install the system and set up things to use the \n"||
    "                 installed version. The system will not require the\n"||
    "                  sources to run (and they can be erased).\n"||
    "        src  --  The system will be compiled in, and run from the \n"||
    "                 sources (typically for developers).",
	[minimum, extended]),
	    show("Installation type is", [default])
	]).
ciao_config_entry('STOP_IF_ERROR',
	[
	    default(yes),
	    valid_values(['yes','no']),
	    query(
    "Stop installation if an error is found.",
        [extended]),
	    show("Stop if error", [default])
	]).
ciao_config_entry('PREFIX', 
	[
	    depend_of([('INSTYPE', InsType), ('SYSAVAIL', SysAvail)]),
	    set_value((InsType=='src', ciaodesrc(Value)), Value),
	    default(get_prefix(SysAvail, InsType, DefValue), DefValue),
	    query("In what directory will the installation be performed?",
	        [minimum, extended]),
	    show("Prefix is", [default])
	]).
ciao_config_entry('BINROOT',
	[
	    depend_of([('PREFIX', Prefix)]),
	    set_value(atom_concat(Prefix, '/bin', Value), Value),
	    show("Executables will go here", [default, minimum, extended])
	]).
ciao_config_entry('LIBROOT',
	[
	    depend_of([('PREFIX', Prefix), ('INSTYPE', InsType)]),
	    set_value(get_libroot(InsType, Prefix, Value), Value),
	    show("Libraries will go here", [default, minimum, extended])
	]).
ciao_config_entry('INCLUDEROOT',
	[
	    depend_of([('PREFIX', Prefix), ('INSTYPE', InsType)]),
	    set_value(get_includeroot(InsType, Prefix, Value), Value),
	    show("Headers will go here", [default, minimum, extended])
	]).
ciao_config_entry('UPDATE_BASHRC',
	[
	    default('yes'),
	    valid_values(['yes','no']),
	    query("Set to \"no\" if you do not wish to configure bash to "||
		"work with Ciao.", [minimum,extended])
	]).
ciao_config_entry('DOTBASHRC',
	[
	    depend_of([('UPDATE_BASHRC', yes), ('SYSAVAIL', SysAvail)]),
	    default(get_bashrc(SysAvail, DefValue), DefValue),
	    query(
    "The bash initialization file where the Ciao variables are set.",
	      [minimum, extended]),
	    show("Bash initialization file", [default])
	]).
ciao_config_entry('UPDATE_CSHRC',
	[
	    default('yes'),
	    valid_values(['yes', 'no']),
	    query(
                "Set to \"no\" if you do not wish to configure csh/tcsh to "||
		"work with Ciao.", [minimum,extended])
	]).
ciao_config_entry('DOTCSHRC',
	[
	    depend_of([('UPDATE_CSHRC', yes), ('SYSAVAIL', SysAvail)]),
	    default(get_cshrc(SysAvail, DefValue), DefValue),
	    query(
    "The csh/tcsh initialization file where the Ciao variables are set.\n"||
    "Note that on some systems tcsh reads \"~/.tcshrc\".", 
            [minimum, extended]),
	    show("Csh/Tcsh initialization file", [default])
	]).
ciao_config_entry('VERIFY_EMACS',
	[
	    set_value(~verify_emacs)
	]).
ciao_config_entry('VERIFY_XEMACS',
	[
	    set_value(~verify_xemacs)
	]).
ciao_config_entry('INSTALL_EMACS_SUPPORT',
	[
	    depend_of([('VERIFY_EMACS',VerifyEmacs),
		('VERIFY_XEMACS', VerifyXEmacs)]),
	    valid_values(['yes', 'no']),
	    default(install_emacs_or_xemacs_support(VerifyEmacs, VerifyXEmacs,
	        Value), Value),
	    query(
    "Set to \"yes\" if you wish to install the Ciao emacs libraries\n"||
    "which implement the graphical environment (highly recommended).  It \n"||
    "should be set to no if emacs is not installed in the system.  It is \n"||
    "safe to leave as \"yes\" otherwise.", [extended]),
	    show("Install graph. env. support", [default, minimum])
	]).
ciao_config_entry('UPDATE_EMACS',
	[
	    depend_of([('INSTALL_EMACS_SUPPORT',yes),
		('SYSAVAIL', SysAvail),('INSTYPE',InsType),
		('VERIFY_EMACS',yes)]),
	    set_value(~update_emacs(SysAvail, InsType, yes)),
	    default('yes'),
	    valid_values(['yes', 'no']),
	    query("Set to \"yes\" if you wish to configure emacs to\n"||
 		  "work with Ciao (modify ~/.emacs).", [minimum, extended]),
	    show("Modify ~/.emacs file", [default])
	]).
ciao_config_entry('DOTEMACS',
	[
	    depend_of([('SYSAVAIL', SysAvail), ('UPDATE_EMACS',yes)]),
	    default(get_emacs(SysAvail, DefValue), DefValue),
	    query("Define the emacs initialization where the Ciao "||
		  "settings will be added.", [minimum, extended]),
	    show("Emacs initialization file", [default])
	]).
ciao_config_entry('UPDATE_XEMACS',
	[
	    depend_of([('INSTALL_EMACS_SUPPORT',yes),
		('SYSAVAIL', SysAvail),('INSTYPE',InsType),
		('VERIFY_XEMACS',yes)]),
	    set_value(~update_xemacs(SysAvail, InsType, yes)),
	    default('yes'),
	    valid_values(['yes', 'no']),
	    query("Set to \"yes\" if you wish to configure XEmacs to\n"||
 		  "work with Ciao (modify ~/.xemacs/init.el).",
		  [minimum, extended]),
	    show("Modify ~/.xemacs/init.el file", [default])
	]).
ciao_config_entry('DOTXEMACS',
	[
	    depend_of([('SYSAVAIL', SysAvail), ('UPDATE_XEMACS',yes)]),
	    default(get_xemacs(SysAvail, DefValue), DefValue),
	    query("Define the xemacs initialization where the Ciao "||
		  "settings will be added.", [minimum, extended]),
	    show("XEmacs initialization file", [default])
	]).
ciao_config_entry('DOCROOT',
	[
	    depend_of([('SYSAVAIL', SysAvail), ('INSTYPE', InsType)]),
	    set_value((InsType=='src', ciaodesrc(S),
	        atom_concat(S, '/doc', Value)), Value),
	    default(get_docroot(SysAvail, DefValue), DefValue),
	    query(
    "Define this to be the directory in which you wish the documentation\n"||
    "installed. Ideally, this directory should be accessible via WWW,\n"||
    "emacs info, and man.\n", 
                [minimum, extended]),
	    show("Documentation will go here",[default])
	]).
ciao_config_entry('WEB_IMAGES_PATH',
	[
	    depend_of([('SYSAVAIL', SysAvail)]),
	    default(get_web_images_path(SysAvail, DefValue), DefValue),
	    query(
    "For the PiLLoW Web programming library, define the directory \n"||
    "(accessible via WWW) where the icons which come with PiLLoW \n"||
    "(in library/pillow/images) will go.", [minimum, extended])
	]).
ciao_config_entry('WEB_IMAGES_URL',
	[
	    depend_of([('SYSAVAIL', SysAvail)]),
	    default(get_web_images_url(SysAvail, DefValue), DefValue),
	    query(
		"Define the URL to access the previous directory via WWW." 
                , [minimum, extended])
	]).
ciao_config_entry('INSTALL_PROLOG_NAME',
	[
	    default('yes'),
	    valid_values(['yes', 'no']),
	    query(
    "Set to \"no\" if you do not wish to create a link to Ciao named\n"||
    "\"prolog\", for example because there are more prologs in the\n"||
    "system.", [minimum, extended])
	]).
ciao_config_entry('WITH_MYSQL',
	[
	    default(~verify_mysql),
	    valid_values(['yes', 'no']),
	    query(
   "Set to \"no\" if you do not wish to interface with the MySQL database.\n"||
   "If you choose to have the MySQL interface, you should have the MySQL\n"||
   "client part installed in the machine where you are compiling and using\n"||
   "it.  The MySQL daemon should also be up and running when using the\n"||
   "interface.", [extended]),
	    show("Install MySQL support", [default, minimum])
	]).
ciao_config_entry('MYSQL_CLIENT_DIRECTORY',
	[
	    depend_of([('WITH_MYSQL', 'yes')]),
	    default(~get_mysql_dir),
	    query(
    "You should also specify where the MySQL client library is installed.",
                [minimum, extended]),
	    show("MySQL client library path", [default])
	]).
ciao_config_entry('WITH_JAVA_INTERFACE',
	[
	    default(~verify_java),
	    valid_values(['yes', 'no']),
	    query(
    "Whether you have a reasonably recent version of Java.\n"||
    "If so the utilities for the Java interface under\n"||
    "$(CIAOSRC)/library/javall will be compiled , along with\n"||
    "examples and documentation.",
		[extended]),
	    show("Use JAVA interface", [default, minimum])
	]).
ciao_config_entry('USE_THREADS',
	[
	    default('yes'),
	    valid_values(['yes', 'no']),
	    query(
    "If you wish to compile an engine with threads capability\n"||
    "(concurrency), set the following variable to \"yes\".  Otherwise, set\n"||
    "it to \"no\".  If the architecture does not support threads (or\n"||
    "thread support has not yet been added to Ciao Prolog for this\n"||
    "architecture), this will be automatically disabled at compile time.\n"||
    "Concurrency support does not cause any appreciable runtime overhead\n"||
    "for non-concurrent programs, so it is safe to leave it as \"yes\".",
	    [extended])
	]).
ciao_config_entry('USE_POSIX_LOCKS',
	[
	    default('no'),
	    valid_values(['yes', 'no']),
	    query(
    "When using threads, locks are mandatory, and they do not make any\n"||
    "sense if you are not using threads.  So, threads enable locks.  Ciao\n"||
    "Prolog includes native code locks for some architectures, but allows\n"||
    "specifying the use of POSIX-compliant locks if posix libraries are\n"||
    "Available.  Posix Locks Will Be Automatically Selected If No Native\n"||
    "Lock Implementation Is included in Ciao for a given architecture.  We\n"||
    "recommend letting this option set to \"no\" since a primitive lock\n"||
    "implementation is usually much faster than then library-based POSIX\n"||
    "one.", [extended])
	]).
ciao_config_entry('OPTIM_LEVEL',
	[
	    default('optimized'),
	    valid_values(['optimized', 'normal']),
	    query(
    "Optimization level used when compiling the bytecode emulator. Choose\n"||
    "one of:\n"||
    "\n"||
    "   optimized       -- Turn on optimization flags\n"||
    "   normal          -- Normal emulator (non-optimized code)\n"||
    "\n"||
    "For normal use, we recommend leaving it as \"optimized\".  But if you\n"||
    "suspect that your compiler performs buggy optimizations (which should\n"||
    "not be the case), turn optimization off.  This can be happen more\n"||
    "easily in concurrent applicacions: if you write any thread-based\n"||
    "program and unexpected results appear, try recompiling Ciao without\n"||
    "optimization options first.", [extended])
	]).
ciao_config_entry('COMPRESS_LIBS',
	[
	    default('no'),
	    valid_values(['yes', 'no']),
	    query(
    "If you wish to compile the Ciao libraries with their bytecode\n"||
    "compressed then set the following variable to \"yes\". Libraries\n"||
    "generated this way will be smaller at the cost of a slightly slower\n"||
    "usage, both in their load as when employed to create an executable.",
		[extended])
	]).
ciao_config_entry('EXECMODE',
	[
	    default('775'),
	    query("Permissions for installed execs/dirs", [extended])
	]).
ciao_config_entry('DATAMODE',
	[
	    default('664'),
	    query("Permissions for installed data files", [extended])
	]).
ciao_config_entry('INSTALLGROUP',
	[
	    default(~get_installgroup),
	    query("This is the group for the installed files", [extended])
	]).
ciao_config_entry('USE_PPL',
	[
	    default(~verify_ppl),
	    valid_values(['yes', 'no']),
	    query("Do you have the Parma Polyhedra Library (PPL) installed "||
		"and want to use it?", [extended]),
	    show("Using PPL", [default,minimum])
	]).
ciao_config_entry('PPL_INTERFACE',
	[
	    depend_of([('USE_PPL','yes')]),
	    default(~get_ppl_interface),
	    query("Specify the full file name of the PPL-Ciao interface file",
	        [extended]),
	    show("PPL interface file", [default,minimum])
	]).
ciao_config_entry('COMPILE_CIAOPPCL',
	[
	    default('no'),
	    valid_values(['yes', 'no']),
	    query(
    "Please specify if you want to compile the CiaoPP Command Line Utility.",
    [extended]),
	    show("Compile CiaoPP Command line", [default,minimum])     
	]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ciao_config_entry('CROSS_COMPILER_HOST',
	[
	    default('none'),
	    query(
    "If you will cross-compile the engine later, please enter the user\n"||
    "name and address of the target machine to extract run-time\n"||
    "characteristics from -- e.g., \"root@my.other.host.com\".  If you\n"||
    "are not going to crosscompile, leave the default value.\n"||
    "Cross-compiling is at the moment done with \"make crossengine\" once \n"||
    "the regular compilation is over.", [extended])
	]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ciao_config_entry('BINDIR',
	[
	    depend_of([('BINROOT', BinRoot)]),
	    set_value(true, BinRoot)
	]).
ciao_config_entry('LIBDIR',
	[
	    depend_of([('INSTYPE',InsType), ('LIBROOT', LibRoot)]),
	    set_value(get_libdir(InsType, LibRoot, Value),Value)
	]).
ciao_config_entry('REALLIBDIR',
	[
	    depend_of([('INSTYPE', InsType),('LIBDIR', LibDir)]),
	    set_value(get_reallibdir(InsType, LibDir, Value), Value)
	]).
ciao_config_entry('ENGINEDIR',
	[
	    depend_of([('INSTYPE', InsType),('REALLIBDIR', RealLibDir)]),
	    set_value(get_enginedir(InsType, RealLibDir, Value), Value)
	]).
ciao_config_entry('DOCDIR',
	[
	    depend_of([('DOCROOT', DocRoot)]),
	    set_value(DocRoot)
	]).
ciao_config_entry('DEBUG_LEVEL',
	[
	    default('nodebug'),
	    valid_values(['nodebug', 'debug', 'profile', 'profile-debug',
	        'paranoid-debug']),
	    query(
    "You only want to change this if you are a developer.  Additionally,\n"||
    "setting the environment variable CIAODEBUG to the value \'-debug\'\n"||
    "at the time of compiling the engine will override the OPTIM_LEVEL\n"||
    "and DEBUG_LEVEL flags, and produce non optimized emulator code with\n"||
    "debugging information.\n"||
    "\n"||
    "Level of debugging built into the bytecode emulator. Choose one of:\n"||
    "\n"||
    "   nodebug         -- Do not include debug information or messages\n"||
    "   debug           -- Emulator with C level debugging info available\n"||
    "                      plus extended C compilation warnings\n"||
    "   profile         -- Include profiling options for the emulator\n"||
    "   profile-debug   -- Include profiling and debug options for the\n"||
    "                      emulator\n"||
    "   paranoid-debug  -- Emulator with C level debugging info available\n"||
    "                      plus paranoid C compilation warnings.", [extended])
	]).
ciao_config_entry('MAKEDIR',
	[
	    set_value(get_make(Value, _), Value)
	]).
ciao_config_entry('MAKENAME',
	[
	    set_value(get_make(_, Value), Value)
	]).
ciao_config_entry('CIAOMODEINITDIR',
	[
	    depend_of([('SYSAVAIL', SysAvail),('INSTYPE',InsType),
	        ('REALLIBDIR',RealLibDir),('INSTALL_EMACS_SUPPORT', EmacsSup)]),
	    set_value(get_ciao_mode_init_dir(SysAvail, InsType, EmacsSup,
	        RealLibDir, Value), Value)
	]).
ciao_config_entry('XEMACSINITDIR',
	[
	    depend_of([('SYSAVAIL', SysAvail),('INSTYPE',InsType),
	        ('REALLIBDIR',RealLibDir),('INSTALL_EMACS_SUPPORT', EmacsSup)]),
	    set_value(get_xemacs_init_dir(SysAvail, InsType, EmacsSup,
	        RealLibDir, Value), Value)
	]).
ciao_config_entry('BIBFILES',
	[
	    default(
		'/home/clip/bibtex/clip/clip,/home/clip/bibtex/clip/general')
	]).
