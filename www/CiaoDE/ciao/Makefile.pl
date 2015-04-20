% ===========================================================================
:- module(_,_,[dcg, make, functions, assertions, hiord]).
% ===========================================================================
:- comment(title,  "Ciao Global Compilation/Installation LPMakefile").
:- comment(author, "Edison Mera").
% ===========================================================================

:- use_module(library('make/system_extra')).
:- use_module(library(file_utils)).
:- use_module(library('engine/system_info'), [get_debug/1]).
:- use_module(library(lists),[list_concat/2,append/3]).
:- use_module(library(format), [format/2, format/3]).
:- use_module(library(persvalue)).
:- use_module(library(aggregates),[findall/3]).
:- use_module(engine(system_info), [get_ciao_ext/1, get_exec_ext/1]).
:- use_module(library(distutils)).
:- use_module(library('distutils/distclean')).
:- use_module(ciaosrc('CIAOSHARED')).
:- use_module(ciaodesrc('CIAODESHARED'), [ciaodesrc/1]).
:- use_module(ciaosrc('CIAOSETTINGS')).

:- function(arith(false)).

% ============================================================================
% CONSTANTS
% ============================================================================

tags           := ~atom_concat(~ciaosrc, '/TAGS').

defaults <- [] # "Preventing lpmake from being called without target" :- true.

show_info <- :- display_list(['ciaodesrc=', ~ciaodesrc,'\n']).

% ============================================================================
% CONFIGURATION MENU
% ============================================================================

ciaosettings <- [] # "Load initial Ciao Settings." :-
	ciaoinitvals.

menuconfig <- [ciaosettings] # "Ciao configuration" :-
	menuconfig.

% ============================================================================
% COMPILATION
% ============================================================================

environment <- [] :-
% Refresh the latest engine
	make_subdir(~lpmake, ~gmake, '.', ~setlocalciao, ~atom_concat([
	    'engine > ', ~install_log])),
% Build a new ciao compiler
	make_subdir(~lpmake, ~gmake, 'ciaoc', ~setlocalciao, ~atom_concat([
            ' > ', ~install_log])).

tags <- [~tags] # "Creation of TAGS for use with the find-tag command "||
	"(ESC-.) in emacs" :- true.

~tags <- ['Makefile.pl'] :-
	tags(Tags),
	ciaosrc(Ciaosrc),
	del_file_nofail(Tags),
	list_files_filter_rec(Ciaosrc, '*.pl','','',[],0,List,[]),
	etags(List, Tags).

deltags <- [] # "Deletion of TAGS file." :-
	del_file_nofail(~tags).

%defaulttype(dyn).

% libraries <- :-
% 	make([alllib, ~atom_concat(alllibrary, ~defaulttype), allcontrib]).

autolibs(BaseDir) :-
	autolibs(
	    [
	        'set_prolog_flag(compress_lib, ', ~compress_libs, ').\n',
	        'use_module(library(gen_asr_file)).\n'
	    ], 
	    BaseDir,
	    [gen_asr_file],
%	    [],
	    ~datamode,
	    ~setlocalciao,
	    ~ciaosh).

allcontrib <- :-
	atom_concat(~ciaosrc, '/contrib/chat80', ChatDir),
	atom_concat(ChatDir, '/NODISTRIBUTE', NDChatDir),
	(   file_exists(ChatDir),
	    \+ file_exists(NDChatDir) ->
	    make_subdir(~lpmake, ~lpmake, 'contrib/chat80', '', '')
	;   true
	),
	autolibs(contrib).

alllib <- :-
	autolibs(lib).

alllibrary <- :-
	mysqllibs,
	autolibs(library).

clp_libs <- :-
	autolibs('library/clpq'),
	autolibs('library/clpr').

alllibrarydyn  <- [ciao_so_libs, clp_libs, ciao_special_libs, alllibrary] :-
	true.
alllibrarystat <- [              clp_libs, ciao_special_libs, alllibrary] :-
	true.

:- comment(bug, "ciao_special_libs, ciao_so_libs and mysqllibs can be
   migrated to lpmake.").

ciao_special_libs <- :-
	(
	    with_java_interface(yes) ->
	    make_subdir(~lpmake, ~gmake, 'library/javall', '', 'all')
	;
	    true
	),
	make_subdir(~lpmake, ~gmake, 'library/pillow', '', '').

ciao_so_libs <- :-
	make_subdirs(~lpmake, ~gmake, ['library/sockets', 'library/random'],
	    '', 'all').

mysql_directory    := 'persdb_mysql'.
mysql_directory_op := 'persdb_mysql_op'.

mysqllibs <- :- mysqllibs.
mysqllibs :-
	with_mysql(yes) ->
	bold_message("Compiling MySQL Libraries..."),
	del_file_nofail(~atom_concat(['library/', ~mysql_directory,
	    '/NOCOMPILE'])),
	replace_strings_in_file([["where_mysql_client_lives",
	    ~atom_codes(~mysql_client_directory)]],
	    ~atom_concat(['library/', ~mysql_directory,
	        '/linker_opts.pl.skel']),
	    ~atom_concat(['library/', ~mysql_directory,
	        '/linker_opts.pl'])),
	do(['cd library ; ', ~setlocalciao, ' ', ~ciaoc, ' -x ',
	    ~mysql_directory, '/mysql_use_client'], fail),
	(
	    file_exists(~atom_concat(['library/', ~mysql_directory_op])) ->
	    replace_strings_in_file([["where_mysql_client_lives",
	        ~atom_codes(~mysql_client_directory)]],
		~atom_concat(['library/', ~mysql_directory_op,
		    '/linker_opts.pl.skel']),
		~atom_concat(['library/', ~mysql_directory_op,
		    '/linker_opts.pl'])),
	    do(['cd library ; ', ~setlocalciao, ' ', ~ciaoc, ' -x ',
		~mysql_directory_op, '/mysql_use_client ; rm -f ',
		~mysql_directory_op, '/mysql_use_client'], fail)
	;
	    true
	)
    ;
	true.


% ============================================================================
% GENERATION OF DOCUMENTATION
% ============================================================================

docs <- [docsemacs, docsreadmes, docsreference] # "Creates documentation files"
	:- true.

docsemacs <- [] # "Creation of emacs documentation files" :-
	make_subdir(~lpmake, ~gmake, 'emacs-mode', ~setlocalciao, 'docs').

docsreference <- [] # "Creation of reference documentation." :-
	% Delete the html file to force the htmlindex creation.
	del_file_nofail(~atom_concat(['doc/reference/', ~mainname,
	    '.htmlindex'])),
	% Delete the html file to force the html creation.
	del_file_nofail(~atom_concat(['doc/reference/', ~mainname, '.html'])),
	make_subdir(~lpmake, ~lpdoc2, 'doc/reference', ~setlocalciao,
	    'all htmlindex infoindex').

docsreadmes <- [] # "Creation of readme files." :-
	make_subdir(~lpmake, ~lpmake, 'doc/readmes', ~setlocalciao,
            '-l distutils/readmemkf -m RSETTINGS force'),
	rename_file('NewUser', 'NewUser.tmp'),
	replace_strings_in_file([
	    ["<LIBROOT>",  ~atom_codes(~ciaolibroot)],
	    ["<LPDOCDIR>", ~atom_codes(~ciaodocroot)]],
	    'NewUser.tmp', 'NewUser').

% ============================================================================
% INSTALLATION
% ============================================================================

install <- [justinstall, reconfigure] :- true.

justinstall <- [] # "Installation of ciao components." :-
	make(~atom_concat('justinstall_', ~instype)).

justinstall_src <- :-
	installpillow.

justinstall_ins <- [] :-
	justinstall_ins.

justinstall_ins :-
	make_dirpath(~buildreallibdir),
	--set_perms([~buildlibdir, ~buildreallibdir], ~execmode),
	make([
		 installeng,
		 installincludes,
		 installciaoc,
		 installshell,
		 installetc,
		 installemacsmode,
		 installdoc
	     ]),
	installlib(lib),
	installlib(library),
	installpillow,
	installlib(contrib),
	installsrc(examples, '*.po|*.itf|*~').

installeng <- :-
	make_subdir(~lpmake, ~gmake, '.', ~setlocalciao, 'installeng').

installincludes <- :-
	make_subdir(~lpmake, ~gmake, '.', ~setlocalciao, 'installincludes').

installciaoc <- [] # "Installation of the compiler." :-
	make_subdir(~lpmake, ~gmake, 'ciaoc', ~setlocalciao, 'install').

installshell <- [] # "Installation of the top level shell." :-
	make_subdir(~lpmake, ~gmake, 'shell', ~setlocalciao, 'install').

installetc <- [] # "Installation of applications in etc." :-
	make_subdir(~lpmake, ~lpmake, 'etc', ~setlocalciao, 'install').

installemacsmode <- [] # "Installation of graphical env (emacs mode)." :-
	install_emacs_support(yes) ->
	make_subdir(~lpmake, ~gmake, 'emacs-mode', ~setlocalciao, 'install')
    ;
	true.

installdoc <- [] # "Installion of documentation files." :-
	make_subdir(~lpmake, ~lpmake, 'doc/reference', ~setlocalciao,
	'-l distutils/installmkf -m LPSETTINGS install').

installlib(LibName) :-
	installsrc(LibName, '*~').

installsrc(DirName, Exclude) :-
	rpm_build_root(RpmBuildRoot),
	atom_concat([~reallibdir, '/', DirName], StdLibDir),
	atom_concat(RpmBuildRoot, StdLibDir, BuildStdLibDir),
	bold_message("Installing ~w ~w libraries in ~w",
	    [~basemain, DirName, StdLibDir]),
	copy_dir_rec(DirName, BuildStdLibDir, '*', Exclude, '.svn', [], 0),
	list_files_rec(BuildStdLibDir, BuildStdLibDir, FileList),
	list_dirs_rec(BuildStdLibDir, BuildStdLibDir, DirList),
	-set_perms(BuildStdLibDir, ~execmode),
	-set_perms(DirList, ~execmode),
	-set_perms(FileList, ~datamode).

installpillow :-
	webimagespath(WebImagesPathP),
	atom_concat(WebImagesPath,'/',WebImagesPathP),
	rpm_build_root(RpmBuildRoot),
	execmode(ExecMode),
	atom_concat(RpmBuildRoot, WebImagesPath, BuildWebImagesPath),
	bold_message("Installing PiLLoW images in ~w", [WebImagesPath]),
	make_dirpath(BuildWebImagesPath),
	--set_perms(BuildWebImagesPath, ExecMode),
	copy_dir_rec('library/pillow/images', BuildWebImagesPath, '*', '*~',
	    '.svn', [], 0),
	list_files_rec(BuildWebImagesPath, BuildWebImagesPath, FileList),
	list_dirs_rec(BuildWebImagesPath, BuildWebImagesPath, DirList),
	--set_perms(DirList, ~execmode),
	--set_perms(FileList, ~datamode).

% ============================================================================
% SCRIPT CONFIGURATION
% ============================================================================

% ----------------------------------------------------------------------------
% BASH
% ----------------------------------------------------------------------------
bashrc <- :-
	update_bashrc(yes) ->
	(-configure_script(~dotbashrc, "#", bashrc_lines))
    ;	true.

uninstall_bashrc <- :-
	update_bashrc(yes) ->
	(-uninstall_script(~dotbashrc, "#"))
    ;	true.

bashrc_lines(Key) -->
	start_message("#", Key),
	bashrc_content,
	end_message("#", Key).

bashrc_content -->
	"if [ -f ", libroot, "/DOTprofile ] ; then\n"||
	"  . ", libroot, "/DOTprofile\n"||
	"fi\n".
% ----------------------------------------------------------------------------
% CSH
% ----------------------------------------------------------------------------
cshrc  <- :-
	update_cshrc(yes)  ->
	(-configure_script(~dotcshrc, "#",  cshrc_lines))
    ;	true.

uninstall_cshrc  <- :-
	update_cshrc(yes)  ->
	(-uninstall_script(~dotcshrc, "#"))
    ;	true.

cshrc_lines(Key) -->
	start_message("#", Key),
	cshrc_content,
	end_message("#", Key).

cshrc_content -->
	"if ( -e ", libroot, "/DOTcshrc ) then\n"||
	"  source ", libroot, "/DOTcshrc\n"||
	"endif\n".
% ----------------------------------------------------------------------------
% EMACS
% ----------------------------------------------------------------------------
emacs  <- :-
	update_emacs(yes) ->
	(-configure_script(~dotemacs, ";",  emacs_lines))
    ;	true.

uninstall_emacs  <- :-
	update_emacs(yes) ->
	(-uninstall_script(~dotemacs, ";"))
    ;	true.

xemacs <- :-
	update_xemacs(yes) ->
	(-configure_script(~dotxemacs, ";",  emacs_lines))
    ;	true.


emacs_lines(Key) -->
	start_message(";", Key),
	emacs_content,
	end_message(";", Key).

emacs_content -->
	"(if (file-exists-p \"", libemacs, "\")\n"||
	"(load-file \"", libemacs, "\")\n"||
	")\n".
% ----------------------------------------------------------------------------
% COMMON STRINGS
% ----------------------------------------------------------------------------
libroot(S, T) :-
	append(~atom_codes(~ciaolibsrc), T, S).

libemacs(S, T) :-
	append(~atom_codes(~ciaolibemacs), T, S).

ciaolibsrc(LibRoot) :-
	instype(src) ->
	atom_concat(~ciaosrc, '/etc', LibRoot)
    ;   atom_concat(~ciaolibroot, '/ciao', LibRoot).

ciaolibemacs(LibEmacs) :-
	instype(src) ->
	atom_concat(~ciaosrc,'/emacs-mode/ciao-mode-init.el', LibEmacs)
    ;   atom_concat([~ciaolibroot, '/ciao/', ~versionmain, '/ciao-mode-init.el'],
	LibEmacs).

reconfigure <- [bashrc, cshrc, emacs, xemacs] # "Modifies the "||
	".bashrc/.cshrc/.emacs files to let ciao run from the installed "||
	"lib files." :- true.

% ============================================================================
% UNINSTALLATION
% ============================================================================

% Remember that if instype is src, uninstallation does not delete any files,
% it only removes the configuration settings in shell scripts and emacs 
% initialization files.

:- comment(bug, "The current installation method cannot uninstall the
   installed pillow images").

unreconfigure <- [uninstall_bashrc, uninstall_cshrc, uninstall_emacs] #
        "Leaves the .bashrc/.cshrc/.emacs file in its original state." :-
        true.

uninstall <- [justuninstall, unreconfigure] # "Uninstall ciao." :- true.

justuninstall <- :-
	instype(InsType),
	(
	    InsType==src ->
	    true
	;
	    InsType==ins ->
	    douninstall
%       ;
%           false
	).

douninstall :-
	uninstalllib(lib),
	uninstalllib(library),
	uninstalllib(contrib),
	uninstalllib(examples),
	uninstallciaoc,
	uninstallshell,
	uninstalletc,
	uninstalldoc,
	uninstallemacsmode,
	uninstallincludes,
	delete_dir_rec(~buildreallibdir).

uninstalllib(LibName) :-
	atom_concat([~buildreallibdir, '/', LibName],
	    BuildStdLibDir),
	delete_dir_rec(BuildStdLibDir).

uninstallincludes :-
	bold_message("Uninstallation of C include files for ~w~w.",
	    [~get_os, ~get_arch]),
	delete_dir_rec(~atom_concat(~rpm_build_root,~installedincludedir)),
	del_file_nofail(~atom_concat([~rpm_build_root,~includeroot,
	    '/ciao_prolog.h'])).

uninstalldoc :-
	bold_message("Uninstallation of documentation files."),
	make_subdir(~lpmake, ~lpmake, 'doc/reference', ~setlocalciao,
	'-l distutils/installmkf -m LPSETTINGS uninstall').

uninstalletc :-
	bold_message("Uninstallation of applications in etc."),
	make_subdir(~lpmake, ~lpmake, 'etc', ~setlocalciao, 'uninstall').

uninstallciaoc :-
	bold_message("Uninstallation of the compiler."),
	make_subdir(~lpmake, ~gmake, 'ciaoc', ~setlocalciao, 'uninstall').

uninstallshell :-
	bold_message("Uninstallation of the top level shell."),
	make_subdir(~lpmake, ~gmake, 'shell', ~setlocalciao, 'uninstall').

uninstallemacsmode :-
	bold_message("Uninstallation of the graphical env (emacs mode)."),
	make_subdir(~lpmake, ~gmake, 'emacs-mode', ~setlocalciao, 'uninstall').
