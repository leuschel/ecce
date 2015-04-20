:- module(_,[format_name/2, get_execmode/1, get_datamode/1,
% 	defaultexecpermissions/1,
% 	defaultdatapermissions/1,
% 	defaultcompresscommand/1,
% 	defaultcompressext/1,
	register_config_file/1], [make,functions,
	assertions]).

:- use_module(library(compiler),[use_module/1]).
:- use_module(library(format)).
%:- use_module(library(terms)).
:- use_module(library(lists)).
:- use_module(library(messages)).
:- use_module(library(system)).
:- use_module(library(distutils)).
:- use_module(library('make/make_rt')).
:- use_module(library('make/system_extra'),[
	ls/2,
	ls/3,
	cat/2,
	cat_append/2,
	del_endings_nofail/2,
	datime_string/1,
	copy_files/3,
	copy_files/4,
	(-)/1,
	add_preffix/3,
	do/2,
	set_perms/2,
	delete_files/1,
	del_dir_if_empty/1,
	del_file_nofail/1,
	del_files_nofail/1]).


:- comment(title, "Default installation makefile for use with lpdoc
   generated doc/manuals").

:- comment(author, "Manuel Hermenegildo").
:- comment(author, "Edison Mera").

:- comment(module, "This module provides the basic options for install
   documentation that has been generated with lpdoc2.  The main
   options are the next:

install		install documentation in dirs indicated in SETTINGS
uninstall	uninstall documentation from dirs indicated in SETTINGS

clean	 	deletes intermediate files, but leaves .texic & docs
distclean 	clean + deletes .texic: leaves only docs (.ps,.dvi,...)
docsclean 	clean + deletes docs: leaves only .texic
realclean 	docsclean + deletes also the .texic files

").

:- function(arith(false)).

% Constants that involves overall system:

%% This should really be somewhere else.
format_name(dvi,    'TeX device-indep'   ).
format_name(ps,     'postscript'         ).
format_name(pdf,    'Adobe pdf (acrobat)').
format_name(texi,   'GNU texinfo source' ).
format_name('HLP',  'Windows help'       ).
format_name(txt,    'plain text'         ).
format_name(ascii,  'ascii plain text'   ).
format_name(html,   'html hypertext'     ).
format_name(info,   'GNU info hypertext' ).
format_name(manl,   'UNIX man'           ).

%% Format which will not be compressed even if compression is selected
%% (also, compressing for formats installed in a subdirectory does not work)
%% PDF is not compressed due to it is already a compressed format.
nocompress(html).
nocompress(pdf).
nocompress(manl). %% To make it readable (or do most man commands uncompress?)

% Define this to be the permissions for installed execs/dirs and data files:
defexecmode(perm(rwx, rwx, rx)).
defdatamode(perm(rw, r, r)).
%% ---------------------------------------------------------------------------
%% ===========================================================================
% Command used to compress files by default
defcompresscommand := 'gzip -f'.
defcompressext     := 'gz'.
%% ---------------------------------------------------------------------------

register_config_file(FileName) :-
%	use_module(FileName),
	dyn_load_cfg_module_into_make(FileName).

get_datamode        := ~get_value_def(datamode, ~defdatamode).
get_execmode        := ~get_value_def(execmode, ~defexecmode).
get_compressext     := ~get_value_def(compressext,     ~defcompressext).
get_compresscommand := ~get_value_def(compresscommand, ~defcompresscommand).

:- comment(
   install_main_doc_file_and_pointer(Ext,SubDir,LocalSubDir,ExtraFiles),
   "@var{Ext} is the file extension (which also designates the
   documentation format). If @var{SubDir} is not '', then all related
   files are installed in that subdirectory (relative to the
   installation directory). If @var{LocalSubDir} is not '', then all
   files are installed from that subdirectory (relative to the
   document generation directory). @var{ExtraFiles} lists additional
   files which should be copied over (to @var{SubDir}, if
   specified).").

install_main_doc_file_and_pointer(Ext,SubDir,LocalSubDir,ExtraFiles) :-
	get_value(mainfile, MainFile),
	atom_concat([MainFile,'.',Ext],Target),
	(  file_exists(Target)
	-> working_directory(WD,WD),
	   get_value(docdir,DocDir),
	   make_dir_if_no_exist(DocDir),
	   (  SubDir = ''
	   -> DocSubDir = DocDir,
	      RSubDir = ''
	   ;  atom_concat([DocDir,'/',SubDir],DocSubDir),
	      make_dir_if_no_exist(DocSubDir),
      	      atom_concat([SubDir,'/'],RSubDir) ),
	   (  LocalSubDir = ''
	   -> RLocalSubDir = ''
	   ;  atom_concat(LocalSubDir,'/',RLocalSubDir) ),	   
	   copy_file(Target,RLocalSubDir,DocSubDir,[overwrite]),
	   copy_files(~ls(LocalSubDir,ExtraFiles),LocalSubDir,DocSubDir,[overwrite]),
	   -(set_perms(~atom_concat([DocSubDir,'/',Target]),~get_datamode)),
	   -(set_perms(~add_preffix(~ls(DocSubDir,ExtraFiles),
	       ~atom_concat(DocSubDir,'/')),
	   ~get_datamode)),
	     (  get_compresscommand(CCommand),
	        get_compressext(CExt),
		\+ nocompress(Ext)
	     -> do(['cd ', DocDir, '; ', CCommand,' ',Target],nofail),
	        generate_html_pointer(DocDir, RSubDir,MainFile,Ext,
	                      ~atom_concat(['.',CExt]),'(compressed)')
	     ;  generate_html_pointer(DocDir, RSubDir,MainFile,Ext,'','') )
	;
	    working_directory(WD1,WD1),
	    warning_message(
            "~w/~w not found; should be generated before installation",
	        [WD1,Target])).

generate_html_pointer(Dir, RSubDir,MainFile,Ext,CompExt,CompText) :-
	atom_concat([Dir, '/', MainFile,'_',Ext,'.htmlindex'],IndexFile),
	format_name(Ext,FN),
	open(IndexFile,write,O),
	format(O,"<UL><LI><A HREF=~w~w.~w~w>",[RSubDir,MainFile,Ext,CompExt]),
	format(O,"<B>~w</B> Reference Manual - in ~w format ",[MainFile,FN]),
	format(O,"~w</A></UL>",[CompText]),
	close(O),
	-set_perms(IndexFile,~get_datamode).

uninstall_main_doc_file_and_pointer(Ext,SubDir,ExtraFiles) :-
	get_value(docdir,DocDir),
	(  SubDir = ''
	-> DocSubDir = DocDir
	;  atom_concat([DocDir,'/',SubDir],DocSubDir) ),
	(  file_exists(DocDir)
	-> get_value(mainfile,MainFile),
	   working_directory(WD,WD),
           cd(DocDir),
	   del_file_nofail(~atom_concat([MainFile,'_',Ext,'.htmlindex'])),
	   (  file_exists(DocSubDir)
	   -> cd(DocSubDir)
	   ;  true ),
	     delete_files(~ls(ExtraFiles)),
	     (  get_compresscommand(_CCommand),
	        get_compressext(CExt),
		\+ nocompress(Ext)
	     -> del_file_nofail(~atom_concat([MainFile,'.',Ext,'.',CExt]))
	     ;  del_file_nofail(~atom_concat([MainFile,'.',Ext])) ),
           cd(DocDir),
           del_dir_if_empty(DocDir),
	   (  SubDir = ''
	   -> true
	   ;  del_dir_if_empty(DocSubDir) ),
	   cd(WD) 
	;  warning_message("Installation directory ~w does not exist",
	                   [DocDir]) ).

%% ***************************************************************************
%% The next has been moved from lpdoc.  It is necessary to check that nothing
%% was broken
%% ***************************************************************************

%% ---------------------------------------------------------------------------
target_comment(install,   "Install manual and indices in install area", []).
%% ---------------------------------------------------------------------------

%% Eliminated dependencies so that regeneration of manuals is never forced 
%% during installation. This means that a prior 'gmake' has to be done by hand.

%% INSTALLATION: first, install manuals; then, install index entries.
%% Installation (and deinstallation) of manuals 

install <- :-
	all_values(docformat,DocFormats),
	add_preffix(DocFormats,install,Targets),
	get_value(mainfile, MainFile),
	bold_message("Installing all default manuals for ~w: ~n*** ~w",
	              [MainFile,Targets]),
	make(Targets),
	add_preffix([htmlindex,infoindex],install,ITarg),
	bold_message("Installing indices for ~w: ~w",[MainFile,ITarg]),
	make(ITarg).

%% ---------------------------------------------------------------------------
target_comment(uninstall,  "Uninstall manual and indices from install area", []).
%% ---------------------------------------------------------------------------

%% Eliminates all files installed  (provided SETTINGS file has not changed)
uninstall <- :-
	all_values(docformat,DocFormats),
	add_preffix(DocFormats,uninstall,Targets),
	get_value(mainfile,MainFile),
	bold_message("Uninstalling manuals for ~w: ~w",[MainFile,Targets]),
	make(Targets),
	add_preffix([htmlindex,infoindex],uninstall,ITarg),
	bold_message("Uninstalling indices for ~w: ~w",[MainFile,ITarg]),
	make(ITarg),
	get_value(docdir,DocDir),
	del_dir_if_empty(DocDir).

%% ---------------------------------------------------------------------------
target_comment(installtexi,  "Install texi manual in install area", []).
%% ---------------------------------------------------------------------------

installtexi <-   :-
	install_main_doc_file_and_pointer(
			texi,'','','autofig*.eps|autofig*.txt').

%% ---------------------------------------------------------------------------
target_comment(uninstalltexi,  "Uninstall texi manual from install area", []).
%% ---------------------------------------------------------------------------

uninstalltexi <-   :-
	uninstall_main_doc_file_and_pointer(texi,'','').
        %% just deleting autofig*.eps dangerous: needed for, e.g., dvi
        %% Also: there may be figures from other documents here...

%% ---------------------------------------------------------------------------
target_comment(installdvi,  "Install dvi manual in install area", []).
%% ---------------------------------------------------------------------------

installdvi <-  :-
	install_main_doc_file_and_pointer(dvi,'','','autofig*.eps').

%% ---------------------------------------------------------------------------
target_comment(uninstalldvi,  "Uninstall dvi manual from install area", []).
%% ---------------------------------------------------------------------------

uninstalldvi <-   :-
	uninstall_main_doc_file_and_pointer(dvi,'','').
        %% just deleting autofig*.eps dangerous: needed for, e.g., texi
        %% Also: there may be figures from other documents here...

%% ---------------------------------------------------------------------------
target_comment(installps,  "Install ps manual in install area", []).
%% ---------------------------------------------------------------------------

installps <-   :-
	install_main_doc_file_and_pointer(ps,'','','').

%% ---------------------------------------------------------------------------
target_comment(uninstallps,  "Uninstall ps manual from install area", []).
%% ---------------------------------------------------------------------------

uninstallps <-  :-
	uninstall_main_doc_file_and_pointer(ps,'','').

%% ---------------------------------------------------------------------------
target_comment(installpdf,  "Install pdf manual in install area", []).
%% ---------------------------------------------------------------------------

installpdf <-  :-
	install_main_doc_file_and_pointer(pdf,'','','').

%% ---------------------------------------------------------------------------
target_comment(uninstallpdf,  "Uninstall pdf manual from install area", []).
%% ---------------------------------------------------------------------------

uninstallpdf <-  :-
	uninstall_main_doc_file_and_pointer(pdf,'','').

%% ---------------------------------------------------------------------------
target_comment(installascii, "Install ascii manual in install area", []).
%% ---------------------------------------------------------------------------

installascii <- :-
	install_main_doc_file_and_pointer(ascii,'','','').

%% ---------------------------------------------------------------------------
target_comment(uninstallascii,  "Uninstall ascii manual from install area", []).
%% ---------------------------------------------------------------------------

uninstallascii <-  :-
	uninstall_main_doc_file_and_pointer(ascii,'','').

%% ---------------------------------------------------------------------------
target_comment(installhtml,  "Install html manual in install area", []).
%% ---------------------------------------------------------------------------

installhtml <-  :-
	get_value(mainfile,MainFile),
	install_main_doc_file_and_pointer(
	      html,
	      ~atom_concat([MainFile,'_html']),
	      ~atom_concat([MainFile,'_html']),
	      ~atom_concat([MainFile,'.css|autofig*.jpg|',MainFile,'*.html'])).

%% ---------------------------------------------------------------------------
target_comment(uninstallhtml, "Uninstall html manual from install area", []).
%% ---------------------------------------------------------------------------

uninstallhtml <-  :-
	get_value(mainfile,MainFile),
	uninstall_main_doc_file_and_pointer(
	      html,
	      ~atom_concat([MainFile,'_html']),
	      ~atom_concat([MainFile,'.css|autofig*.jpg|',MainFile,'*.html'])).

%% ---------------------------------------------------------------------------
target_comment(installinfo,  "Install info manual in install area", []).
%% ---------------------------------------------------------------------------

installinfo <-   :-
	install_main_doc_file_and_pointer(info,'','',''),
	bold_message(
	 "Remember to add an entry to info dir (or select infoindex)!"),
	get_value(docdir,DocDir),
	bold_message(
	 "Remember to add ~n*** ~w~n*** to INFOPATH or copy to std info area!",
	 [DocDir]).

%% ---------------------------------------------------------------------------
target_comment(uninstallinfo, "Uninstall info manual from install area", []).
%% ---------------------------------------------------------------------------

uninstallinfo <-   :-
	uninstall_main_doc_file_and_pointer(info,'','').

%% ---------------------------------------------------------------------------
target_comment(installtxt,  "Install txt manual in install area", []).
%% ---------------------------------------------------------------------------

installtxt <-   :-
	install_main_doc_file_and_pointer(txt,'','','').

%% ---------------------------------------------------------------------------
target_comment(uninstalltxt,"Uninstall txt manual from install area", []).
%% ---------------------------------------------------------------------------

uninstalltxt <-    :-
	uninstall_main_doc_file_and_pointer(txt,'','').

%% ---------------------------------------------------------------------------
target_comment(installmanl,  "Install manl manual in install area", []).
%% ---------------------------------------------------------------------------

installmanl <-   :-
	install_main_doc_file_and_pointer(manl,'manl','',''),
	get_value(docdir,DocDir),
	bold_message(
	  "Remember to add~n*** ~w~n*** to MANPATH or copy to std man area!",
	  [DocDir]).

%% ---------------------------------------------------------------------------
target_comment(uninstallmanl, "Uninstall manl manual from install area", []).
%% ---------------------------------------------------------------------------

uninstallmanl <-   :-
	uninstall_main_doc_file_and_pointer(manl,'manl','').

%% ----------------------------------------------------------------------------
%% Installation of index entries
%% ----------------------------------------------------------------------------

%% ---------------------------------------------------------------------------
target_comment(installhtmlindex,
	"Install docs in install area html index", []).
%% ---------------------------------------------------------------------------

%% $(HTMLINDEXHEADFILE) $(HTMLINDEXTAILFILE)
installhtmlindex <-   :-
	get_value(mainfile,MainFile),
	atom_concat([MainFile,'.htmlindex'],TI),
	(  file_exists(TI)
	-> atom_concat([MainFile,'.htmlbullet'],TB),
	   get_value(docdir,DocDir),
	   atom_concat(DocDir,'/',DocDirP),
	   make_dir_if_no_exist(DocDir,~get_execmode),	
	   copy_file(TB,DocDir,[overwrite]),
	   copy_file(TI,DocDir,[overwrite]),
	   %% In this case the figures must be in ~docdir!
	   copy_files(~ls('autofig*.jpg'),DocDir,[overwrite]),
	   -set_perms(~atom_concat([DocDirP,TI]),~get_datamode),
	   -set_perms(~atom_concat([DocDirP,TB]),~get_datamode),
	   -set_perms(~add_preffix(~ls(DocDir,'autofig*.jpg'), DocDirP),
	       ~get_datamode),
	   make(updatehtmlindex),
	   make(installreadme)
	; warning_message(
            "~w not found; should be generated before installation",[TI])).

%% ---------------------------------------------------------------------------
target_comment(uninstallhtmlindex,
	"Uninstall docs from install area html idx", []).
%% ---------------------------------------------------------------------------

uninstallhtmlindex <-    :-
	get_value(docdir, DocDir),
	(  file_exists(DocDir)
	-> working_directory(WD,WD),
	   cd(DocDir),
	     get_value(mainfile, MainFile),
	     atom_concat([MainFile,'.htmlindex'],TI),
	     del_file_nofail(TI),
	     atom_concat([MainFile,'.htmlbullet'],TB),
	     del_file_nofail(TB),
	     delete_files(~ls('autofig*.jpg')),
	   cd(WD),
	   make(updatehtmlindex),
           make(uninstallreadmeanddir)
	;  warning_message("Installation directory ~w does not exist",
	                   [DocDir]) ).

%% ---------------------------------------------------------------------------
target_comment(updatehtmlindex, "Update html index for all docs in docdir", []).
%% ---------------------------------------------------------------------------

updatehtmlindex <-   :-
	   working_directory(WD,WD),
	   get_value(docdir,DocDir),
	   make_dir_if_no_exist(DocDir,~get_execmode),	
	   cd(DocDir),
	   get_value(htmlindex_headfile, HIH),
	   cat(HIH,'index.html'),
	   cat_append(~ls('*.htmlbullet'),'index.html'),
	   cat_append(~ls('*.htmlindex'),'index.html'),
	   open('index.html',append,O),
	   format(O,"<p><hr><p><b>Last modified on:</b> ~s.<p>",
	            [~datime_string]),
	   close(O),
	   get_value(htmlindex_tailfile, HIT),
	   cat_append(HIT,'index.html'),
	   -set_perms('index.html',~get_datamode),
	   cd(WD).

%% ---------------------------------------------------------------------------
target_comment(installreadme,  "Install readme in install area", []).
%% ---------------------------------------------------------------------------

installreadme <-   :-
	get_value(docdir, DocDir),
	make_dir_if_no_exist(DocDir,~get_execmode),	
	working_directory(WD,WD),
	cd(DocDir),
	open('README',write,O),
	format(O,
	  "*** WARNING: Files automatically generated -- do not edit!~n",[]),
	-set_perms('README',~get_datamode),
	cd(WD).

%% ---------------------------------------------------------------------------
target_comment(uninstallreadmeanddir,  "Uninstall readme from install area", []).
%% ---------------------------------------------------------------------------

uninstallreadmeanddir <-   :-
	get_value(docdir, DocDir),
	(  file_exists(DocDir)
	-> del_file_nofail(~atom_concat([DocDir,'/README'])),
           del_dir_if_empty(DocDir)
	;  warning_message("Installation directory ~w does not exist",
	                   [DocDir]) ).

%% ---------------------------------------------------------------------------
target_comment(installinfoindex,  "Install doc in install area info index", []).
%% ---------------------------------------------------------------------------

%% $(INFODIRHEADFILE) $(INFODIRTAILFILE)
installinfoindex <-   :-
        get_value(mainfile, MainFile),
	atom_concat([MainFile,'.infoindex'],Target),
	(  file_exists(Target)
	-> get_value(docdir, DocDir),
	   make_dir_if_no_exist(DocDir,~get_execmode),	
	   copy_file(Target,DocDir,[overwrite]),
	   working_directory(WD,WD),
	   cd(DocDir),
	   -set_perms(Target,~get_datamode),
	   cd(WD),
	   make(updateinfodir),
	   make(installreadme)
	; warning_message(
            "~w not found; should be generated before installation",[Target])).

%% ---------------------------------------------------------------------------
target_comment(uninstallinfoindex,
	"Uninstall from install area info index", []).
%% ---------------------------------------------------------------------------

uninstallinfoindex <-   :-
	get_value(docdir, DocDir),
	(  file_exists(DocDir)
	-> get_value(mainfile, MainFile),
	   del_file_nofail(~atom_concat([DocDir,'/',MainFile,'.infoindex'])),
	   make(updateinfodir),
	   make(uninstallreadmeanddir)
	;  warning_message("Installation directory ~w does not exist",
	                   [DocDir]) ).

%% ---------------------------------------------------------------------------
target_comment(updateinfodir,
	"Update info directory for all docs in docdir", []).
%% ---------------------------------------------------------------------------

updateinfodir <-   :-
	working_directory(WD,WD),
	get_value(docdir,DocDir),
	make_dir_if_no_exist(DocDir,~get_execmode),	
	cd(DocDir),
	get_value(infodir_headfile, IDH),
	cat(IDH,dir),
	cat_append(~ls('*.infoindex'),dir),
	get_value(infodir_tailfile, IDT),
	cat_append(IDT,dir),
	-set_perms(dir,~get_datamode),
	cd(WD).

:- push_prolog_flag(multi_arity_warnings,off).

make_dir_if_no_exist(Dir) :-
	make_dir_if_no_exist(Dir, ~get_execmode).

make_dir_if_no_exist(Dir, ExecMode) :-
	file_exists(Dir) ->
	true
 ;
	make_dirpath(Dir),
	-set_perms(Dir, ExecMode).

:- pop_prolog_flag(multi_arity_warnings).

%% ===========================================================================
%% Commands for cleaning up after generating manuals
%% ===========================================================================

%% ---------------------------------------------------------------------------
target_comment(texclean,   "Delete tex-related intermediate files", []).
%% ---------------------------------------------------------------------------

texclean <- []  :-
	(   get_value(mainfile, MainFile) ->
	    del_endings_nofail(
	    [   '.aux','.cp','.cps','.fn','.fns','.ky','.kys','.log',
		'.tp','.tps','.op','.ops','.fi','.fis','.pd','.pds',
		'.pr','.prs','.ug','.ugs','.co','.cos','.fu','.fus',
		'.li','.lis','.pg','.pgs','.ap','.aps','.mo','.mos',
		'.gl','.gls','.toc','.te','.tes','.vr','.vrs','.de',
		'.des','.bbl','.blg'
	    ],
	    MainFile)
	;
	    true
	),
        ( get_value(libtexinfo, yes) -> del_file_nofail('texinfo.tex') ; true ).

%% ---------------------------------------------------------------------------
target_comment(clean,   "Delete intermediate files. Leaves .texic & docs", []).
%% ---------------------------------------------------------------------------

clean <- [texclean] :-
        (   get_value(mainfile, MainFile) ->
	    del_endings_nofail(['refs.el','refs.aux','refs.blg','_dvips.log'],
		MainFile)
	;
	    true
	),
	delete_files(~ls('*~|*.asr|*.itf|*.po|autofig*.ppm')).

%% ---------------------------------------------------------------------------
target_comment(distclean,
	"Perform clean+del .texic etc. Leaves dvi, ps, etc", []).
%% ---------------------------------------------------------------------------

distclean <- [clean] :-
	delete_files(~ls('*.texic|*.refs')).

%% ---------------------------------------------------------------------------
target_comment(docsclean, "Perform clean+delete docs. Leaves only .texic", []).
%% ---------------------------------------------------------------------------

docsclean <- [clean] :-
	get_value(mainfile, MainFile) ->
	docsclean(MainFile)
 ;
	true.

%% ---------------------------------------------------------------------------
target_comment(realclean, "Perform docsclean + delete also .texic files", []).
%% ---------------------------------------------------------------------------

realclean <- [docsclean] :-
	del_files_nofail(~ls('*.texic|*.refs|autofig*.eps|autofig*.txt')).
