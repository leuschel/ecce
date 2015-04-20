% ===========================================================================
:- module(_,_,[make,functions,assertions]).
% ===========================================================================
:- use_module(library('make/system_extra'),[replace_strings_in_file/3,
	copy_file/3, copy_file/4, make_dirpath/1, del_file_nofail/1,
	del_files_nofail/1, do/2, cd/1, add_suffix/3,set_perms/2,'-'/1,
	'--'/1,	symbolic_link/3,ls/2, delete_file/1, file_exists/1]).
:- use_module(library('compiler/exemaker'),[make_exec/2]).
:- use_module(library(lists),[append/3]).
:- use_module(library(aggregates),[findall/3]).
%:- use_module(library(terms)).
:- use_module(library(distutils)).
:- use_module(library('distutils/distclean')).
:- use_module(engine(system_info), [get_ciao_ext/1, get_exec_ext/1]).

:- use_module(ciaosrc('CIAOSETTINGS')).
:- use_module(ciaosrc('CIAOSHARED')).
:- use_module(ciaodesrc('CIAODESHARED')).

:- function(arith(false)).

plutility := fileinfo | get_deps | pldiff | viewpo | lpmake | cleandirs |
	show_asr | compiler_output | synch_actions.

%dotcshrc     := ~atom_concat(~srcbindir, '/DOTcshrc').
%dotprofile   := ~atom_concat(~srcbindir, '/DOTprofile').
basefile     := ~atom_concat([~srcbindir, '/', ~versionmain]).
ciao_get_arch := 'ciao_get_arch'.

shscript := ~ciao_get_arch | ~basemain.

abs_plutility     := ~atom_concat([~srcbindir, '/', ~plutility,
	'-', ~vers, ~get_ciao_ext]).
abs_ciao_get_arch := ~atom_concat([~srcbindir, '/', ~ciao_get_arch,
	'-', ~vers]).
abs_shscript      := ~atom_concat([~srcbindir, '/', ~shscript,
	'-', ~vers]).

exe_header := ~atom_concat(~ciaosrc,'/lib/compiler/header').

all <- :-
%	dotcshrc(DOTcshrc),
%	dotprofile(DOTprofile),
	del_files_nofail(['DOTcshrc', 'DOTprofile', ~basefile]),
	make([comp_message, plutilities, 'DOTcshrc', 'DOTprofile', shscripts]).

plutilities <- :-
	findall(P, abs_plutility(P), PL),
%	display(PL),
	make(PL).
% 	get_ciao_ext(C),
% 	srcbindir(BI),
% 	findall(P, (plutilit(B), atom_concat([BI, '/', B, C], P)), PL),
% 	make(PL).

shscripts <- :-
	findall(P, abs_shscript(P), PL),
	make(PL).

install <- [] # "Install ciao shell utilities." :-
	ciaobindir(BinDir),
	rpm_build_root(RpmBuildRoot),
	atom_concat(RpmBuildRoot, BinDir, BuildBinDir),
	vers(Vers),
	versionmain(VersionMain),
	srcbindir(SrcBinDir),
	make_dirpath(BuildBinDir),
%	dotcshrc(DOTcshrc),
%	dotprofile(DOTprofile),
	-set_perms(BuildBinDir, ~execmode),
% Copy shell initialization files
	copy_file('DOTprofile', ~atom_concat(RpmBuildRoot, ~reallibdir), [overwrite]),
	-set_perms(~atom_concat([RpmBuildRoot, ~reallibdir,'/DOTprofile']),
	    ~datamode),
	do(['ln -sf ', ~reallibdir, '/DOTprofile ', RpmBuildRoot, ~libdir,
	    '/DOTprofile'], nofail),
	copy_file('DOTcshrc', ~atom_concat(RpmBuildRoot,~reallibdir), [overwrite]),
	do(['ln -sf ', ~reallibdir, '/DOTcshrc ', RpmBuildRoot, ~libdir,
	    '/DOTcshrc'], nofail),
% Copy startup script
	atom_concat([SrcBinDir,   '/', VersionMain], SourceVersionMain),
	atom_concat([BuildBinDir, '/', VersionMain], TargetVersionMain),

	del_file_nofail(TargetVersionMain),
	copy_file(SourceVersionMain, TargetVersionMain, [overwrite]),
	-set_perms(TargetVersionMain, ~execmode),

	del_file_nofail(~atom_concat([BuildBinDir,'/',~basemain])),
	symbolic_link(~versionmain, BuildBinDir, ~basemain),
	(   install_prolog_name(yes) ->
	    del_file_nofail(~atom_concat([BuildBinDir,'/prolog'])),
	    symbolic_link(~versionmain, BuildBinDir, 'prolog')
	;
	    true
	),
	get_ciao_ext(Ext),
	(
	    (
		plutility(BaseFile),
		atom_concat([SrcBinDir,'/',BaseFile,'-',Vers,Ext], SourceFile),
		atom_concat([              BaseFile,'-',Vers,Ext], TargetFile)
	    ;
		shscript(BaseFile),
		atom_concat([SrcBinDir,'/',BaseFile,'-',Vers    ], SourceFile),
		atom_concat([              BaseFile,'-',Vers    ], TargetFile)
	    ),
	    --delete_file(~atom_concat([RpmBuildRoot,BinDir,'/',TargetFile])),
	    --delete_file(~atom_concat([RpmBuildRoot,BinDir,'/',BaseFile])),
%	    -copy_file(SourceFile, ~atom_concat([RpmBuildRoot,BinDir,'/',
%	        TargetFile]), [overwrite]),
	    -copy_file(TargetFile, SrcBinDir, ~atom_concat(RpmBuildRoot,
	        BinDir), [overwrite]),
%	    -copy_file(BaseFile,   SrcBinDir, ~atom_concat(RpmBuildRoot,
%		BinDir), [overwrite]),
	    --delete_file(~atom_concat([BuildBinDir,'/',BaseFile])),
	    symbolic_link(TargetFile, BuildBinDir, BaseFile),
	    -set_perms(~atom_concat([BuildBinDir,'/',BaseFile]),~execmode),
	    -set_perms(~atom_concat([BuildBinDir,'/',TargetFile]),~execmode),
%	    symbolic_link(TargetFile, BinDir, BaseFile),
	    fail
	;
	    true
	).

uninstall <- :-
	justuninstall.

justuninstall :-
	instype(InsType),
	(
	    InsType==src ->
	    true
	;
	    InsType==ins ->
	    douninstall
% 	;
% 	    fail
	).

douninstall :-
	ciaobindir(BinDir),
	rpm_build_root(RpmBuildRoot),
	libdir(LibDir),
	reallibdir(RealLibDir),
	del_files_nofail([
	    ~atom_concat([RpmBuildRoot, LibDir,'/DOTprofile']),
	    ~atom_concat([RpmBuildRoot, LibDir,'/DOTcshrc']),
	    ~atom_concat([RpmBuildRoot, BinDir,'/prolog']),
	    ~atom_concat([RpmBuildRoot, RealLibDir,'/DOTprofile']),
	    ~atom_concat([RpmBuildRoot, RealLibDir,'/DOTcshrc']),
	    ~atom_concat([RpmBuildRoot, BinDir,'/',~basemain]),
	    ~atom_concat([RpmBuildRoot, BinDir,'/',~versionmain]),
	    ~atom_concat([RpmBuildRoot, LibDir,'/NewUser'])]),
	get_ciao_ext(Ext),
	vers(Vers),
	(
	    (
		plutility(BaseFile),
		atom_concat([BaseFile,'-',Vers,Ext],File)
	    ;
		shscript(BaseFile),
		atom_concat([BaseFile,'-',Vers],File)
	    ),
% 	    display('deleting '),display([~atom_concat([BinDir,'/',File]),
% 	    ~atom_concat([BinDir,'/',BaseFile])]),nl,
	    --delete_file(~atom_concat([RpmBuildRoot, BinDir,'/',BaseFile])),
	    --delete_file(~atom_concat([RpmBuildRoot, BinDir,'/',File])),
	    fail
	;
	    true
	),
	(   install_prolog_name(yes) ->
	    --delete_file(~atom_concat([RpmBuildRoot, ~ciaobindir, '/prolog']))
	;
	    true
	).

% Generate shell initialization files
'DOTcshrc' <- ['DOTcshrc.skel']  :: DOTcshrc :-
	replace_strings_in_file([
	    ["documentation_directory", ~atom_codes(~ciaodocdir)],
	    ["binary_directory", ~atom_codes(~ciaobindir)]],
	    'DOTcshrc.skel', DOTcshrc).

'DOTprofile' <- ['DOTprofile.skel'] :: DOTprofile :-
	replace_strings_in_file([
	    ["documentation_directory", ~atom_codes(~ciaodocdir)],
	    ["binary_directory", ~atom_codes(~ciaobindir)]],
	    'DOTprofile.skel', DOTprofile).

% Generate startup script
~basefile <- ['ciao.skel'] :: BaseFile :-
	atom_codes(~get_ciao_ext, Ext),
	replace_strings_in_file([
	    ["<CIAOLIBDIR>", ~atom_codes(~libdir)],
	    ["CIAOVERSION=", ~append("CIAOVERSION=-", ~atom_codes(~vers))],
            ["CIAOSUFFIX=", "CIAOSUFFIX="||Ext],
	    ["<CIAODOCDIR>", ~atom_codes(~ciaodocdir)],
	    ["binary_directory", ~atom_codes(~ciaobindir)]],
	    'ciao.skel', BaseFile),
	del_file_nofail(~atom_concat([~srcbindir,'/',~basemain])),
	symbolic_link(~versionmain, ~srcbindir, ~basemain),
	(   install_prolog_name(yes) ->
	    del_file_nofail(~atom_concat([~srcbindir,'/prolog'])),
	    symbolic_link(~versionmain, ~srcbindir, 'prolog')
	;
	    true
	),
	-set_perms(BaseFile,~execmode).

~abs_ciao_get_arch <- [ciao_get_arch] :: BaseFile :-
	copy_file(ciao_get_arch, BaseFile, [overwrite]),
	del_file_nofail(~atom_concat([~srcbindir,'/ciao_get_arch'])),
	symbolic_link(~atom_concat('ciao_get_arch-',~vers), ~srcbindir,
	'ciao_get_arch').

% Generate initial user message:  It Must be created after NewUser
% I think this must not be created here, even more, the extension is incorrect

% 'NewUser-install' <- ['../NewUser'] :-
% 	replace_strings_in_file([
% 	    ["<LIBROOT>",  ~atom_codes(~ciaolibroot)],
% 	    ["<LPDOCDIR>", ~atom_codes(~ciaodocroot)]],
% 	    ~atom_concat(~ciaosrc,'/NewUser'),
% 	    'NewUser-install').

comp_message <- :-
	display_comment('Compiling utilities in etc directory.').

~abs_plutility <- [~plsource(FileName), ~exe_header] :: FileName :-
	srcbindir(SrcBinDir),
	vers(Vers),
	get_ciao_ext(Ext),
	atom_concat([SrcBinDir, '/', BaseFile, '-', Vers, Ext], FileName),
	atom_concat([BaseFile, '-', Vers, Ext], TargetFile),
	atom_concat(BaseFile, '.pl', SourceFile),
	atom_concat([~ciaosrc,'/etc/', SourceFile], AbsSourceFile),
	catch((
		  make_exec([AbsSourceFile], FileName),
		  del_file_nofail(~atom_concat([SrcBinDir,'/',BaseFile])),
		  symbolic_link(TargetFile, SrcBinDir, BaseFile)
	      ), E,
	(display(E), throw(E))).

cleanetc <- :-
	del_file_nofail(~abs_plutility), fail ; true.

% 	compile_util(FileBase,FileBase).

% compile_util(FileBase,File) :-
% 	get_ciao_ext(Ext),
% 	display('Compiling '),display(File),display('...\n'),
% 	catch(make_exec([FileBase],~atom_concat([File,Ext])), E,
% 	(display(E),throw(E))).
%	do(['ln -sf ', File, Ext, ' ', FileBase], nofail).

% cc  := 'gcc'.

% force_fix_size <- :-
% 	do([~cc, ' -o fix_size fix_size.c'], nofail).

% realclean <- [distclean] :- true.
