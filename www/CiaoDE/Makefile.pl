:- module(_,_,[dcg,functions,make,assertions]).

:- use_module(library(compiler), [use_module/1,unload/1]).
:- use_module(library(distutils)).
:- use_module(library(lists)).
:- use_module(library('make/system_extra')).
:- use_module(library(system)).
%:- use_module(library(terms)).
:- use_module(library(patterns)).
:- use_module(library(messages)).
:- use_module(library(file_utils)).
:- use_module(engine(system_info), [get_ciao_ext/1, get_exec_ext/1]).
:- use_module(library('distutils/distclean')).
:- use_module(ciaodesrc('CIAODESETTINGS')).
:- use_module(ciaosrc('CIAOSHARED')).
:- use_module(library(aggregates)).

:- function(arith(false)).

comment(module, "

@section{Main CiaoDE Makefile}

bootclean <- realclean + Remove the static lpmake and engine.  Note
             that if you do it, you must  execute the ./configure script again.
").

all <- [allciao, allextra] :- true.

allciao <- :-
	make_subdir(~lpmake, ~gmake, 'ciao', ~setlocalciao, all).

allextra <- :-
	make_subdirs(~lpmake, ~lpmake, ~findall(P, extracomponent(P)),
	    ~setlocalciao, all).

docs <- [docsciaode] :-
	make_subdirs(~lpmake, ~lpmake, ~findall(P, component(P)),
	    ~setlocalciao, docs).

docsciaode <- :-
	do(['./configure --help > docsrc/common/configure_help.tmp'], fail),
	make_subdir(~lpmake, ~lpmake, 'docsrc/readmes', ~setlocalciao,
	    '-l distutils/readmemkf -m RSETTINGS realclean all').

install <- [justinstall, reconfigure] :- true.

installdoc <- :-
	make_subdirs(~lpmake, ~lpmake, ~findall(P, component(P)),
	    ~setlocalciao, 'installdoc').

justinstall <- :-
	make(~atom_concat('justinstall_', ~instype)).

justinstall_src <- :-
	note($$(
	    "Skipping copy of library files (in-source installation) ...")),
	docdir(DocDir),
	make_dirpath(DocDir),
	string_to_file("", ~atom_concat(DocDir, '/NODISTRIBUTE')),
	make(installdoc).

justinstall_ins <- :-
	make_subdir(~lpmake, ~gmake, 'ciao', ~setlocalciao, justinstall),
	make_subdirs(~lpmake, ~lpmake, ~findall(P, extracomponent(P)),
	    ~setlocalciao, justinstall).

uninstall <- :-
	make([~atom_concat('uninstall_', ~instype)]).

uninstall_src <- :-
	note($$(
	    "Skipping removal of library files (in-source installation) ...")),
	make(unreconfigure).

uninstall_ins <- :-
	make_subdirs(~lpmake, ~lpmake, ~findall(P, extracomponent(P)),
	    ~setlocalciao, uninstall),
	make(uninstallciao).

uninstallciao <- :-
	make_subdir(~lpmake, ~gmake, 'ciao', ~setlocalciao, uninstall).

unreconfigure <- :-
	make_subdirs(~lpmake, ~lpmake, ~findall(P, extracomponent(P)),
	    ~setlocalciao, unreconfigure),
	make_subdir(~lpmake, ~lpmake, 'ciao', ~setlocalciao, unreconfigure).

reconfigure <- :-
	make_subdir(~lpmake, ~lpmake, 'ciao', ~setlocalciao, reconfigure),
	make_subdirs(~lpmake, ~lpmake, ~findall(P, extracomponent(P)),
	    ~setlocalciao, reconfigure),
	bold_message(
    "Your initialization files have been modified for Ciao execution.\n"||
    "You must make sure they are re-read (by, e.g., logging out and\n"||
    "back into your system) before using any Ciao component.").

bootclean <- :-
	do(['rm -rf bin'], fail).

%	delete_dir_rec('bin').

test_tty <- :-
	return_code(A),
	(
	    A=="\\r" ->
	    display('\\r\n')
	;
	    display('\\n\n')
	).

license_file := ~atom_codes(~winpath(relative, ~atom_concat(~ciaodesrc,
	'/LGPL'))).

source_dir := ~atom_codes(~winpath(relative, ~ciaodesrc)).
output_dir := ~atom_codes(~winpath(relative, ~atom_concat(~ciaodesrc,
	'/dist'))).

get_files(Files) :-
	string_to_file("", 'ciao/doc/NODISTRIBUTE'),
	string_to_file("", 'ciao/engine/NODISTRIBUTE'),
	string_to_file("", 'lpdoc/doc/NODISTRIBUTE'),
	string_to_file("", 'ciaopp/doc/NODISTRIBUTE'),
	string_to_file("", 'docsrc/NODISTRIBUTE'),
	del_files_nofail([
			     'doc/NODISTRIBUTE',
			     'bin/NODISTRIBUTE'
			 ]),
	list_bin_distribution('./','', List),
	list_to_string(List, Files, Extras),
	extra_files(Extras, []),
	string_to_file("", 'doc/NODISTRIBUTE'),
	string_to_file("", 'bin/NODISTRIBUTE'),
	del_files_nofail([
	    'ciao/doc/NODISTRIBUTE',
	    'ciao/engine/NODISTRIBUTE',
	    'lpdoc/doc/NODISTRIBUTE',
	    'ciaopp/doc/NODISTRIBUTE',
	    'docsrc/NODISTRIBUTE'
			 ]).

ciao_engine_posix_bin_dir := ~atom_concat(['ciao/bin/', ~get_os, ~get_arch]).
ciao_engine_bin_dir := ~winpath(relative, ~ciao_engine_posix_bin_dir).
ciao_engine_bin_dir_c := ~atom_codes(~ciao_engine_bin_dir).

extra_files -->
	{ list_concat([~ciao_engine_bin_dir_c, "\\"], CiaoEngineBinDirC) },
	file_entry_iss(
	    "ciao\\doc\\common\\CiaoHead.info",
	    "ciao\\doc\\common\\"),
	file_entry_iss(
	    "ciao\\doc\\common\\CiaoTail.info",
	    "ciao\\doc\\common\\"),
	file_entry_iss(~list_concat([ CiaoEngineBinDirC, "ciaoengine.exe"]),
	    CiaoEngineBinDirC),
	file_entry_iss(~winpathc('/usr/bin/sh.exe'),      CiaoEngineBinDirC),
	file_entry_iss(~winpathc('/usr/bin/cygwin1.dll'), CiaoEngineBinDirC).

file_entry_iss(Source, DestDir) -->
	"Source: ", dcg_append(Source),
	"; DestDir: {app}\\", dcg_append(DestDir), "\n".

list_to_string([]) -->
	"".
list_to_string([File|Files]) -->
	file_entry_iss(~fullwinname(File), ~fullwinpath(File)),
	list_to_string(Files).

winpathc(A) := ~atom_codes(~winpath(relative,A)).

fullwinname(File, WinName) :-
	atom_concat('/',BaseFile, File),
	winpathc(BaseFile, WinName).

fullwinpath(File, WinPath) :-
	extract_unix_filepath(File, Path),
	atom_concat('/',BasePath, Path),
	winpathc(BasePath, WinPath).

discompose_win_filename_string(FullName, Dir, Name, Extension) :-
	discompose_filename_string(FullName, 0'\\, Dir, Name, Extension).

discompose_unix_filename_string(FullName, Dir, Name, Extension) :-
	discompose_filename_string(FullName, 0'/,  Dir, Name, Extension).

discompose_filename_string(FullName, PathSeparator, Dir, Name, Extension) :-
	list_concat([Main, [PathSeparator | SubPath]], FullName),
	!,
	discompose_filename2_string(Main, PathSeparator, Dir, SubPath, Name, Extension).
discompose_filename_string(FullName, PathSeparator, "",  Name, Extension) :-
	list_concat([Name, [PathSeparator | Extension]], FullName),
	!.
discompose_filename_string(Name,     _,             "",  Name, "").

discompose_filename2_string(Main, PathSeparator, Dir, SubPath, Name, Extension) :-
	discompose_filename_string(SubPath, PathSeparator, Dir2, Name, Extension),
	list_concat([Main, [PathSeparator | Dir2]], Dir).

extract_win_filepath(FullName, Path) :-
	extract_filepath(FullName, 0'\\, Path).

extract_unix_filepath(FullName, Path) :-
	extract_filepath(FullName, 0'/, Path).

extract_filepath(FullName, PathSeparator, Path) :-
	discompose_filename_string(~atom_codes(FullName), PathSeparator,
	    PathC, _, _),
	atom_codes(Path, PathC).

dcg_append(A,B,C) :- append(A,C,B).

:- use_module(library('compiler/exemaker'),[make_exec/2]).

'ciao/Win32/wsetup.cpx' <- ['ciao/Win32/wsetup.pl'] :-
	make_exec(['ciao/Win32/wsetup.pl'], 'ciao/Win32/wsetup.cpx').

'ciaode_Win32.iss' <- ['etc/ciaode_Win32.iss.skel'
		      ] :-
	ciaode_Win32_iss.

default_dir_name := ~atom_codes(~packname(~wholesystem)).

ciaode_Win32_iss :-
	atom_codes(~componentversion(~wholesystem), ComponentVersion),
	ciaode_revision_string(S),
	list_concat([ComponentVersion, " (r" || S, ")"], Version),
	replace_strings_in_file([
	    ["<MyAppName>",          "Ciao Development Environment"],
	    ["<MyAppVerName>",       "Ciao Development Environment " ||
	                             Version],
	    ["<OutputBaseFileName>", ~atom_codes(~version(~wholesystem))],
	    ["<MyAppPublisher>",     "The CLIP Laboratory"],
	    ["<LicenseFile>",        ~license_file],
	    ["<MyAppExeName>",       ~atom_codes(~atom_concat(['ciaosh-',
	                             ~vers, ~get_ciao_ext]))],
	    ["<SourceDir>",          ~source_dir],
	    ["<OutputDir>",          ~output_dir],
	    ["<DefaultDirName>",     ~default_dir_name],
	    ["<CiaoEngineBinDir>",   ~ciao_engine_bin_dir_c],
	    ["<Files>",              ~get_files]
	 ],
	'etc/ciaode_Win32.iss.skel',   'ciaode_Win32.iss').

iscc := '/cygdrive/c/Program\\ Files/Inno\\ Setup\\ 5/iscc.exe'.

installer_Win32 <- ['ciaode_Win32.iss'
	,'ciao/Win32/wsetup.cpx'
		   ]:-
	installer_Win32.

installer_Win32 :-
	note('Creating windows installer, please be patient...\n'),
	do([~iscc, ' ciaode_Win32.iss'], nofail).
