:- module(_,_,[make,functions,assertions]).
:- use_module(library('make/system_extra'), [do/2, getenvstr/2,
	add_preffix/3, set_perms/2, rename_file/2, del_file_nofail/1,
	writef/2, writef/3, del_files_nofail/1, ls/3, '-'/1,
	delete_files/1]).
:- use_module(library(distutils), [make_subdir/5, make_subdirs/5,
	delete_dir_rec/1]).
:- use_module(library(lists),[list_concat/2]).
:- use_module(library(terms),[atom_concat/2]).
:- use_module('../CIAOSHARED', [gmake/1,setlocalciao/1,datamode/1,execmode/1,
	lpmake/1]).
:- use_module(library(read)).

:- function(arith(false)).
%Better more this whould be dynamic:

:- use_module('DISTSETTINGS').

:- use_module(library('distutils/distmkf')).
:- initialization(initmakefile).

initmakefile :-
	distmkf:register_config_file(ciaosrc('dist/DISTSETTINGS')),
	distmkf:register_config_file(ciaosrc('CIAOSHARED')).

libsofile :=
	'library/random/random'|
	'library/sockets/sockets_c'|
	'library/persdb_mysql/mysql_client'.

customdistdoc <- :-
%	make_subdir(~lpmake,  ~gmake, '../emacs-mode', ~setlocalciao, 'doc'),
	make_subdir(~lpmake,  ~gmake, '../emacs-mode', ~setlocalciao, 'CiaoMode.pl'),
	make_subdir(~lpmake, ~lpmake, '../doc',        ~setlocalciao, 'braveclean').

distwin32clean <- :-
	distwin32clean(~distsrcwin32).

distwin32clean(D) :-
	bold_message("Configuring ~w distribution tree... (Win32)", D),
	make_subdir(~lpmake, ~gmake, D, '', 'fixpath'),
	make_subdir(~lpmake, ~gmake, ~atom_concat([D, '/doc/reference']),
	    ~setlocalciao, 'distclean'),
	make_subdir(~lpmake, ~gmake, ~atom_concat([D, '/emacs-mode']),
	    ~setlocalciao, 'distclean'),
	make_subdir(~lpmake, ~gmake, D, ~setlocalciao,
	    'cleanbackups engclean'),
% Patch those multiple compilers...
	make_subdir(~lpmake, ~gmake, ~atom_concat([D,    '/ciaoc']),
	    ~setlocalciao, 'cleanexecs'),
	make_subdir(~lpmake, ~gmake, ~atom_concat([D,      '/etc']),
	    ~setlocalciao, 'distclean'),
	make_subdir(~lpmake, ~gmake, ~atom_concat([D, '/examples']),
	    ~setlocalciao, 'distclean'),
	bold_message("Compiling installation script... (Win32)"),
	do(['cd ', D, '/Win32 ; ', D, '/ciaoc/ciaoc wsetup'], nofail),
	-set_perms(~add_preffix(~ls(~atom_concat(D,'/Win32'),'wsetup.*'),
	~atom_concat(D,'/Win32/')),~datamode),
	-set_perms(~atom_concat(D,'/Win32/wsetup'),~execmode),
	bold_message("Cleaning up .o and .so in libs... (Win32)"),
	(   del_file_nofail(~atom_concat(~libsofile, '_*o')),
	    fail
	;
	    true
	),
	(   Cmd = [D,'/ciaoc/ciaoc -c ', D, '/', ~libsofile],
	    do(Cmd, nofail),
	    fail
	;
	    true
	),
	bold_message("Deleting files that distclean would delete... (Win32)"),
	del_file_nofail(~atom_concat(D, '/lib/compiler/browse_builtins')),
	make_subdirs(~lpmake, ~gmake,
	~add_preffix([
			 '/library/class/examples/geometry',
			 '/library/persdb/examples',
			 '/library/actmods/examples',
			 '/library/concurrency/examples'
		     ],D),
		     ~setlocalciao, 'distclean'),
	bold_message("Putting correct suffix to Ciao executables... (Win32) "),
	do(['cd ', D, '; find etc Win32 ciaoc shell ! -name ''*.*'' !',
	' -type d -perm +u+x -exec mv {} {}.cpx \\;'], nofail),
	bold_message("Deleting non-useful files for Win distribution... (Win32) "),
	del_files_nofail(~add_preffix([
					  '/INSTALLATION',
					  '/Makefile',
					  '/SETTINGS',
					  '/SHARED',
					  '/NewUser',
					  '/makefile-sysindep'], D)),
	rename_file(~atom_concat(D,'/INSTALLATION-Win32'),
	~atom_concat(D, '/INSTALLATION')),
	delete_dir_rec(~atom_concat(D, '/makefile-sysdep')),
	bold_message("Changing text files to Win newlines... (Win32)"),
	unix2dos(~add_preffix(['/install.bat', '/client.bat'], D),''),
	unix2dos(~add_preffix(['/README', '/INSTALLATION',
	'/COPYRIGHT'], D), '.txt').

unix2dos([],_).
unix2dos([F|Fs], Suffix) :-
	unix2dos(F,Suffix),!,
	unix2dos(Fs,Suffix).

unix2dos(File, Suffix) :-
	atom_concat(File, '.tmp', File_tmp),
	atom_concat(File, Suffix, Target),
	rename_file(File, File_tmp),
	do(['awk \'{print $$0 \"\\r\"}\' ', File_tmp, ' > ', Target], nofail),
	set_perms(Target, ~datamode),
	del_file_nofail(File_tmp).
