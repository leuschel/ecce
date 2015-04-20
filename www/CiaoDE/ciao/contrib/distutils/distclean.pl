:- module(_, _, [make, functions, assertions]).

:- use_module(library(distutils)).
:- use_module(library(system)).
:- use_module(library('make/system_extra')).
:- use_module(library(compiler)).
:- use_module(library(patterns)).

comment(module, "

@section{cleanning routines}

Have in mind the next definitions:

clean     <- remove auto generated temporary and process files.

rawclean  <- clean + remove files which must not be distributed in a
             raw distribution.  A raw distribution is a precompiled
             (almost ready to use) distribution.

distclean <- rawclean + remove compiled files, including texic files,
	     but preserve documentation.

realclean <- distclean + remove all auto generated files, including
             documentation.

").

clean <- [] # "Remove auto generated temporary files." :-
	recursive_clean('.').

rawclean  <- [] #
   "clean + remove files which must not be distributed in a
    raw distribution.  A raw distribution is a precompiled
    (almost ready to use) distribution."
        :-
	recursive_rawclean('.').

% Specific files that must be deleted in order to clean CiaoDE

clean_specific := 'ciao/SETTINGS' | 'ciao/SETTINGS_AUTO'.

distclean <- :-
	display('Cleaning readmes...\n'),
	recursive_readmes_distclean,
	display('Recursive distclean...\n'),
	recursive_distclean('.'),
	(
	    --delete_file(~clean_specific),
	    fail
	;
	    true
	).

realclean <- :-
	display('Cleaning readmes...\n'),
	recursive_readmes_realclean,
	display('Cleaning lpdoc...\n'),
	recursive_lpdoc_docsclean,
	display('Recursive realclean...\n'),
	recursive_realclean('.').

recursive_readmes_distclean <- :-
	recursive_readmes_distclean.

recursive_readmes_distclean :-
	find('.', recursive_readmes_file, find_dummy_fail_,
	    recursive_readmes_dir, find_dummy_,
	    params(~nodist_dirs, ~skip_dirs, distclean), _, []).

recursive_readmes_realclean <- :-
	recursive_readmes_realclean.

recursive_readmes_realclean :-
	find('.', recursive_readmes_file, find_dummy_fail_,
	    recursive_readmes_dir, find_dummy_,
	    params(~nodist_dirs, ~skip_dirs, realclean), _, []).

recursive_readmes_file(_BaseDir, _File, _) :-
	fail.

recursive_readmes_dir(BaseDir, Dir, params(NDFiles, SkipDirs, CleanType)) :-
	is_distributable_dir(BaseDir, Dir, NDFiles, SkipDirs),
	working_directory(W,W),
	cd(~atom_concat([BaseDir,'/', Dir])),
	(   file_exists('RSETTINGS.pl'), file_exists('LPSETTINGS.pl') ->
	    (
		display_list(['Cleanning ', BaseDir, '/', Dir, '...\n']),
		-use_module('LPSETTINGS'),
% 		(   _:mainfile(MainFile) ->
% %		    display_list(['docsclean(', MainFile, ')\n'])
% 		    docsclean(MainFile)
% 		;   true
% 		),
		(   CleanType==realclean ->
		    --use_module('RSETTINGS'),
		    _:readmetarget(R),
		    (
			_:component(C),
%			display(~atom_concat([R,'/',C])),nl,
			--delete_file(~atom_concat([R,'/',C])),
			fail
		    ;   true
		    ),
		    --unload('RSETTINGS')
		;   true
		),
		(   _:component(C),
%		    display(~atom_concat(C, '.pl\n')),
		    del_file_nofail(~atom_concat(C, '.pl')),
		    del_file_nofail(~atom_concat(C, '.ascii')),
		    fail
		;   true
		),
%		--unload('LPDOCCOMMON.pl'),
		--unload('LPSETTINGS') ->
		cd(W),
		fail
	    ;   true
	    )
	;   true
	),
	cd(W).


recursive_skel_ext_distclean :-
	recursive_ext_distclean('.skel', ['']).

recursive_ext_distclean(SourceExt, OutputExts) :-
	find('.', recursive_ext_file, find_dummy_fail_,
	    recursive_ext_dir, find_dummy_,
	    params(SourceExt, OutputExts, ['Makefile.pl', 'LPSETTINGS.pl'],
	    ~nodist_dirs, ~skip_dirs), _, []).

recursive_ext_file(BaseDir, File, params(SourceExt, OutputExts, SkipFiles,
	    _NDFiles, _SkipDirs)) :-
        \+ ( member(File, SkipFiles) ),
	atom_concat(FileBase, SourceExt, File),
	(
	    member(OutputExt, OutputExts),
	    atom_concat([BaseDir, '/', FileBase, OutputExt], OutputFileName),
	    file_exists(OutputFileName),
	    -file_property(OutputFileName, type(regular)),
	    -del_file_nofail(OutputFileName),
%	    display(OutputFileName),nl,
	    fail
        ;   true
        ),
	!,
	fail.

recursive_ext_dir(BaseDir, Dir, params(_, _, _, NDFiles, SkipDirs)) :-
	is_distributable_dir(BaseDir, Dir, NDFiles, SkipDirs).

% ----------------------------------------------------------------------------
recursive_lpdoc_docsclean <- :-
	recursive_lpdoc_docsclean.

recursive_lpdoc_docsclean :-
	find('.', recursive_lpdoc_file, find_dummy_fail_, recursive_lpdoc_dir,
	    find_dummy_, params(~nodist_dirs, ~skip_dirs), _, []).

recursive_lpdoc_file(_BaseDir, _File, _) :- fail.

recursive_lpdoc_dir(BaseDir, Dir, params(NDFiles, SkipDirs)) :-
	is_distributable_dir(BaseDir, Dir, NDFiles, SkipDirs),
	working_directory(W,W),
	cd(~atom_concat([BaseDir,'/', Dir])),
	(   file_exists('LPSETTINGS.pl'), \+ ( file_exists('RSETTINGS.pl') ) ->
	    (
		display_list(['Cleanning ', BaseDir, '/', Dir, '...\n']),
		--use_module('LPSETTINGS.pl'),
		(   _:mainfile(MainFile) ->
%		    display_list(['docsclean(', MainFile, ')\n'])
		    docsclean(MainFile)
		;   true
		),
		--unload('LPSETTINGS.pl') ->
		cd(W),
		fail
	    ;   true
	    )
	;   true
	),
	cd(W).

% ----------------------------------------------------------------------------
recursive_clean(BaseDir) :-
	recursive_generic_clean(BaseDir, params(~skip_raw_files, ~nodist_dirs,
	    ~skip_dirs)).

recursive_rawclean(BaseDir) :-
	recursive_generic_clean(BaseDir, params(~skip_raw_files, ~nodist_dirs,
	    ~skip_dirs)).

recursive_distclean(BaseDir) :-
	recursive_generic_clean(BaseDir, params(~skip_dist_files, ~nodist_dirs,
	    ~skip_dirs)).

recursive_realclean(BaseDir) :-
	recursive_generic_clean(BaseDir, params(~skip_dist_files, ~nodist_dirs,
	    ~skip_dirs)).

recursive_generic_clean(BaseDir, params(Pattern, NDFiles, SkipDirs)) :-
	find(BaseDir, recursive_clean_file, find_dummy_fail_,
	    recursive_clean_dir, find_dummy_,
	    params(Pattern, NDFiles, SkipDirs), _, []).

recursive_clean_file(BaseDir, File, params(Pattern, _NDFiles, _SkipDirs)) :-
	match_pattern_pred(Pattern, File),
	atom_concat([BaseDir, '/', File], FileName),
	--del_file_nofail(FileName),
%	display_list([FileName,'\n']),
	!,
	% fail to avoid the File be added to the list
	fail.

% The method will not clean directories marked with NODISTRIBUTE, or
% which match with skip_dirs pattern

recursive_clean_dir(BaseDir, Dir, params(_Pattern, NDFiles, SkipDirs)) :-
	is_distributable_dir(BaseDir, Dir, NDFiles, SkipDirs).
