:- module(distutils,
	[
	    copy_src_distribution/2,
	    copy_src_distribution/3,
	    copy_raw_distribution/2,
	    copy_raw_distribution/3,
	    delete_dir_rec/1,
	    delete_dir_rec_list/1,
	    docsclean/1,
	    specific_docclean/2,
	    additional_docclean/1,
	    is_distributable_dir/4,
	    list_files_rec/2,
	    list_files_rec/3,
	    list_dirs_rec/2,
	    list_dirs_rec/3,
	    list_src_distribution/3,
	    list_bin_distribution/3,
	    list_files_filter_rec/8,
	    list_files_filter_rec/9,
	    list_filter_files_rec/7,
	    list_filter_files_rec_dest/7,
	    cvs_export/3,
	    write_list/2,
	    clean_html_dir/1,
	    display_comment/1,
	    make_subdir/5,
	    make_subdirs/5,
	    extract_strings/3,
	    absolute_dir_name/2,
	    copy_dir_rec/2,
	    copy_dir_rec/6,
	    copy_dir_rec/7,
	    isadmin/0,
	    usersrc/1,
	    get_backup_filename/2,
	    backup/1,
	    secure_write/2,
	    pwd/1,
	    skip_dirs/1,
	    skip_clean_files/1,
	    skip_dist_files/1,
	    skip_dist_files_bak/1,
	    skip_raw_files/1,
	    nodist_dirs/1,
	    collect_modules/2,
	    compile_collected_modules/7,
	    dump_collect_modules/4,
	    find/8,
	    find_dummy_/3,
	    find_dummy_fail_/3,
	    autolibs/5,
	    autolibs/6,
	    configure_script/3,
	    uninstall_script/2,
	    start_message/4,
	    end_message/4,
	    atom_concat/2,
	    atom_list_to_string/3,
	    flat_list/2,
	    string_to_term/2,
	    strings_to_terms/2,
	    return_code/1,

	    atomdelim/3,
	    atomdelim_list/3,


% deprecated:
	    recurse_directory/7

	],
	[runtime_ops,
	 dcg,
	 regtypes,
	 assertions,
	 basicmodes,
	 make,
	 functions,
	 hiord
	]).

:- use_package(library(assertions)).

:- use_module(library('make/system_extra'), [do/2, directory_files/2, (-)/1,
   (--)/1, writef/2, set_perms/2, readf/2, getenvstr/2,del_file_nofail/1,
   ls/2, ls/3, del_files_nofail/1, replace_strings_in_file/3, enter_exit/3,
   add_preffix/3]).

:- use_module(library(system), [delete_file/1, delete_directory/1,
   replace_characters/4, file_property/2, file_exists/1,
   make_dirpath/1, make_directory/1, popen/3, copy_file/3,
   copy_file/2, current_env/2, working_directory/2, time/1]).

:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(messages)).
:- use_module(library(patterns)).
:- use_module(library(file_utils)).
:- use_module(library(filenames),[no_path_file_name/2]).
:- use_module(library(lists),[list_concat/2, length/2, append/3]).
:- use_module(library('make/make_rt')).
:- use_module(library(errhandle)).
:- use_module(library(persvalue)).
:- use_module(library(format)).

:- reexport(library('distutils/configutils')).

:- function(arith(false)).

% -------------------------------------------------------------------------
% Task list:

% 2004-03-14:
% Better organized Web Page:

% Current versions must be displayed in a clean main page, while old
% versions in a separate one.

% 2004-02-24:
% Add this app to the cvs repository

% Lets to generate the distribution of this package in a well defined
% format, This implies that all works well to this app itself.
% -------------------------------------------------------------------------

%:- use_module(library(terms),[atom_concat/2]).


:- comment(atom_concat(List, Atom), "Unifies Atom with the
   concatenation of @var{List}.  It also works even if @var{List}
   contains free variables.  Examples:

?- atom_concat(['/home/edison/svn/dist/CiaoDE/bin',
   '/',X,'-','1.11',''],
   '/home/edison/svn/dist/CiaoDE/bin/fileinfo-1.11').

X = fileinfo ? 

yes
?- 

?- atom_concat([a,b,c],A).

A = abc ? ;

no
?- atom_concat([a,B|C],abcde), display([B, C]),nl,fail.
[,[b,c,d,e]]
[,[b,c,de]]
[,[b,cd,e]]
[,[b,cde]]
[,[bc,d,e]]
[,[bc,de]]
[,[bcd,e]]
[,[bcde]]
[b,[c,d,e]]
[b,[c,de]]
[b,[cd,e]]
[b,[cde]]
[bc,[d,e]]
[bc,[de]]
[bcd,[e]]
[bcde,[]]

no
?- 

?- atom_concat([A,b,C],abc).

A = a,
C = c ? ;

no
?- 

?- atom_concat([A,b,''],ab).

A = a ? ;

no

").

% atom_concat( [], '').
% atom_concat( [A|As], Atom) :-
% 	atom(Atom),
% 	!,
% 	atom_concat(A, After, Atom),
% 	atom_concat_(As, After).
% atom_concat( [A|As], Atom) :-
% 	atom_concat_(As, After),
% 	atom_concat(A, After, Atom).

atom_concat([], '').
atom_concat(L, Atom) :-
	var(L),
	!,
	L = [A|As],
	atom_concat(A, Atom0, Atom),
	A \== '',
	atom_concat(As, Atom0).
atom_concat([A|As], Atom) :-
	atom(Atom),
	!,
	atom_concat(A, Atom0, Atom),
	atom_concat(As, Atom0).
atom_concat([A|As], Atom) :-
	atom_concat(As, Atom0),
	atom_concat(A, Atom0, Atom).

string_to_term(String, Term) :-
	pipe( ReadFrom , WriteTo ),
	format(WriteTo,"~s.",[String]),
	close(WriteTo),
	read_term(ReadFrom,Term,[]),
	close(ReadFrom).

strings_to_terms([],    []).
strings_to_terms([S|Ss],[T|Ts]) :-
	string_to_term(S, T),
	strings_to_terms(Ss, Ts).

flat_list(List, FlatList) :-
	flat_list_(List, FlatList, []),
	!.

flat_list_([], T, T).
flat_list_([X|Y], Z, T) :-
	flat_list_(X, Z, P),
	flat_list_(Y, P, T).
flat_list_([X|Y],[X|Z],T) :-
	flat_list_(Y, Z, T).

shortstring(MaxLength, Preffix, Input, Output) :-
	length(Input, N),
	length(Preffix, P),
	(   N > MaxLength + P ->
	    Cut is N - MaxLength,
	    cutstring(Cut, Input, Output1),
	    append(Preffix, Output1, Output)
	;
	    Output = Input
	).

cutstring(0, S, S).
cutstring(N, [_|Input], Output) :-
	N2 is N - 1,
	cutstring(N2, Input, Output).

display_comment(TextList) :-
	list(TextList),!,
	bold_message(~atom_concat(TextList)).

display_comment(Text) :-
	bold_message(~atom_codes(Text)).

%pwd := ~current_env('PWD').

pwd(A) :- working_directory(A,A).

make_subdir(MakeFrom, MakeTo, Dir, PreParams, Action) :-
	(
	    pwd(PWD) ->
	    shortstring(30, "...", ~atom_codes(PWD), PWDDisplay)
	;
	    message(warning , ['The enviroment variable PWD was not found!']),
	    PWDDisplay = '??'
%	    PWD = '??'
	),
	(   MakeFrom = [_, MakeFromDisplay] -> true
%	    atom_concat(MakeFrom, MakeFromFile)
	;
%	    MakeFromFile = MakeFrom,
	    MakeFromDisplay = MakeFrom
	),
	(   MakeTo = [_, MakeToDisplay] ->
	    atom_concat(MakeTo, MakeToFile)
	;
	    MakeToFile = MakeTo,
	    MakeToDisplay = MakeTo
	),
	shortstring(30, "...", ~atom_codes(Dir), DirDisplay),
	format("~w -> ~w: Entering `~s/~s`\n",[MakeFromDisplay, MakeToDisplay,
	PWDDisplay, DirDisplay]),
	atom_concat([PreParams, ' ', MakeToFile, ' ', Action], Command),
	command_subdir(Dir, Command),
	format("~w <- ~w: Leaving  `~s/~s`\n",[MakeFromDisplay, MakeToDisplay,
	PWDDisplay, DirDisplay]).

make_subdirs(From , To , Dirs , PreParams, Action ) :-
	make_subdirs_(Dirs ,From , To , PreParams, Action ).

make_subdirs_( []        , _MakeFrom, _MakeTo, _PreParams, _Action).
make_subdirs_( [Dir|Dirs],  MakeFrom,  MakeTo,  PreParams,  Action) :-
	make_subdir(   MakeFrom, MakeTo  , Dir   , PreParams, Action ),
	make_subdirs_( Dirs    , MakeFrom, MakeTo, PreParams, Action ).

command_subdir(Dir, Command) :-
	display(~atom_concat([Dir, ': Executing `', Command, '`\n'])),
	do(['cd ', Dir, ' && ', Command], fail).

normalize_path(Dir, Dir2) :- atom_concat(Dir2,'/', Dir),!.
normalize_path(Dir, Dir).
% :- atom_concat( Dir,'/',Dir2),!.

absolute_dir_name(Dir, AbsDir) :-
	absolute_file_name('', '', '', Dir, _, _, AbsDir).

% absolute_dir_name(Dir, DirN) :-
% 	atom_concat(AbsDir,'/',AbsDir),!.
% absolute_dir_name(Dir, Dir).

:- push_prolog_flag(multi_arity_warnings, off).

list_files_rec(BaseDir, BaseList) :-
	find(BaseDir, find_dummy_, find_dummy_fail_,
	    find_dummy_, find_dummy_, _, BaseList, []).

list_files_rec(BaseDir, DestDir, DestList) :-
	list_files_rec(BaseDir, BaseList),
	add_preffix(List, BaseDir, BaseList),
	add_preffix(List, DestDir, DestList).

list_dirs_rec(BaseDir, BaseList) :-
	find(BaseDir, find_dummy_fail_, find_dummy_,
	    find_dummy_, find_dummy_, _, BaseList, []).

list_dirs_rec(BaseDir, DestDir, DestList) :-
	list_dirs_rec(BaseDir, BaseList),
	add_preffix(List, BaseDir, BaseList),
	add_preffix(List, DestDir, DestList).

write_list(_Stream, []).
write_list( Stream, [L|Ls]) :-
 	write(Stream, L),
	write(Stream, '\n'),
 	write_list(Stream, Ls).

:- comment(bug, "Change match_pattern_pred by match_pattern, to use
   strings instead atoms").

% ----------------------------------------------------------------------------
% Auto generated temporary and process files
skip_clean_only  := '*.out|*.aux|*.log||*.tmp|tmpciao*'.
skip_clean_files := ~skip_clean_only.

% Files which must not be distributed in a raw/binary distribution
skip_bak_only       := '*.bak|*.Bak|*.BAK|*.old|*.Old|*.OLD|*.gz|*.bz2|*.tgz|*.tbz|*.zip|*.tar|*~'.
skip_raw_only       := '*_opt.pl|*_co.pl|*.iss'.
skip_pure_only      := '*.texic|*.refs|auto*.eps|auto*.txt|auto*.ppm|*.src|*_blue.*|*refs.el|*refs.aux|*refs.blg'.
skip_raw_files      := ~atom_concat([~skip_clean_only, '|', ~skip_raw_only, '|' , ~skip_pure_only]).
skip_raw_files_bak  := ~atom_concat([~skip_bak_only, '|', ~skip_raw_files]).

% Files which can be auto generated, and must not be distributed in a
% source distribution
skip_dist_only      := ~atom_concat([
    '*.po|*.itf|*.dep|*.asr|*.ast|*.ass|*.o|*.so|*.dll|*_glue.c|*_inline.c|',
    '*_auto.pl|path_setup.pl|auto_compile_options.pl|',
    'path_init.pl|ciaopp_exe.pl|icon_address.pl|selected_ppl_interface.pl|',
    'calistep_bench.pl|calistep_cost_auto.pl|calistep_measure.pl'
				    ]).
skip_dist_files     := ~atom_concat([~skip_raw_files, '|', ~skip_dist_only]).
skip_dist_files_bak := ~atom_concat([~skip_bak_only, '|', ~skip_dist_files]).

% Files that do not needed to be cleaned, and will not be distributed
skip_dirs   := 'bak|*.bak|*.Bak|*.BAK|CVS|.svn'.

% File name used as flags to indicate if a directory will not be distributed
nodist_dirs := ['NODISTRIBUTE', '.NODISTRIBUTE', '.nodistribute'].

% ----------------------------------------------------------------------------

list_bin_distribution(BaseDir, DestDir, List) :-
	list_filter_files_rec_dest(BaseDir, DestDir, '*',
	    ~skip_raw_files_bak, ~skip_dirs, ~nodist_dirs, List).

list_src_distribution(BaseDir, DestDir, List) :-
	list_filter_files_rec_dest(BaseDir, DestDir, '*',
	    ~skip_dist_files_bak, ~skip_dirs, ~nodist_dirs, List).

list_filter_files_rec_dest(BaseDir, DestDir, Pattern, SkipFiles, SkipDirs,
	    NDFiles, DestList) :-
	list_filter_files_rec(BaseDir, Pattern, SkipFiles, SkipDirs,
	    NDFiles, BaseList, []),
	add_preffix(List, BaseDir, BaseList),
	add_preffix(List, DestDir, DestList).

list_filter_files_rec(BaseDir, Pattern, SkipFiles, SkipDirs,
	NDFiles, Result, Tail) :-
	find(BaseDir, list_filter_file_, find_dummy_fail_,
	    list_filter_dir_condition_, find_dummy_,
	    param(Pattern, SkipFiles, SkipDirs, NDFiles), Result, Tail).

list_filter_file_(BaseDir, File, param(Pattern, SkipFiles,
	        _SkipDirs, _NDFiles)) :-
	atom_concat([BaseDir, '/', File], FileName),
	\+(file_property(FileName, linkto(_))),
	\+( match_pattern_pred(SkipFiles, File) ),
	match_pattern_pred(Pattern, File),
	!.

list_filter_dir_condition_(CurrBaseDir, Dir, param(_Pattern, _SkipFiles,
	        SkipDirs, NDFiles)) :-
	is_distributable_dir(CurrBaseDir, Dir, NDFiles, SkipDirs).

% ----------------------------------------------------------------------------

copy_dir_rec(BaseDir, DestDir, Pattern, PatternExcludeFile, SkipDirs,
	NDFiles) :-
	make_dirpath(DestDir),
	find(BaseDir, copy_file_, find_dummy_fail_, copy_dir_condition_,
	    find_dummy_, param(BaseDir, DestDir, Pattern, PatternExcludeFile,
	    SkipDirs, NDFiles), _Result, _Tail).

copy_file_(CurrBaseDir, File, param(BaseDir, DestDir, Pattern, SkipFiles,
	        _SkipDirs, _NDFiles)) :-
	(
	    match_pattern_pred(SkipFiles, File) ->
	    fail
	;
	    match_pattern_pred(Pattern, File) ->
	    atom_concat([BaseDir, RelDir], CurrBaseDir),
	    atom_concat([DestDir, RelDir], CurrTargetDir),
	    atom_concat([CurrBaseDir, '/', File], FileName),
	    atom_concat([CurrTargetDir, '/', File], CurrTargetFile),
	    ( catch(
		copy_file(FileName, CurrTargetFile, [overwrite,timestamp]),
		error(Error, Where),
		(   display('{WARNING: An exception was catched: '),
		    (handle_error(Error,Where);true),display('}\n'))
		) -> true ; true )
	;
	    true
	),
	!,
	% fail to avoid the File be added to the list
	fail.

copy_dir_condition_(CurrBaseDir, Dir, param(BaseDir, DestDir, _Pattern, _SkipFiles,
	        SkipDirs, NDFiles)) :-
	is_distributable_dir(CurrBaseDir, Dir, NDFiles, SkipDirs),
	atom_concat([BaseDir, RelDir], CurrBaseDir),
	atom_concat([DestDir, RelDir], TargetBaseDir),
	atom_concat([TargetBaseDir, '/', Dir], TargetDir),
	make_dirpath(TargetDir).

is_distributable_dir(BaseDir, Dir, NDFiles, SkipDirs) :-
	member(NDFile, NDFiles),
	atom_concat([BaseDir,'/',Dir,'/',NDFile], NDFileName),
	file_exists(NDFileName) ->
	fail
    ;
	\+ ( match_pattern_pred(SkipDirs, Dir) ).

copy_src_distribution(BaseDir, DestDir, Verbose) :-
	copy_dir_rec(BaseDir, DestDir, '*',
	~skip_dist_files, ~skip_dirs, ~nodist_dirs, Verbose).

copy_src_distribution(BaseDir, DestDir) :- 
	copy_src_distribution(BaseDir, DestDir, 1).

% like src... but also copy the .po .itf .asr .dep files
copy_raw_distribution(BaseDir, DestDir, Verbose) :-
	copy_dir_rec(BaseDir, DestDir, '*',
	~skip_raw_files, ~skip_dirs, ~nodist_dirs, Verbose).

copy_raw_distribution(BaseDir, DestDir) :-
	copy_raw_distribution(BaseDir, DestDir, 1).

copy_dir_rec(BaseDir, DestDir) :-
	copy_dir_rec(BaseDir, DestDir, '*', '', '', [], 0).

extract_strings([], _, []).
extract_strings(Strings, Separator, String) :-
	(
	    list_concat([Head, Separator, Tail],Strings) ->
	    (
		String=Head
	    ;
		extract_strings(Tail, Separator, String)
	    )
	;
	    String=Strings
	),
	\+(String = "").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Script manipulation predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_backup_filename(FileName, B) :-
	get_backup_filename(FileName, 0, B).

get_backup_filename(FileName, I, B) :-
	atom_number(IA, I),
	atom_concat([FileName, '.bak~', IA, '~'], B),
	\+ file_exists(B)
    ;
	I2 is I + 1,
	get_backup_filename(FileName, I2, B).
	

backup(FileName) :-
	(
	    file_exists(FileName) ->
	    get_backup_filename(FileName, B),
	    --delete_file(B),
	    copy_file(FileName, B)
	;
	    true
	).

secure_write(FileName, T) :-
	backup(FileName),
	string_to_file(T, FileName).

uninstall_script(File, Comment) :-
	file_exists(File) ->
	file_to_string(File, String, Tail),
	(   Tail = [],
	    script_undo(Comment, _Key, NewString, After, _, _, After, [],
	        String, []) ->
	    secure_write(File, NewString)
	;   true
	)
    ;
	true.

% ----------------------------------------------------------------------------
script_undo(Comment, Key, Before, BeforeTail, Bash, BashTail, After,
	    AfterTail) -->
	any_string(Before, BeforeTail),
	start_message(Comment, Key),
	any_string(Bash, BashTail),
	end_message(Comment, Key),
	any_string(After, AfterTail).

% ----------------------------------------------------------------------------
any_string(BTail, BTail) --> "".
any_string([Char|Head], BTail, [Char|Chars], Tail) :-
	any_string(Head, BTail, Chars, Tail).

genkey(Min, Max, Value) :-
	time(Time),
	Value is Min + (Time mod (Max - Min + 1)).

:- meta_predicate configure_script(?, ?, pred(3)).

configure_script(File, Comment, PredLines) :-
	(   file_exists(File) ->
	    file_to_string(File, String, Tail)
	;   String = Tail
	),
	(
	    Tail = [],
	    script_undo(Comment, Key, NewString, BTail, _, _, After, [],
	        String, []) ->
	    PredLines(Key, BTail, After)
	;   PredLines(~number_codes(~genkey(10000000, 99999999)), Tail, []),
	    NewString = String
	),
	secure_write(File, NewString).

this_word(Word, S, T) :-
	append(Word, T, S).

start_message(Comment, Key) -->
	"\n", this_word(Comment), " @begin(", key_message(Key),
	")@ - Do not edit these lines - added automatically!\n".

end_message(Comment, Key) -->
	this_word(Comment), " @end(", key_message(Key),
	")@ - End of automatically added lines.\n".

key_message("") --> "".
key_message([D|H]) --> digit(D), key_message(H).

digit(0'0) --> "0".
digit(0'1) --> "1".
digit(0'2) --> "2".
digit(0'3) --> "3".
digit(0'4) --> "4".
digit(0'5) --> "5".
digit(0'6) --> "6".
digit(0'7) --> "7".
digit(0'8) --> "8".
digit(0'9) --> "9".

isadmin :-
	current_env('USERNAME',X), X=='root'.

usersrc(UserSrc) :-
	current_env('USERSRC',UserSrc).

% ----------------------------------------------------------------------------

:- meta_predicate find(       ?, pred(3), pred(3), pred(3), pred(3), ?, ?, ?).
:- meta_predicate find_rec(?, ?, pred(3), pred(3), pred(3), pred(3), ?, ?, ?). 

find_dummy_(_BaseDir,_File,_Params).
find_dummy_fail_(_BaseDir, _File, _Params) :- fail.

find(BaseDir, FileCondition, DirCondition, DirBefore, DirAfter, Params, Result,
	    Tail) :-
	find_rec(~directory_files(BaseDir), BaseDir, FileCondition,
	    DirCondition, DirBefore, DirAfter, Params, Result, Tail).

find_rec([],           _BaseDir, _FileCondition, _DirCondition, _DirBefore,
	   _DirAfter, _Params, Result, Result) :- !.
find_rec(['.'|Files],   BaseDir,  FileCondition,  DirCondition,  DirBefore,
	    DirAfter,  Params, Result, Tail) :- !,
	find_rec(Files, BaseDir,  FileCondition,  DirCondition,  DirBefore,
	    DirAfter,  Params, Result, Tail).
find_rec(['..'|Files],  BaseDir,  FileCondition,  DirCondition,  DirBefore,
	    DirAfter,  Params, Result, Tail) :- !,
	find_rec(Files, BaseDir,  FileCondition,  DirCondition,  DirBefore,
	    DirAfter,  Params, Result, Tail).
find_rec([File|Files],  BaseDir,  FileCondition,  DirCondition,  DirBefore,
	    DirAfter,  Params, Result, Tail) :-
	atom_concat([BaseDir, '/', File], FileName),
	(
	    \+(file_property(FileName, linkto(_))),
	    file_exists(FileName),
	    file_property(FileName, type(directory)) ->
	    find_dir(File, BaseDir, FileName, FileCondition, DirCondition,
	        DirBefore, DirAfter, Params, Result, Tail0)
	;
	    (   FileCondition(BaseDir, File, Params) ->
		Result = [FileName|Tail0]
	    ;   Result = Tail0
	    )
	),
	!,
	find_rec(Files, BaseDir, FileCondition, DirCondition, DirBefore,
	    DirAfter, Params, Tail0, Tail).

find_dir(File, BaseDir, FileName, FileCondition, DirCondition, DirBefore,
	    DirAfter, Params, Result, Tail) :-
	(   DirCondition(BaseDir, File, Params) ->
	    Result = [FileName|Tail0]
	;   Result = Tail0
	),
	(
	    DirBefore(BaseDir, File, Params) ->
	    find(FileName, FileCondition, DirCondition, DirBefore, DirAfter,
	        Params, Tail0, Tail),
	    ( DirAfter(BaseDir, File, Params) -> true ; true )
	;
	    Tail0 = Tail
	).

:- pop_prolog_flag(multi_arity_warnings).

% --- Delete routines

:- comment(delete_dir_rec(Directory), "Deletes the directory
   @var{Directory} and all the files and subdirectories
   recursively.").

delete_dir_rec(Dir) :-
	\+ file_property(Dir, linkto(_)),
	file_exists(Dir),
	file_property(Dir, type(directory)) ->
	find(Dir, find_delete_file_, find_dummy_fail_, find_dummy_,
	    find_delete_directory_, _, _, _),
%	display(delete_directory(Dir)),nl
	-delete_directory(Dir)
    ;
	true.

find_delete_file_(BaseDir, File, _Params) :-
%	display(delete_file(~atom_concat([BaseDir,'/',File]))),nl,
	-delete_file(~atom_concat([BaseDir,'/',File])),
	fail.

find_delete_directory_(BaseDir, File, _Params) :-
%	display(delete_directory(~atom_concat([BaseDir,'/',File]))),nl.
	-delete_directory(~atom_concat([BaseDir,'/',File])).

delete_dir_rec_list([]).
delete_dir_rec_list([Dir|Dirs]) :-
	delete_dir_rec(Dir),
	delete_dir_rec_list(Dirs).

% --- collector

collect_modules_filecond(BaseDir, File, Params) :-
	match_pattern_pred('*.pl', File),
	collect_modules_filecond2(BaseDir, File, Params).

collect_modules_filecond2(BaseDir, File, _) :-
	\+( member(File, ['LPSETTINGS.pl', 'Makefile.pl']) ),
	atom_concat([BaseDir,'/',File], FileName),
%	set_prolog_flag(write_strings, on),
	open(FileName, read, Stream),
	catch(
	    read(Stream, Term),
	    E, (
	    warning(['File ', FileName, ' raised the following exception:',
	        E]),
	    Term = [])),
	close(Stream),
	!,
	(   (Term = (:- module(_,_,_)) ; Term = (:- module(_,_))) ->
	    true
	;   fail
	).

collect_modules_dircond(BaseDir, Dir, params(NCFiles)) :-
	member(NCFile, NCFiles),
	atom_concat([BaseDir, '/', Dir, '/', NCFile], NCFileName),
	file_exists(NCFileName) ->
	fail
 ;      true.

using_tty :-
	popen('(stty > /dev/null) 2>&1', read, S),
	stream_to_string(S,"").

return_code("\\r") :-
	using_tty,
	!.
return_code("\\n").

tty(using_tty) :-
	using_tty,
	!.
tty(no_tty).

append_dcg(A,B,C) :- append(A, C, B).

write_compiling_msg(using_tty, I, N) -->
	"display(user_error,\'\\rCompiling ",
	append_dcg(~number_codes(I)), "/",
	append_dcg(~number_codes(N))," \').\n".
write_compiling_msg(no_tty,    _, _) -->
	"display(user_error,\'\\nCompiling \').\n".

write_collect_modules(FileName, UsingTTY, Options, BaseDirP) -->
	{length(FileName, N)},
	write_collect_modules_(FileName,    UsingTTY,  1, N, Options, BaseDirP).
write_collect_modules_([],                 _UsingTTY, _I, N,_Options,_BaseDirP) -->
	"display(user_error,\'",
	append_dcg(~return_code),
	"*** Compiled ",
	append_dcg(~number_codes(N)),
	" modules\\n\').\n".
write_collect_modules_([FileName|FileNames], UsingTTY, I, N, Options, BaseDirP) -->
	{atom_concat(BaseDirP, File, FileName),
	 atom_codes(File,FileS)
	},
	write_compiling_msg(UsingTTY, I, N),
	"display(user_error,\'", append_dcg(FileS), "\').\n" ||
	"display(user_error,\' \').\n" ||
	"make_po(\'", append_dcg(FileS), "\').\n",
	write_gaf(FileS,Options),
	{I2 is I + 1},
	write_collect_modules_(FileNames, UsingTTY, I2, N, Options, BaseDirP).

write_gaf(FileS,Options) -->
	{member(gen_asr_file, Options),
	 !},
	"gaf(\'", append_dcg(FileS), append_dcg("\').\n").
write_gaf(_,_) --> "".

collect_modules(BaseDir, Modules) :-
	find(BaseDir, collect_modules_filecond, find_dummy_fail_,
	    collect_modules_dircond, find_dummy_,
	    params(['Makefile', 'Makefile.pl', 'LPSETTINGS.pl',
	    '.NOCOMPILE','NOCOMPILE']), Modules, []).

dump_collect_modules(BaseDir, Options, String, Tail) :-
	collect_modules(BaseDir, Modules),
	atom_concat(BaseDir, '/', BaseDirP),
	tty(UsingTTY),
	write_collect_modules(Modules, UsingTTY, Options, BaseDirP, String, Tail).

compile_collected_modules(BaseDir, HeadText, Options, CollectFile,
	    SetLocalCiao, CiaoShell, MessagesFile) :-
	generate_collect_file(BaseDir,HeadText,Options,CollectFile),
	do(['cd ', BaseDir, ' ; ', SetLocalCiao, ' ', CiaoShell,
	    ' -f < ', CollectFile, ' > ', MessagesFile], fail).

generate_collect_file(BaseDir,HeadText,Options,CollectFile) :-
	atom_list_to_string(HeadText, String, Tail0),
	dump_collect_modules(BaseDir, Options, Tail0, []),
	string_to_file(String, ~atom_concat([BaseDir,'/', CollectFile])).

atom_list_to_string([], T, T).
atom_list_to_string([A|As], T0, T) :-
	atom_codes(A, S),
	append(S, T1, T0),
	atom_list_to_string(As, T1, T).

% Delete auto generated docs by lpdoc
docsclean(MainFile) :-
	specific_docclean(html,  MainFile),
	specific_docclean(texi,  MainFile),
	specific_docclean(dvi,   MainFile),
	specific_docclean(ps,    MainFile),
	specific_docclean(pdf,   MainFile),
	specific_docclean(txt,   MainFile),
	specific_docclean(ascii, MainFile),
	specific_docclean(txt,   MainFile),
	specific_docclean(htmlindex, MainFile),
	specific_docclean(htmlbullet, MainFile),
	specific_docclean(manl, MainFile),
	specific_docclean(info, MainFile),
	additional_docclean(MainFile).

additional_docclean(MainFile) :-
	specific_docclean(htmlsumm, MainFile),
	specific_docclean(l, MainFile),
	del_files_nofail(~ls(~atom_concat([MainFile,
	    'autofig*.ppm|autofig*.jpg|']))).

specific_docclean(html, MainFile) :-
	del_file_nofail(~atom_concat([MainFile,'.html'])),
	clean_html_dir(MainFile),
	!.
specific_docclean(info, MainFile) :-
	del_files_nofail(~ls(~atom_concat([MainFile,'*.info*']))),
	!.
specific_docclean(DocFormat, MainFile) :-
	del_files_nofail(~ls(~atom_concat([MainFile,'*.',DocFormat]))),
	!.

clean_html_dir(MainFile) :-
	atom_concat([MainFile, '_html'], HtmlDir),
	delete_dir_rec(HtmlDir).

autolibs(Header, BaseDir, Mode, SetLocalCiao, CiaoSh) :-
	autolibs(Header, BaseDir, [gen_asr_file], Mode, CiaoSh, SetLocalCiao).

autolibs(Header, BaseDir, Options, Mode, SetLocalCiao, CiaoSh) :-
	bold_message("Compiling ~w libraries", [BaseDir]),
	AllModules = 'all_modules.tmp',
	AllModuleMessages = 'all_module_messages.tmp',
	atom_concat([BaseDir, '/', AllModules], AllModulesR),
	atom_concat([BaseDir, '/', AllModuleMessages], AllModuleMessagesR),
	del_files_nofail([AllModulesR, AllModuleMessagesR]),
	compile_collected_modules(
	    BaseDir,
	    Header,
	    Options,
	    AllModules,
	    SetLocalCiao,
	    CiaoSh,
	    AllModuleMessages),
	set_perms(AllModulesR, Mode),
	del_files_nofail([AllModulesR, AllModuleMessagesR]),
	bold_message("~w compilation completed.", [BaseDir]).

atomdelim(AD, D, A) :-
	atom_concat([A0,D,ADs],AD) ->
	(
	    A = A0
	;
	    atomdelim(ADs,D,A)
	)
 ;
	A=AD.

atomdelim_list(A, D, [L|Ls]) :-
	atom_concat([L,D,As] ,A),
	!,
	atomdelim_list(As, D, Ls).
atomdelim_list(A,_, [A]).

%%% --------------------------------------------------------------------------
%%% Deprecated predicates:  (Marked as deprecated in 2005-07-13)
%%% --------------------------------------------------------------------------

copy_dir_rec_old(BaseDir, DestDir, Pattern, PatternExcludeFile,
	PatternExcludeDir, ExcludeDirFiles, Verbose) :-
	normalize_path(BaseDir,PBaseDir),
	normalize_path(DestDir,PDestDir),
	recurse_directory(copy_dir_before_old_, dummy_, copy_file_old_,
	PBaseDir, params(PBaseDir, PDestDir, Pattern, PatternExcludeFile,
	PatternExcludeDir, ExcludeDirFiles, Verbose), _, _).

copy_dir_before_old_(BaseSubDir, Params, _, _) :-
	Params = params(BaseDir, DestDir, _, _, _PatternExcludeDir,
	_ExcludeDirFiles, _Verbose),
	list_files_filter_dir_before_(BaseSubDir, Params, [], _) ->
	atom_concat(BaseDir,RelSubDir,BaseSubDir),
	atom_concat(DestDir,RelSubDir,DestSubDir),
	make_dirpath(DestSubDir).

copy_file_old_(File, params(BaseDir, DestDir, Pattern, PatternExcludeFile, _, _,
	    Verbose), _, _) :-
	list_files_filter_(File, params(BaseDir, DestDir, Pattern,
	PatternExcludeFile, _, _, Verbose), [], [DestFile]) ->
	copy_file(File, DestFile, [overwrite]).

% This predicate is present only for compatibility
copy_dir_rec(BaseDir, DestDir, Pattern, PatternExcludeFile, SkipDirs, NDFiles,
	    _Verbose) :-
	copy_dir_rec(BaseDir, DestDir, Pattern, PatternExcludeFile,
	    SkipDirs, NDFiles).

cvs_export(Params, Module, Dir) :-
	do(['cd ', Dir, ' && ', Params, ' cvs export -D \"0 seconds ago\" ',
	Module], nofail).

list_files_filter_dir_before_(BaseSubDir, params(_BaseDir, _DestDir, _Pattern,
	_PatternExcludeFile, PatternExcludeDir, ExcludeDirFiles, Verbose),
	List, List) :-
        member(ExcludeFile, ExcludeDirFiles),
	atom_concat([BaseSubDir, '/', ExcludeFile], AbsExcludeFile),
	file_exists(AbsExcludeFile) ->
	(   Verbose == 1 ->
	    format("{NOTE: Skipping Specific Excluded Dir ~w}\n", [BaseSubDir])
	;
	    true
	),
	fail
 ;
	(
%	    display(match_pattern_pred(PatternExcludeDir,BaseSubDir)),nl,
	    match_pattern_pred(PatternExcludeDir,BaseSubDir) ->
	    (   Verbose == 1 ->
		format("{NOTE: Skipping Pattern Excluded Dir ~w}\n",
		[BaseSubDir])
	    ;
		true
	    ),
	    fail
	;
	    true
	).

list_files_filter_(File, params(BaseDir, DestDir, Pattern, PatternExcludeFile,
	_,_,Verbose), ListIn, ListOut) :-
	no_path_file_name(File,BaseFile),
	(   match_pattern_pred(PatternExcludeFile,BaseFile) ->
	    (   Verbose = 1 ->
		format("{NOTE: Skipping Excluded File ~w}\n",[File]),fail
	    ;
		true
	    ),
	    ListOut = ListIn
	;
	    (   match_pattern_pred(Pattern,BaseFile) ->
		atom_concat(BaseDir,RelFile,File),
		atom_concat(DestDir,RelFile,DestFile),
	        ListOut = [DestFile|ListIn]
	    ;
		(   Verbose = 1 ->
		    format("{NOTE: Skipping Not Included File ~w}\n", [File]),
		    fail
		;
		    true
		),
		ListOut = ListIn
	    )
	).

list_files_filter_rec(BaseDir, DestDir, Pattern, PatternExcludeFile,
	PatternExcludeDir,ExcludeDirFiles,Verbose,List) :-
	list_files_filter_rec(BaseDir, DestDir, Pattern, PatternExcludeFile,
	PatternExcludeDir, ExcludeDirFiles, Verbose, [], List).

list_files_filter_rec(BaseDir, DestDir, Pattern, PatternExcludeFile,
	PatternExcludeDir, ExcludeDirFiles, Verbose, ListIn, ListOut) :-
	normalize_path(BaseDir, PBaseDir),
	recurse_directory(list_files_filter_dir_before_, dummy_,
	list_files_filter_, PBaseDir, params(PBaseDir, DestDir, Pattern,
	PatternExcludeFile, PatternExcludeDir, ExcludeDirFiles, Verbose),
	ListIn,ListOut).


% clean_html_dir(MainFile) :-
% 	atom_concat([MainFile, '_html'], HtmlDir),
% 	(   file_exists(HtmlDir)
% 	->  
% 	    del_files_nofail(
% 	        ~add_preffix(~ls(HtmlDir,
% 	        ~atom_concat([
% 	        MainFile, '*.html|', MainFile, '.css|autofig*.jpg'
% 	        ])),
% 		~atom_concat(HtmlDir,'/'))),
%% Kludge:
% 	    delete_directory(HtmlDir)
%%	    delete_dir_rec(HtmlDir)
%         ;   true
%         ).

list_dirs_rec_old(Dir,  Dest, List) :-
	list_dirs_rec_old_(Dir, Dest, [], List).

list_dirs_rec_old_(Dir, Dest, List, ListOut) :-
	normalize_path(Dir, PDir),
	recurse_directory(list_dirs_, dummy_, dummy_, PDir, (PDir, Dest),
	List, ListOut).

list_dirs_(BaseSubDir, (Dir, Dest), List, [DestSubDir|List]) :-
	atom_concat(Dir, RelSubDir,BaseSubDir),
	atom_concat(Dest,RelSubDir,DestSubDir).

list_files_rec_old(Dir, Dest, List) :- 
	list_files_rec_old_(Dir, Dest, [], List).

list_files_rec_old_(Dir,Dest,List,ListOut) :-
	normalize_path(Dir, PDir),
	recurse_directory(dummy_, dummy_, list_files_, PDir, (PDir, Dest),
	List, ListOut).

list_files_(File, (Dir,Dest), List, [DestFile|List]) :-
	atom_concat(Dir,  RelFile, File),
 	atom_concat(Dest, RelFile, DestFile).

do_recurse_directory([],           _GoalDirBefore,_GoalDirAfter,_GoalFile,
	_Directory,_Params,ParamsOut,ParamsOut).
do_recurse_directory(['.'|Files],   GoalDirBefore, GoalDirAfter, GoalFile,
	Directory, Params,ParamsIn,ParamsOut) :-
	do_recurse_directory(Files, GoalDirBefore, GoalDirAfter, GoalFile,
	Directory, Params,ParamsIn,ParamsOut).
do_recurse_directory(['..'|Files],  GoalDirBefore, GoalDirAfter, GoalFile,
	Directory, Params,ParamsIn,ParamsOut) :-
	do_recurse_directory(Files, GoalDirBefore, GoalDirAfter, GoalFile,
	Directory, Params,ParamsIn,ParamsOut).
do_recurse_directory([File|Files],  GoalDirBefore, GoalDirAfter, GoalFile,
	Directory, Params,ParamsIn,ParamsOut) :-
	atom_concat([Directory,'/',File],File2),
	catch(
	(
	    \+(file_property(File2, linkto(_))),
	    file_exists(File2),
	    file_property(File2, type(directory)) -> IsDir=true ; IsDir=false
	),
	error(Error,Where), 
	(
	    format("{WARNING: Skipping File ~w because it caused the "||
		"following exception: ",[File]),
	    (handle_error(Error,Where);true), format("}\n",[]),
	     IsDir=unknown
	)),
	(   IsDir==true ->
	    recurse_directory(GoalDirBefore, GoalDirAfter, GoalFile, File2,
	    Params, ParamsIn, ParamsIn2)
	;
	    true
	),
	(   IsDir==false ->
	    PFile =.. [GoalFile, File2, Params, ParamsIn, ParamsIn2],
	    (
		catch(PFile,
		error(Error,Where),
		(display('{WARNING: An exception was catched: '),
		(handle_error(Error,Where);true),display('}\n'))) -> true
	    ;
		ParamsIn2=ParamsIn
	    )
	;
	    true
	),
	!,
	do_recurse_directory(Files, GoalDirBefore, GoalDirAfter, GoalFile,
	Directory, Params, ParamsIn2, ParamsOut).

recurse_directory(GoalDirBefore, GoalDirAfter, GoalFile, Directory, Params,
	ParamsIn,ParamsOut) :-
	directory_files(Directory, Files),
	!,
	PBefore =.. [GoalDirBefore, Directory, Params, ParamsIn, ParamsIn2],
	(   call(PBefore) ->
	    do_recurse_directory(Files, GoalDirBefore, GoalDirAfter, GoalFile,
	    Directory, Params, ParamsIn2, ParamsIn3),
	    PAfter =.. [GoalDirAfter, Directory, Params, ParamsIn3, ParamsOut],
	    (
		call(PAfter) -> true
	    ;
		ParamsOut=ParamsIn3
	    )
	;
	    PAfter =.. [GoalDirAfter, Directory, Params, ParamsIn, ParamsOut],
	    (
		call(PAfter) -> true
	    ;
		ParamsOut = ParamsIn
	    )
	).

dummy_(_,_,X,X).
