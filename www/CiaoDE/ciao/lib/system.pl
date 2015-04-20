:- module(system, 
        [
            pause/1,
            time/1,
            datime/1,
            datime/9,
            datime_struct/1,
            getenvstr/2,
            setenvstr/2,
	    current_env/2,
	    set_env/2,
	    del_env/1,
	    c_errno/1,
%	    c_strerror/1,
	    copy_file/2,
	    copy_file/3,
	    copy_file/4,
	    copymode/1,
            extract_paths/2,
            get_pid/1,
            current_host/1,
            current_executable/1,
            umask/2,
            make_directory/2,
            make_directory/1,
	    make_dirpath/2,
	    make_dirpath/1,
            working_directory/2,
            cd/1,
            shell/0,
            shell/1,
            shell/2,
            system/1,
            system/2,
            popen/3,
            popen_mode/1,
            exec/4,
            exec/3,
            exec/8,
            wait/3,
            directory_files/2,
            mktemp/2,
            file_exists/1,
            file_exists/2,
            file_property/2,
            file_properties/6,
            modif_time/2,
            modif_time0/2,
            fmode/2,
            chmod/2,
            chmod/3,
            delete_file/1,
            delete_directory/1,
            rename_file/2,
	    using_windows/0,
	    winpath/2,
	    winpath/3,
	    winpath_c/3,
	    cyg2win/3,
	    no_swapslash/3,
	    replace_characters/4,
	    system_error_report/1
        ],
	[assertions, isomodes, hiord, regtypes]).

:- comment(title, "Operating system utilities").

:- comment(author, "Daniel Cabeza").
:- comment(author, "Manuel Carro").

:- comment(module, "This module contains predicates for invoking
   services which are typically provided by the operating system.
   Note that the predicates which take names of files or directories
   as arguments in this module expect atoms, not @concept{path
   alias}es. I.e., generally these predicates will not call
   @pred{absolute_file_name/2} on names of files or directories taken
   as arguments.").

:- use_module(engine(internals), ['$unix_popen'/3, '$exec'/8]).
:- use_module(library(lists), [append/3]).
:- use_module(library(messages),[warning_message/2]).

:- impl_defined([
        working_directory/2, directory_files/2, pause/1, time/1, datime/9,
        current_host/1, c_get_env/2, c_set_env/2, c_current_env/3, c_del_env/1,
	c_copy_file/3, c_winpath/2, c_posixpath/2, c_winfile/2, c_posixfile/2,
	c_errno/1, c_strerror/1, get_pid/1, current_executable/1,
	shell/0, shell/2, system/2, mktemp/2, file_exists/2, wait/3,
	file_properties/6, chmod/2, umask/2, delete_file/1, using_windows/0,
	delete_directory/1, rename_file/2, make_directory/2]).


:- multifile define_flag/3.

%% This flag determines which path style is returned by the system
%% predicates which can return paths.  'os_dependent' means paths 
%% will be returned in the forma0-t the OS accepts it.  'posix' means
%% using a POSIX-style, with slashes, no drive letters or UNC paths,
%% and semicolons as separators.

define_flag(path_style, [posix, os_dependent], os_dependent).


:- comment(pause(Seconds), "Make this thread sleep for some @var{Seconds}.").

:- true pred pause(+int).

:- comment(time(Time), "@var{Time} is unified with the number of seconds
     elapsed since January, 1, 1970 (UTC).").

:- true pred time(?int).

 %% :- comment(walltime(Time),"@var{Time} is unified with the time in
 %%      milliseconds elapsed in the real world since the last call to
 %%      @pred{walltime/1}. The first call returns a meaningless number.").
 %% 
 %% :- true pred walltime(?int).

:- comment(datime(Datime), "@var{Datime} is unified with a term of the
     form @tt{datime(Year,Month,Day,Hour,Minute,Second)} which contains
     the current date and time.").

:- true pred datime(?datime_struct).

datime(datime(Year,Month,Day,Hour,Min,Sec)) :-
        datime(_, Year, Month, Day, Hour, Min, Sec, _, _).

:- true regtype datime_struct/1.

datime_struct(datime(Year,Month,Day,Hour,Min,Sec)) :-
        int(Year), int(Month), int(Day), int(Hour), int(Min), int(Sec).

:- comment(datime(Time,Year,Month,Day,Hour,Min,Sec,WeekDay,YearDay),
	"@var{Time} is as in @pred{time/1}. @var{WeekDay} is the number
	of days since Sunday, in the range 0 to 6.  @var{YearDay} is the
	number of days since January 1, in the range 0 to 365.").

:- true pred datime(+int,?int,?int,?int,?int,?int,?int,?int,?int)
        # "If @var{Time} is given, the rest of the arguments are unified
        with the date and time to which the @var{Time} argument refers.".

:- true pred datime(-int,?int,?int,?int,?int,?int,?int,?int,?int)
	# "Bound @var{Time} to current time and the rest of the
	arguments refer to current time.".

:- comment(bug, "In some situations, copy_file don't work when the
second argument is a directory, example:

 	copy_file( 'site/ciaopp_online.html' ,
	           ~distdir, yes),

").

:- true pred copy_file(+atm, +atm, +atm).
:- true pred copy_file(+atm, +atm).

:- true pred errno(?atm).
:- true pred strerrno(?atm).

:- comment(copy_file(Source,Destination), "Copies the file @var{Source} to
	@var{Destination}.").

:- regtype copymode/1.

copymode(yes).
copymode(no).

copy_mode_options(yes,[overwrite]).
copy_mode_options(no, []).

:- regtype copy_option/1.

copy_option(overwrite).  % overwrite
copy_option(timestamp).  % preserve time stamp

:- regtype copy_options/1.

copy_options([]).
copy_options([L|Ls]):-
	copy_option(L),
	copy_options(Ls).

copy_option_flag(overwrite,1).
copy_option_flag(timestamp,2).

copy_options_flag(Options, Flag) :-
	copy_options_flag_(Options, 0, Flag).

copy_option_flag_(Option, F0, F) :-
	copy_option_flag(Option, F1),
	F is F0 \/ F1.

copy_options_flag_([], F, F).
copy_options_flag_([Option|Options], F0, F) :-
	copy_option_flag_(Option, F0, F1),
	copy_options_flag_(Options, F1, F).

:- push_prolog_flag(multi_arity_warnings,off).
copy_file(Source, Destination) :-
	copy_file(Source, '', Destination, []).

copy_file(Source, Destination, CopyOptions) :-
	copy_file(Source, '', Destination, CopyOptions).

copy_file(Source, _SourceDir, _Destination, _CopyOptions) :-
	( \+atom(Source) ),
	!,
	throw(error(domain_error(atom,Source),copy_file/4-1)).
copy_file(_Source, SourceDir, _Destination, _CopyOptions) :-
	( \+atom(SourceDir) ),
	!,
	throw(error(domain_error(atom,SourceDir),copy_file/4-2)).
copy_file(_Source, _SourceDir, Destination, _CopyOptions) :-
	( \+atom(Destination) ),
	!,
	throw(error(domain_error(atom,Destination),copy_file/4-3)).
copy_file(_Source, _SourceDir, _Destination, CopyOptions) :-
	%% Backward compatibility with CopyMode:
	\+( ( copy_options(CopyOptions) ; copymode(CopyOptions) ) ),
	!,
	throw(error(domain_error(copy_options,CopyOptions),copy_file/4-4)).
% The next is for backward compatibility.  clause deprecated since 2005-12-16
copy_file(Source, SourceDir, Destination, CopyMode) :-
	copymode(CopyMode),
	!,
	copy_mode_options(CopyMode,CopyOptions),
	warning_message("in copy_file/4-5, use of ~w in last argument "||
	    "is deprecated, instead use ~w",[CopyMode,CopyOptions]),
	copy_file(Source,SourceDir,Destination,CopyOptions).
copy_file(Source, SourceDir, Destination, CopyOptions) :-
	(
	    SourceDir = '' ->
	    S = Source
	;
	    atom_concat(SourceDir, '/', S0),
	    atom_concat(S0, Source, S)
	),
	copy_options_flag(CopyOptions, CopyFlag),
	(
	    file_exists(Destination),
	    file_property(Destination, type(directory)) ->
	    atom_concat(Destination, '/', D0),
	    atom_concat(D0, Source, D1),
	    c_copy_file(S, D1, CopyFlag)
	;
	    c_copy_file(S, Destination, CopyFlag)
	).

:- regtype winpath_option/1.

winpath_option(full).
winpath_option(relative).

:- true pred winpath(-winpath_option, -atom, +atom).
:- true pred winpath(-winpath_option, +atom, -atom).

:- comment(winpath(Option, Posix, WinPath), "@var{Option} specifies if
   you want to get a relative or a full path.  @var{Posix} represent a
   path as usual in unix, and @var{WinPath} is the Windows-Style
   representation of @var{Posix}.").

winpath(Path, WinPath) :-
	winpath(full, Path, WinPath).

winpath(Full,_Path,_WinPath) :-
	( \+ winpath_option(Full) ),
	!,
	throw(error(domain_error(winpath_option,Full),winpath/3-1)).
winpath(_Full, Path,_WinPath) :-
	( \+ ( var(Path); atom(Path) ) ),
	!,
	throw(error(domain_error(var_or_atom,Path),winpath/3-2)).
winpath(_Full,_Path, WinPath) :-
	( \+ ( var(WinPath); atom(WinPath) ) ),
	!,
	throw(error(domain_error(var_or_atom,WinPath),winpath/3-3)).
winpath(Full, Path, WinPath) :-
	( var(Full), var(Path), var(WinPath) ),
	!,
	throw(error(instantiation_error)).
winpath(full, Path, WinPath) :-
	atom(Path) ->
	c_winpath(Path, WinPath)
 ;
	c_posixpath(WinPath, Path).
winpath(relative, Path, WinPath) :-
	atom(Path) ->
	c_winfile(Path, WinPath)
 ;
	c_posixfile(WinPath, Path).

:- comment(winpath_c/3, "Same as winpath/3, but for strings.").

winpath_c(Option, Dir, Path) :-
	atom_codes(DirA, Dir),
	winpath(Option, DirA, PathA),
	atom_codes(PathA, Path).

:- pred cyg2win(CygWinPath, WindowsPath, SwapSlash) : string * var *
   atom => string * string * atom # "Converts a posix path to a
   Windows-style path.  If @var{SwapSlash} is @tt{swap}, slashes are
   converted in to backslash.  If it is @tt{noswap}, they are
   preserved.".

cyg2win(Dir, Path, Swap) :-
	winpath_c(relative, Dir, PathSwap),
	no_swapslash(Swap, PathSwap, Path).

no_swapslash(swap, Dir, Dir) :-
	!.
no_swapslash(noswap, Dir, Path) :-
	do_no_swapslash(Dir, Path).

do_no_swapslash(Dir, Path) :-
	replace_characters(Dir, 0'\\, 0'/, Path).

:- comment(bug, "Currently we are seeing if CYGWIN environment
   variable exists to detect if we are inside the cygwin environment,
   which is not a good method.").

:- true pred using_windows # "Success if the operating system is using
   windows instead of posix operating system (which includes cygwin
   under windows).".

:- comment(replace_characters(String, SearchChar, ReplaceChar,
   Output), "Replaces all the occurrences of @var{SearchChar} by
   @var{ReplaceChar} and unifies the result with @var{Output}").

replace_characters([], _, _, []).
replace_characters([S|Ss], C, R, [T|Ts]) :-
	replace_character(S, C, R, T),
	replace_characters(Ss, C, R, Ts).

replace_character(S, S, R, R) :- !.
replace_character(S, _, _, S).
						% 
:- pop_prolog_flag(multi_arity_warnings).

:- comment(getenvstr(Name, Value), "The environment variable @var{Name}
    has @var{Value}.  Fails if variable @var{Name} is not defined.").

:- true pred getenvstr(+atm, ?string).

getenvstr(Name, _Value) :-
	( \+ atom(Name) ),
	!,
	throw(error(domain_error(atom,Name),getenvstr/2-1)).

getenvstr(Name, Value) :-
	c_get_env(Name, Value2),
	atom_codes(Value2,Value).

:- comment(setenvstr(Name, Value), "The environment variable @var{Name}
    is assigned @var{Value}.").

:- true pred setenvstr(+atm, +string).

setenvstr(Name, _Value) :-
	( \+ atom(Name) ),
	!,
	throw(error(domain_error(atom,Name),setenvstr/2-1)).

setenvstr(_Name, Value) :-
	( \+ ( Value = [_|_] ; Value = [] ) ),
	!,
	throw(error(domain_error(character_code_list,Value),setenvstr/2-2)).

setenvstr(Name, Value) :-
	atom_codes(Value2,Value),
	c_set_env(Name, Value2).

:- true pred set_env(+atm, +atm).

:- comment(set_env(Name, Value), "The environment variable @var{Name}
    is assigned @var{Value}.").

set_env(Name, _Value) :-
	( \+ atom(Name) ),
	!,
	throw(error(domain_error(atom,Name),set_env/2-1)).
set_env(_Name, Value) :-
	( \+ atom(Value) ),
	!,
	throw(error(domain_error(atom,Value),set_env/2-2)).
set_env(Name, Value) :-
	c_set_env(Name, Value).

:- true pred del_env(+atm).

:- comment(del_env(Name), "The environment variable @var{Name} is
   removed.").

del_env(Name) :-
	( \+ atom(Name) ),
	!,
	throw(error(domain_error(atom,Name),del_env/1-1)).

del_env(Name) :-
	c_del_env(Name).

:- true pred current_env(?atm, ?atm).

:- comment(current_env(Name,Value), "If @var{Name} is an atom, then
   unifies the environment variable @var{Name} with its value. Note
   that this predicate can be used to enumerate all the environment
   variables using backtracking.").


:- push_prolog_flag(multi_arity_warnings,off).


current_env(Name, _Value) :-
	( \+ ( var(Name); atom(Name) ) ),
	!,
	throw(error(domain_error(var_or_atom,Name),current_env/2-1)).
current_env(_Name, Value) :-
	( \+ ( var(Value); atom(Value) ) ),
	!,
	throw(error(domain_error(var_or_atom,Value),current_env/2-2)).
current_env(Name, Value) :-
	atom(Name) -> c_get_env(Name,Value);
	current_env(0, Name, Value).

current_env(I, Name, Value) :-
	c_current_env(I, Name2, Value2),
	(
	    Name=Name2, Value=Value2
	;
	    J is I + 1,
	    current_env(J, Name, Value)
	).

:- pop_prolog_flag(multi_arity_warnings).


:- comment(extract_paths(String, Paths), "Interpret @var{String} as the
   value of a UNIX environment variable holding a list of paths and
   return in @var{Paths} the list of the paths.  Paths in @var{String}
   are separated by colons, and an empty path is considered a shorthand
   for '.' (current path).  The most typical environment variable with this
   format is PATH.  For example, this is a typical use:
@begin{verbatim}
?- set_prolog_flag(write_strings, on).

yes
?- getenvstr('PATH', PATH), extract_paths(PATH, Paths).

PATH = "":/home/bardo/bin:/home/clip/bin:/opt/bin/:/bin"",
Paths = [""."",""/home/bardo/bin"",""/home/clip/bin"",""/opt/bin/"",""/bin""] ?

yes
?- 
@end{verbatim}
").

:- true pred extract_paths(+string, ?list(string)).

extract_paths([], ["."]).
extract_paths([C|Cs], [Path|Paths]) :-
        extract_path(C, Cs, ".", Path, Cs_),
        extract_paths_(Cs_, Paths).

extract_paths_([], []).
extract_paths_([_|Cs], Paths) :- % skip ":"
        extract_paths(Cs, Paths).

extract_path(0':, Cs, Path, Path, [0':|Cs]) :- !.
extract_path(C, [], _, [C], []) :- !.
extract_path(C, [D|Cs], _, [C|Path], Cs_) :-
        extract_path(D, Cs, [], Path, Cs_).


:- comment(get_pid(Pid), "Unifies @var{Pid} with the process
     identificator of the current process or thread.").

:- true pred get_pid(?int).

:- comment(current_host(Hostname), "@var{Hostname} is unified with the
        fully qualified name of the host.").

:- true pred current_host(?atm).

:- comment(current_executable(Path), "Unifies @var{Path} with the path
        to the current executable.").

:- true pred current_executable(?atm).

:- comment(umask(OldMask, NewMask), "The process file creation mask was
    @var{OldMask}, and it is changed to @var{NewMask}.").

:- true pred umask(?int, +int).

:- true pred umask(OldMask, NewMask)
        : (var(OldMask), var(NewMask), OldMask == NewMask)
       => (int(OldMask), int(NewMask))
        # "Gets the process file creation mask without changing it.".

:- comment(working_directory(OldDir, NewDir),"Unifies current working
     directory with @var{OldDir}, and then changes the working
     directory to @var{NewDir}. Calling
     @tt{working_directory(Dir,Dir)} simply unifies @tt{Dir} with the
     current working directory without changing anything else.").

:- true pred working_directory(?atm, +atm).

:- true pred working_directory(OldDir, NewDir)
        : (var(OldDir), var(NewDir), OldDir == NewDir) => atm * atm
        # "Gets current working directory.".

:- comment(cd(Path), "Changes working directory to @var{Path}.").

:- true pred cd(+atm).

cd(Dir) :- working_directory(_, Dir).

:- true pred shell # "Execs the shell specified by the environment
   variable @tt{SHELL}. When the shell process terminates, control is
   returned to Prolog.".

:- comment(bug, "@pred{shell/n} commands have a bug in Windows: if the
   environment variable SHELL is instantiated to some Windows shell
   implementation, then it is very possible that shell/@{1,2@} will not
   work, as it is always called with the -c flag to start the user
   command.  For example, COMMAND.COM @bf{might} need the flag /C -- but
   there is no way to know a priori which command line option is
   necessary for every shell!  It does not seems usual that Windows sets
   the SHELL environment variable: if it is not set, we set it up at
   startup time to point to the @tt{sh.exe} provided with Ciao, which is
   able to start Windows aplications.  Therefore, @tt{?-
   shell('command.com').} just works.").

:- comment(shell(Command), "@var{Command} is executed in the shell
    specified by the environment variable @tt{SHELL}. It succeeds if
    the exit code is zero and fails otherwise.").

%%      On MSDOS or Windows, if `SHELL' is defined it is expected to
%%      name a UNIX like shell which will be invoked with the argument `-c
%%      COMMAND'. If `SHELL' is undefined, the shell named by `COMSPEC'
%%      will be invoked with the argument `/C COMMAND'.

:- true pred shell(+atm).

shell(Path) :- shell(Path, 0).

:- comment(shell(Command, ReturnCode), "Executes @var{Command} in the
     shell specified by the environment variable @tt{SHELL} and stores
     the exit code in @var{ReturnCode}.").

:- true pred shell(+atm, ?int).

:- comment(system(Command), "Executes @var{Command} using the shell
        @apl{/bin/sh}.").

:- true pred system(+atm).

system(Path) :- system(Path, _Status).

:- comment(system(Command, ReturnCode), "Executes @var{Command} in the
     @apl{/bin/sh} shell and stores the exit code in @var{ReturnCode}.").

:- true pred system(+atm, ?int).


:- true pred split_atom(+Atom, ?AtomList) : 
                  atm * list(atm) #
  "This predicate decomposes a non-null atom which can have blank spaces 
   into a list of atoms, each one corresponding to the non-blank
   characters in the initial atom.".

split_atom(CommandAndArgs, Split):- 
        name(CommandAndArgs, ComandAndArgsStr),
        split_in_atoms(ComandAndArgsStr, Split).

split_in_atoms("", []).
split_in_atoms(" "||Rest, Atoms):-
        split_in_atoms(Rest, Atoms).
split_in_atoms(StartArg, [Atom|Atoms]):-
        StartArg = [NonBlank|_],
        NonBlank \== 0' ,
        extract_atom(StartArg, RestArgs, AtomStr), 
        name(Atom, AtomStr),
        split_in_atoms(RestArgs, Atoms).

extract_atom("", "", "").
extract_atom(" "||Rest, Rest, "").
extract_atom([Char|Chars], Rest, [Char|RestAtom]):-
        Char \== 0' ,
        extract_atom(Chars, Rest, RestAtom).


:- comment(exec(Command, StdIn, StdOut, StdErr), "Starts the process
   @var{Command} and returns the standart I/O streams of the process in
   @var{StdIn}, @var{StdOut}, and @var{StdErr}.  If @var{Command}
   contains blank spaces, these are taken as separators between a program
   name (the first chunk of contiguous non-blank characters) and options
   for the program (the subsequent contiguous pieces of non-blank
   characters), as in @pred{exec('ls -lRa ../sibling_dir', In, Out, Err)}.").

:- comment(bug,
   "If @pred{exec/4} does not find the command to be executed, there is 
    no visible error message: it is sent to a error output which has already 
    been assigned to a different stream, disconnected from the one the 
    user sees.").

:- true pred exec(+atm, -stream, -stream, -stream).

exec(Command, StdIn, StdOut, StdErr):- 
        split_atom(Command, [Program|Arguments]),
        exec(Program, Arguments, StdIn, StdOut, StdErr, true, _, _).


:- true pred exec(+Command, 
                  +Arguments, 
                  ?StdIn, 
                  ?StdOut,
                  ?StdErr,
                  +Background,
                  -PID,
                  -ErrCode) : atm * list(atm) * 
                              stream * stream * stream *
                              atm * int * int

# "@pred{exec/8} gives a finer control on execution of process.
@var{Command} is the command to be executed and @var{Arguments} is a
list of atoms to be passed as arguments to the command.  When called
with free variables, @var{StdIn}, @var{StdOut}, and @var{StdErr} are
instantiated to streams connected to the standard output, input, and
error of the created process. @var{Background} controls whether the
caller waits for @var{Command} to finish, or if the process executing
@var{Command} is completely detached (it can be waited for using
@pred{wait/3}). @var{ErrCode} is the error code returned by the
lower-level @tt{exec()} system call (this return code is
system-dependent, but a non-zero value usually means that something
has gone wrong).  If @var{Command} does not start by a slash,
@pred{exec/8} uses the environment variable @tt{PATH} to search for
it.  If @tt{PATH} is not set, @tt{/bin} and @tt{/usr/bin} are
searched.".

exec(Comm, Args, StdIn, StdOut, StdErr, Background, PID, ErrCode) :-
     '$exec'(Comm, Args, StdIn, StdOut, StdErr, Background, PID, ErrCode).


:- comment(exec(Command, StdIn, StdOut), "Starts the process
@var{Command} and returns the standart I/O streams of the process in
@var{StdIn} and @var{StdOut}. @tt{Standard error} is connected to
whichever the parent process had it connected to. @var{Command} is
   treated and split in components as in @pred{exec/4}.").

:- true pred exec(+atm, -stream, -stream).

exec(Command, StdIn, StdOut):- 
        exec(Command, StdIn, StdOut, []).

 %% :- comment(bug, "When reading from a exec'ed process, the 'end of
 %% file' condition (when the launched process finishes) somehow
 %% propagates to the standard input of the Ciao Prolog process, thus
 %% causing subsequent Ciao Prolog reads to return the 'end of file'
 %% condition.").


:- true pred wait(+Pid, -RetCode, -Status) : int * int * int # "wait/3
waits for the process numbered @var{Pid}.  If @var{PID} equals
-1, it will wait for any children process.  @var{RetCode} is usually
the PID of the waited-for process, and -1 in case in case of error.
@var{Status} is related to the exit value of the process in a
system-dependent fashion.".


:- comment(popen(Command, Mode, Stream), "Open a pipe to process
@var{Command} in a new shell with a given @var{Mode} and return a
communication @var{Stream} (as in UNIX @tt{popen(3)}). If @var{Mode}
is @tt{read} the output from the process is sent to @var{Stream}. If
@var{Mode} is @tt{write}, @tt{Stream} is sent as input to the
process. @var{Stream} may be read from or written into using the
ordinary stream I/O predicates. @var{Stream} must be closed explicitly
using @pred{close/1}, i.e., it is not closed automatically when the
process dies.  Note that @pred{popen/2} is defined in ***x as using
@tt{/bin/sh}, which usually does not exist in Windows systems.  In
this case, a @tt{sh} shell which comes with Windows is used.

").

:- true pred popen(+atm, +popen_mode, -stream).

 %% popen(Command, Mode, S) :-
 %% 	nonvar(Mode),
 %% 	popen_mode(Mode),
 %% 	atom(Command), !,
 %% 	'$unix_popen'(Command, Mode, S0), !, S=S0.

popen(Command, Mode, S):-
        nonvar(Mode), 
        popen_mode(Mode),
        atom(Command), !,
        (
            using_windows ->
            getenvstr('SHELL', WindowsShell),
            atom_codes(Shell, WindowsShell)
        ;
            Shell = '/bin/sh'
        ),
        (
            Mode = read ->
            exec(Shell, ['-c', Command], [], S, [], true, _, _)
        ;
            exec(Shell, ['-c', Command], S, [], [], true, _, _)
        ).

:- regtype popen_mode(M)
  # "@var{M} is 'read' or 'write'.".

popen_mode(read).
popen_mode(write).

:- comment(directory_files(Directory, FileList), "@var{FileList} is
   the unordered list of entries (files, directories, etc.) in
   @var{Directory}.").

:- true pred directory_files(+atm,?list(atm)).

:- comment(mktemp(Template, Filename), "Returns a unique
   @var{Filename} based on @var{Template}: @var{Template} must be a
   valid file name with six trailing X, which are substituted to
   create a new file name.  @var{Filename} is created in read/write mode 
   but closed immediately after creation.").

:- true pred mktemp(+atm, ?atm).

:- comment(file_exists(File), "Succeeds if @var{File} (a file or
        directory) exists (and is accessible).").

:- true pred file_exists(+atm).

file_exists(Path) :- file_exists(Path, 0).

:- comment(file_exists(File, Mode), "@var{File} (a file or directory)
   exists and it is accessible with @var{Mode}, as in the Unix call
   @tt{access(2)}. Typically, @var{Mode} is 4 for read permission, 2
   for write permission and 1 for execute permission.").

:- true pred file_exists(+atm, +int).

:- comment(file_property(File, Property), "@var{File} has the property
   @var{Property}. The possible properties are:

@begin{description}

@item{type(@var{Type})} @var{Type} is one of @tt{regular}, @tt{directory},
      @tt{symlink}, @tt{fifo}, @tt{socket} or @tt{unknown}.

@item{linkto(@var{Linkto})} If @var{File} is a symbolic link,
      @var{Linkto} is the file pointed to by the link (and the other
      properties come from that file, not from the link itself).

@item{mod_time(@var{ModTime})} @var{ModTime} is the time of last
      modification (seconds since January, 1, 1970).

@item{mode(@var{Protection})} @var{Protection} is the protection mode.

@item{size(@var{Size})} @var{Size} is the size.

@end{description}

   If @var{Property} is uninstantiated, the predicate will enumerate the
   properties on backtracking.").

:- true pred file_property(+atm, ?struct).

file_property(Path, Property) :-
	file_property_(Property, Path).

file_property_(Property, Path) :-
	var(Property), !,
	file_properties(Path, Type, Linkto, Time, Protection, Size),
	( Property = type(Type)
	; Linkto \== '', Property = linkto(Linkto)
	; Property = mod_time(Time)
	; Property = mode(Protection)
	; Property = size(Size)
	).
file_property_(type(Type), Path) :- !,
	file_properties(Path, Type0, [], [], [], []),
	Type = Type0.
file_property_(linkto(File), Path) :- !,
	file_properties(Path, [], File0, [], [], []),
	File0 \== '',
	File = File0.
file_property_(mod_time(Time), Path) :- !,
	file_properties(Path, [], [], Time, [], []).
file_property_(mode(Protection), Path) :- !,
	file_properties(Path, [], [], [], Protection, []).
file_property_(size(Size), Path) :- !,
	file_properties(Path, [], [], [], [], Size).
file_property_(Other, _) :-
	throw(error(domain_error(file_property_type,Other),
	file_property/2-2)).

:- comment(file_properties(Path, Type, Linkto, Time, Protection, Size),
        "The file @var{Path} has the following properties:

@begin{itemize} 

@item File type @var{Type} (one of @tt{regular}, @tt{directory},
      @tt{symlink}, @tt{fifo}, @tt{socket} or @tt{unknown}).

@item If @var{Path} is a symbolic link, @var{Linkto} is the file pointed
      to.  All other properties come from the file pointed, not the
      link.  @var{Linkto} is '' if @var{Path} is not a symbolic link.

@item Time of last modification @var{Time} (seconds since January, 1,
      1970).

@item Protection mode @var{Protection}.

@item Size in bytes @var{Size}.

@end{itemize}
").

:- true pred file_properties(+atm, ?atm, ?atm, ?int, ?int, ?int).

:- comment(modif_time(File, Time), "The file @var{File} was last
     modified at @var{Time}, which is in seconds since January, 1,
     1970. Fails if @var{File} does not exist.").

:- true pred modif_time(+atm, ?int).

modif_time(Path, Time) :-
        prolog_flag(fileerrors, OldFE, off),
        ( file_properties(Path, [], [], Time, [], []) ->
            set_prolog_flag(fileerrors, OldFE)
        ; set_prolog_flag(fileerrors, OldFE),
          fail
        ).

:- comment(modif_time0(File, Time), "If @var{File} exists, @var{Time} is
      its latest modification time, as in @pred{modif_time/2}.
      Otherwise, if @var{File} does not exist, @var{Time} is zero.").


:- true pred modif_time0(+atm, ?int).

modif_time0(Path, Time) :-
        prolog_flag(fileerrors, OldFE, off),
        ( file_properties(Path, [], [], T, [], []), !
        ; T = 0
        ),
        set_prolog_flag(fileerrors, OldFE),
        Time = T.

:- comment(fmode(File, Mode), "The file @var{File} has protection mode
        @var{Mode}.").

:- true pred fmode(+atm, ?int).

fmode(Path, Mode) :-
        file_properties(Path, [], [], [], Mode, []).

:- comment(chmod(File, NewMode), "Change the protection mode of file
        @var{File} to @var{NewMode}.").

:- true pred chmod(+atm, +int).

:- comment(chmod(File, OldMode, NewMode), "The file @var{File} has
    protection mode @var{OldMode} and it is changed to @var{NewMode}.").

:- true pred chmod(+atm, ?int, +int).

:- true pred chmod(File, OldMode, NewMode)
        : (atm(File), var(OldMode), var(NewMode), OldMode == NewMode)
        => atm * atm * atm
        # "Equivalent to fmode(@var{File},@var{OldMode})".

chmod(Path, OldMode, NewMode) :-
        OldMode == NewMode, !,
        fmode(Path, OldMode).
chmod(Path, OldMode, NewMode) :-
        fmode(Path, OldMode),
        chmod(Path, NewMode).


:- comment(delete_directory(File), "Delete the directory @var{Directory}.").

:- pred delete_directory(+atm).

:- comment(delete_file(File), "Delete the file @var{File}.").

:- pred delete_file(+atm).

:- comment(rename_file(File1, File2), 
        "Change the name of  @var{File1} to @var{File2}.").

:- pred rename_file(+atm,+atm).

:- comment(make_directory(DirName, Mode), "Creates the directory
   @var{DirName} with a given @var{Mode}.  This is, as usual, operated
   against the current umask value.").

:- pred make_directory(+atm, +int).

:- comment(make_directory(DirName),
        "Equivalent to @tt{make_directory(D,0o777)}.").

:- pred make_directory(+atm).

make_directory(D) :-
        make_directory(D,0o777).

:- pred make_dirpath(+sourcename, +int).

:- comment(make_dirpath(Path, Mode),
        "Creates the whole @var{Path} for a given directory with a given @var{Mode}. As an example, @tt{make_dirpath('/tmp/var/mydir/otherdir')}."). 

% We should take care of correct instantiation modes, types, etc. here.
% We are however delegating it to make_directory/2 and absolute_file_name/7
% (called below).
make_dirpath(Path, Mode) :-
%	absolute_file_name(Path, '', '', '.', AbsolutePath, _, _),
%	atom_codes(AbsolutePath, AbsPathCodes),
         atom_codes(Path, PathCodes),
         (          % If relative, transform it into absolute
             PathCodes = "/"||_ ->
             AbsPathCodes = PathCodes
         ;
             working_directory(CurrentDir, CurrentDir),
             atom_codes(CurrentDir, CurrentDirCodes),
             append(CurrentDirCodes, "/"||PathCodes, AbsPathCodes)
         ),
        make_abs_dir(AbsPathCodes, '', Mode).
% 
% Making the intermediate directories: instead of cd'ing to
% directories (which is something which can break and modify the
% program state), we construct incrementally the intermediate directories.
% The idea here is to traverse the absolute path and construct 
% partial paths, which are appended to an atom which represents the
% (initially empty) path.  Depending on the implementation of
% atom_concat/3 (and of the atoms), this can be done quite fast if the
% first argument does not need to be completely traversed.

% End of path
make_abs_dir("", _, _Mode):- !.
% The recursive case: perform a step in the recursion and construct the
% intermediate directory.
make_abs_dir(Path, IncPath, Mode):-
        decompose(Path, RestPath, Component-[]),
        % Transform into atom and add to partial path
        atom_codes(PartComp, Component),
        atom_concat(IncPath, PartComp, NewPath),
        (
            file_exists(NewPath) ->
            true
        ;
            make_directory(NewPath, Mode)
        ),
        make_abs_dir(RestPath, NewPath, Mode).


% decompose("//"||PathWOSlash, RestPath, Queue):- !, 
%         decompose("/"||PathWOSlash, RestPath, Queue).
decompose("/"||PathWOSlash, RestPath, "/"||Queue-TailQ):-
        decompose_aux(PathWOSlash, RestPath, Queue-TailQ).
decompose_aux("", "", Q-Q).
decompose_aux("/"||P, "/"||P, Q-Q):- !.
decompose_aux([P|Ps], RestP, [P|RestQ]-TailQ):- 
        decompose_aux(Ps, RestP, RestQ-TailQ).



 %% make_dirpath(Path, Mode) :-
 %% 	working_directory(CurrentDir, CurrentDir),
 %% 	make_dirpath_aux(Path, Mode),
 %% 	working_directory(_, CurrentDir).
 %% 
 %% make_dirpath_aux(Path, Mode) :-
 %% 	atom_concat(Head, Tail, Path),
 %% 	atom_concat('/', SubTail, Tail), !,
 %% 	(Head = '' ->
 %% 	 working_directory(CurrentDir, '/')
 %% 	;
 %% 	 ((file_exists(Head), file_property(Head, type(directory)))
 %% 	  ;
 %% 	   make_directory(Head, Mode) 
 %% 	 ),
 %% 	 working_directory(CurrentDir, Head)),
 %% 	make_dirpath_aux(SubTail, Mode).
 %% make_dirpath_aux(Dir, Mode) :-
 %% 	make_directory(Dir, Mode).
 %% 	 



:- pred make_dirpath(+atm).

:- comment(make_dirpath(Path),
                "Equivalent to @tt{make_dirpath(D,0o777)}.").

:- pred make_dirpath(+atm).
	  
make_dirpath(Path) :-
        make_dirpath(Path, 0o777) .

:- pred system_error_report( Report ) : var(Report) -> atm(Report)
# "Report is the error message from the last system call, like
  @tt{strerror} in POSIX.".

system_error_report( X ) :-
	c_strerror( X ).
