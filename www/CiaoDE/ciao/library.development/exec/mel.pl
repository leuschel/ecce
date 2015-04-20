 %% make_exec.pl -- make small executables and libraries with ql code embedded
 %% Author          : Manuel Carro
 %% Created On      : Thu Jun 13 18:22:01 1996
 %% Last Modified By: MCL
 %% Last Modified On: Fri Feb  7 13:12:18 2003
 %% Update Count    : 769
 %% Status          : Kludge to include boot! Needs rewriting.


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%
 %% User options.
 %% verbose(Flag): if Flag is 'on', messages are printed which tell
 %% the user wht is going on.
 %%
 %% installbase(String): String is the name of the directory where the
 %% system in installed.
 %%
 %% ciaolibname(String): the name of the ciao library. By default
 %% String = "ciao" (which stands for the file "libciao.so").
 %%
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% :- entry(make_exec(A, B), [ground(A), ground(B)]).

use_modules(on).  % If 'on' only the toplevel modules need to be
                   % provided. The rest is automatically
                   % recompiled/linked. 
verbose(on).                            % Tell me what's happening
installbase('/home/clip/Systems/ciao2/bin/').
ciaolibname(ciao).                    % To link with -lciao
entry_point_suffix('_entry_point').     %
default_library(boot).                   %% Will always be added

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%
 %% make_exec(FilesAndOptions, ExecName): Produce the binary ExecName
 %% using FilesAndOptions, which consist of:
 %% * Atoms, which are taken to mean Prolog file names (no need to add
 %%  ".pl" suffix; however, the corresponding file should end in .pl).
 %% * Structures of the form lib(Atom), where Atom is the name of
 %%  a library containing predicates used by the executable.
 %% * Structures of the form libpath(Atom), where Atom is a path to a
 %%  directory containing libraries located in non-standard places.
 %%
 %% make_lib(FilesAndOptions, LibName): produce a ld-compliant shared
 %% library for later use. Options are the same as make_lib.
 %%
 %%
 %% The options passed to make_lib/2 and make_exec/2 are passed to ld,
 %% and have the same meaning.
 %%
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


make_exec(Files, ExecName):- make_exec_lib(Files, exec, ExecName).
make_lib(Files, LibName):-   make_exec_lib(Files, lib, LibName).

make_exec(File):- atom(File), make_exec_lib(File, exec, File).
make_lib(File):-  atom(File), make_exec_lib(File, lib, File).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% make_exec_lib(FilesAndOptions, MakeWhat, FinalFileName): make a
 %% library or an executable file depending on Type. 
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_exec_lib(Files, Type, ExecName):-
        nonvar(Files),
        Files = [_|_],  !,
        (
            atom(ExecName) ->
            make_exec_list_files(Files, Type, ExecName)
        ;
            format(user_error, 
            "{ Executable or library name must be an atom }~n", [])
        ).
make_exec_lib(File, Type, ExecName):-
        atom(File), !,
        make_exec_list_files([File], Type, ExecName).
make_exec_lib(_File, _Type, _ExecName):-
            format(user_error, 
            "{ First argument must be an atom or a list of atoms }~n", []).



 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% make_exec_list_files(FilesAndOptions, Type, ExecOrLibName):
 %% process a list of options and file names to produce an executable
 %% or a library.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_exec_list_files(Args, Type, ExecOrLib):-
        classify_args(Args, Names, Libraries, LibDirs),
        find_out_dependencies(Names, AllNames),
%%        extract_libraries(AllNames, AllUserNames),
        treat_libraries(AllNames, AllUserNames0, Extra),
	treat_extra(Extra, AllUserNames0, AllUserNames1),
        default_library(Boot),
	make_depends([library(Boot)],Shell),
	append(Shell, AllUserNames1, AllUserNames),
%%        make_ql_files_from_pl(AllUserNames0, AllQlNames),
	make_ql(AllUserNames0),
	all_ql_names(AllUserNames, AllQlNames),
        utrace( make_c_files_from_ql(AllUserNames, AllQlNames, CFileNames) ),
        (
            Type = lib ->
            atom_concat(lib, ExecOrLib, ExecOrLibName)
        ;
            ExecOrLib = ExecOrLibName
        ),
        write_main_file(Type,[Boot|AllNames],Libraries,ExecOrLibName,MainFileName),
        compile_c_files([MainFileName|CFileNames], Type, ObjFiles),
        link_stuff(Type, ObjFiles, Libraries, LibDirs, ExecOrLibName).



 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Classify the input arguments as Prolog file names, library names and
 %% library directories. Prolog file names are stripped out the ".pl"
 %% suffix if necessary.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

classify_args([], [], [], []).
classify_args([This|Ls], Pl, Libs, LDirs):- 
        where_goes_each(This, Pl, Libs, LDirs, RestPl, RestLibs, RestLDirs),
        classify_args(Ls, RestPl, RestLibs, RestLDirs).

where_goes_each(lib(L), Pl, [L|Lib], LDir, Pl, Lib, LDir):-
        atom(L). % , !. % , name(L, LString).
where_goes_each(libpath(LDir), Pl, Lib, [LDir|LDirs], Pl, Lib, LDirs):-
        atom(LDir). % , !. % ,  name(LDir, LDirStr).
where_goes_each(P, [PName|Pl], Lib, LDir, Pl, Lib, LDir):-
        atom(P), % !, 
        name(P, PStr),
        (
            append(BareName, ".pl", PStr) ->
            name(PName, BareName)
        ;
            name(PName, PStr)
        ).
where_goes_each(What, Pl, Lib, LDir, Pl, Lib, LDir):-
        format(user_error, "{ Ignoring object ~w }~n", [What]).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% generate a main C file which loads the ql code and calls the main
 %% function in the ciao library.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_main_file(Type, Objects, Libraries, ExecName, Main):-
        atom_concat(ExecName, 'XXXXXX', Template),
        unix(mktemp(Template, UniqueFile)),
        atom_concat(UniqueFile, '.c', Main),
        telling(OldInput),
        tell(Main),
        dump_header,
        write_main_function(Type, ExecName, Objects, Libraries),
        told,
        tell(OldInput).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Write the entry point of the library or executable. Executables
 %% have a main() function as entry point which calls all linked
 %% libraries. Libraries have an entry point which load themselves and
 %% nothing else.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_main_function(Type, ExecOrLibName, Objects, Libraries):-
        entry_point_suffix(EPS),
        choose_function_name_and_args(Type, ExecOrLibName, EPS),
        write_entry_point_calls(Objects, EPS),
        (
            Type = exec ->
            add_lib_prefix(Libraries, LibPrefixed),
            write_entry_point_calls(LibPrefixed, EPS),
            format("~n  ciao_kick_start();~n", [])
        ;
            true
        ),
        format("}~n", []).

add_lib_prefix([], []).
add_lib_prefix([L|Ls], [LibEp|OLs]):- 
        atom_concat(lib, L, LibEp),
        add_lib_prefix(Ls, OLs).

choose_function_name_and_args(exec, _ExecName, _EPS):-
        format("~n~nint main(argc, argv)~n", []),
        format("    int argc;~n", []),
        format("    char *argv[];~n", []),
        format("{~n", []),
        format("  ciao_init(argc, argv);~n~n", []).
choose_function_name_and_args(lib, LibName, EPS):-
        format("~n~nint ~w~w()~n", [LibName, EPS]),
        format("{~n", []).

write_entry_point_calls([], _EPS).
write_entry_point_calls([EntryPoint|Rest], EPS):-
        format("  ~w~w();~n", [EntryPoint, EPS]),
        write_entry_point_calls(Rest, EPS).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Link all the objects and libraries, possibly specifying some
 %% non-standard library directories, to give an executable or a
 %% library. This is actually an interface to the UNIX ld
 %% command. Options might depend on the operating system.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

link_stuff(Type, Objects, Libs, LibDirs, ExecOrLibName):-
        multi_concat(Objects, AllO),
        give_message("{ Linking ~w to produce ~w... }~n", [AllO, ExecOrLibName]),
        add_ciao_libs(Type, Libs, LibDirs, CiaoLibs, CiaoLibDirs),
        convert_libs_to_options(CiaoLibs, CiaoLibDirs, Options),
        link_command(Type, Objects, ExecOrLibName, Options, CommandString),
        execute_string(CommandString).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% ciao libs need to be specified only when making an executable
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_ciao_libs(exec, Libs, LDirs, [CiaoLibName|Libs], [CiaoLibDir|LDirs]):-
        ciaolibname(CiaoLibName),
        ciaolibdir(CiaoLibDir).
add_ciao_libs(lib, Libs, LDirs, Libs, LDirs).
        

link_command(lib, Objects, Lib, Options, CommandString):- 
        get_arch(Arch),
        make_lib_name(Arch, Lib, LibName),
        dyn_linking(Arch, Linker, DLOption),
        multi_concat([Linker, ' ', DLOption, ' -o ', LibName
                     | Options], Command1),
        multi_concat([Command1|Objects], CommandString).

link_command(exec, Objects, Exec, OtherLibs, CommandString):- 
        get_arch(Arch),
        compiler(Arch, Compiler),
        link_exec_options(Arch, Options),
        multi_concat([Compiler, ' ', Options, ' -o ', Exec
                      | OtherLibs], Command1),
        multi_concat([Command1|Objects], CommandString).


convert_libs_to_options(Libs, LDirs, Options):-
        (
            get_arch('SUNSOL') ->
            add_options(LDirs, ' -R', Options, R_Options)
        ;
            R_Options = Options
        ),
        add_options(LDirs, ' -L', R_Options, MoreOpts),
        add_options(Libs, ' -l', MoreOpts, []).

add_options([], _OptFlag, Options, Options).
add_options([What|Rest], OptFlag, [CompleteFlag|MoreOpt], Options):-
        atom_concat(OptFlag, What, CompleteFlag),
        add_options(Rest, OptFlag, MoreOpt, Options).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% make_ql_files_from_pl(PlFIles, QlFIles): makes Qls from the
 %% PlFiles given in list
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_ql_files_from_pl([], []).
make_ql_files_from_pl([PlName|PlNames], [QlFileName|QlFileNames]):-
        atom_concat(PlName,'.pl',PlFileName),
        atom_concat(PlName,'.ql',QlFileName),
 %%         name(PlFileName, PlFileAtom),
 %%         name(QlFileName, QlFileAtom),
        make_ql_from_pl(PlFileName, QlFileName),
        make_ql_files_from_pl(PlNames, QlFileNames).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% make_c_files_from_ql(Names, QlFileNames, CFileNames): make a C
 %% file for each Ql file, containing the Ql code in a static
 %% array, its size and a function which loads it into the core of the
 %% ciao library. The names of the objects are taken from the file
 %% names. 
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_c_files_from_ql([], [], []).
make_c_files_from_ql([Name|Names], [QlName|QlNames], [CFile|CFiles]):-
        make_one_c_file(Name, QlName, CFile),
        make_c_files_from_ql(Names, QlNames, CFiles).

make_one_c_file(Name, QlName, CFileName):-
	( 
            Name = library(Name0) -> 
            true 
        ; 
            Name0=Name
        ),
        atom_concat(Name0, '.c', CFileName),
        make_c_from_ql(QlName, CFileName, Name0).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Create a .ql file from a .pl file, if needed. Returns list of
 %% files used by this .pl.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_ql_from_pl(PlFile, QlFile):- 
            younger_than(QlFile, PlFile) ->
            fcompile(PlFile)
            ;
            true.
 
        

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Create a .c file from a .ql file, if needed
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_c_from_ql(QlFile, CFile, Name0):-
        younger_than(CFile, QlFile) ->
        give_message("{ Translating ~w to ~w... }", [QlFile, CFile]),
        entry_point_suffix(EPS),
	name_suffix(Name0, Name),
        atom_concat(Name, EPS, EPName),
        atom_concat(Name, '_ql', QlArrayName),
        current_input(OldInput), 
        open(QlFile, read, NewInput),
        ( set_input(NewInput) ;
            close(NewInput), set_input(OldInput), fail ),
        current_output(OldOutput),
        open(CFile, write, NewOutput),
        ( set_output(NewOutput) ;
            close(NewOutput), set_output(OldOutput), fail ),
        dump_header,
        dump_array_name(QlArrayName),
        translate_ql_code_to_c_syntax(QlArraySize),
        dump_epilogue(EPName, QlArrayName, QlArraySize),
        !,
        close(NewInput), set_input(OldInput),
        close(NewOutput), set_output(OldOutput),
        give_message("{ Written ~w bytes of ql code }~n", [QlArraySize])
 ;
        true.
make_c_from_ql(_QlFile, CFile, _Name):-
        format(user_error, "{ Panic! Error while creating C file ~w }~n",
        [CFile]).

dump_header:-
        format("/* ~n", []),
        format("  This file has automatically been generated with~n",[]),
        format("  software produced by the folks at the CLIP lab~n",[]),
        format("  (C) Clip Lab 1996, 1997~n", []),
        format("  Reach 'em at http://www.clip.dia.fi.upm.es/~n", []),
        format("*/~n~n~n", []).

dump_array_name(Name):- format("static char ~w[] = {~n", [Name]).

translate_ql_code_to_c_syntax(Size):-
        get0(Code),
        translate_ql_code_to_c_syntax(Code, 1, Size).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% This should be a tight, fast loop. 
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

translate_ql_code_to_c_syntax(-1, Size, Size):- !,  %%  Enf of file
        format("    0~n};~n~n", []).
translate_ql_code_to_c_syntax(Code, SizeIn, SizeOut):-
        %% Code > -1,    %% OK, I now it is more decl., but I want it fast
        display(Code),
        display(',
'),
        get0(NewCode),
        SizeMid is SizeIn + 1,
        translate_ql_code_to_c_syntax(NewCode,SizeMid, SizeOut).

dump_epilogue(EPName, QlArrayName, QlSize):- 
        format("~w(){ciao_load_internal_ql(~w, ~w);}~n",
        [EPName, QlArrayName, QlSize]).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Compile a list of C files to .o files
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compile_c_files([], _Type, []).
compile_c_files([CFile|CFiles], Type, [' ', ObjFile |ObjFiles]):-
        compile_one_c_file(CFile, Type, ObjFile),
        compile_c_files(CFiles, Type, ObjFiles).

compile_one_c_file(CFile, Type, ObjAtom):-
        name(CFile, CFileStr),
        append(Name, ".c", CFileStr),
        append(Name, ".o", ObjFile),
        %%         name(CAtom, CFile),
        name(ObjAtom, ObjFile),
        (
            younger_than(ObjAtom, CFile) ->
            give_message("{ Making object code file for ~w... }~n", [CFile]),
            compile_command(Type, COptions),
            atom_concat(COptions, CFile, CommandString),
            execute_string(CommandString)
        ;
            true
        ).
            

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% The options to compile object files depend on wether we are
 %% linking to a shared library or to an executable file.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compile_command(exec, Command):-
        get_arch(Arch),
        compiler(Arch, C),
%%        append(C, " -g -c ", Command).
        atom_concat(C, ' -g -c ', Command).
compile_command(lib,  Command):- 
        get_arch(Arch),
        dyn_compiling(Arch, Compiler, Options),
        multi_concat([Compiler, ' ', Options, ' '], Command).



 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% find_out_dependencies(Files, AllFiles): for modular programs, 
 %% find out all files on which a given set of files may depend.
 %% If the use of modules is inhibited, the set of provided files is
 %% returned. 
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_out_dependencies(Files, AllFiles):-
        use_modules(on), !,
        find_out_dependencies_(Files, AllFiles).
find_out_dependencies(Files, Files):-
        use_modules(off).

find_out_dependencies_(Files, AllFiles):-
        sort(Files, SortedFiles),
        find_out_deps(SortedFiles, AllFiles).

find_out_deps(Files, AllFiles):-
        make_depends(Files, MoreFiles),
        sort(MoreFiles, SortedFiles),
        find_out_deps(Files, SortedFiles, AllFiles).

find_out_deps(Files, Files, Files):- !.
find_out_deps(Files, SFiles, AllFiles):-
        Files \== SFiles,
        find_out_dependencies_(SFiles, AllFiles).
        


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% extract_libraries(Progs, Libs): take out library names
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract_libraries([], []).
extract_libraries([library(_)|Ps], Rs):- !, 
        extract_libraries(Ps, Rs).
extract_libraries([A|Ps], [A|Rs]):-
        atom(A),
        extract_libraries(Ps, Rs).

 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% extract_libraries(Progs, Rest, Libs): take out library names
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract_libraries([], [], []).
extract_libraries([P|Ps], Rs, [P|Ls]):-
	P=library(_), !, 
        extract_libraries(Ps, Rs, Ls).
extract_libraries([A|Ps], [A|Rs], Ls):-
        atom(A),
        extract_libraries(Ps, Rs, Ls).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% treat_libraries(AllNames, AllUserNames,Extra): 
 %%   either leave library names, or extract them and add qloads in
 %%   an extra file, depending on a flag
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Hook predicate for prolog_flag
%:-
define_flag(linking,[static,dynamic],static).

treat_libraries(AllNames, AllUserNames, Extra):-
	prolog_flag(linking, Flag),
        treat_libraries(Flag, AllNames, AllUserNames, Extra).

treat_libraries(static, AllNames, AllNames, none).
treat_libraries(dynamic, AllNames, Names, Tmp):-
	treat_files(AllNames, QlFiles, SoFiles),
	extract_libraries(QlFiles, Names, Libs),
        unix(mktemp('tmpciaoXXXXXX', Tmp)),
	tell(Tmp),
	write_qloads(Libs),
	write_links(SoFiles),
	told.

write_qloads([L|Ls]):-
	absolute_file_name(L, '.ql', F, _NewDir),
	format(':- ~q(qload(~q)).~n',['',F]),
	write_qloads(Ls).
write_qloads([]).

all_ql_names([F|Fs], [P|Ps]):-
	absolute_file_name(F, '.ql', P, _NewDir),
	all_ql_names(Fs, Ps).
all_ql_names([], []).
	
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Miscellaneous predicates
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ciaolibdir(CiaoLibDir):-
        get_arch(Arch),
        installbase(Base),
        atom_concat(Base, Arch, CiaoLibDir).


multi_concat([Atom], Atom):- !.
multi_concat([Atom|Atoms], AllAppended):-
        multi_concat(Atoms, Partial),
        atom_concat(Atom, Partial, AllAppended).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% give_message(Format, List): identical to format/2, but writes only if
 %% the verbose falg is on.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

give_message(F, L):-
        verbose(V),
        give_message(V, F, L).
give_message(on, F, L):- format(F, L), ttyflush.
give_message(off, _F, _L).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Checks that YoungerFile is younger than OlderFile
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


younger_than(YoungerFile, _OlderFile):-
        \+ 'SYSCALL'('$unix_access'(YoungerFile, 0)).
younger_than(YoungerFile, OlderFile):-
        'SYSCALL'('$unix_modified'(YoungerFile, YoungerTime)),
        'SYSCALL'('$unix_modified'(OlderFile, OlderTime)),
        YoungerTime < OlderTime.


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Executes the string passed as a Unix command
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

execute_string(Command):-
%%        name(Command, CommandString),
        format("{ Command: ~w }~n", [Command]),
        unix(shell(Command)).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Database with options regarding compiling and linking for shared
 %% libraries in several systems 
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dyn_compiling(System, C, '-fPIC -g -c'):-
        System = 'LINUX',
        compiler(System, C).
dyn_compiling(System, C, '-fPIC -g -c'):-
        System = 'SUN4',
        compiler(System, C).
dyn_compiling(System, C, '-g -c'):-
        System = 'SUNSOL',
        compiler(System, C).


dyn_linking('LINUX', gcc, '-shared').
dyn_linking('SUN4', ld, '-assert pure-text').
dyn_linking('SUNSOL', ld, '-G').


link_exec_options('LINUX', '-rdynamic').
link_exec_options('SUNSOL', '').
link_exec_options('SUN4', '').

compiler(_System, 'gcc').


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Oh, gosh... every system seems to have its own preferences in
 %% everything...
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_lib_name(Arch, Lib, Libname):-
        lib_suffix(Arch, Suffix),
%        append(Lib, Suffix, Libname).
        atom_concat(Lib, Suffix, Libname).

lib_suffix('SUN4', '.so.1.0').
lib_suffix('SUNSOL', '.so').
lib_suffix('LINUX', '.a').

name_suffix(Name0, Name):-
	name(Name0, Str0),
	name('/', [C]),
	name_suffix0(Str0, C, Str),
	name(Name, Str).

name_suffix0(Str0, C, Str):-
	append(_Prefix, [C|Suffix], Str0), !,
	name_suffix0(Suffix, C, Str).
name_suffix0(Str, _, Str).
