 %% make_exec.pl -- make small executables and libraries with ql code embedded
 %% Author          : Manuel Carro
 %% Created On      : Thu Jun 13 18:22:01 1996
 %% Last Modified By: Manuel Carro
 %% Last Modified On: Fri Feb 28 18:37:15 1997
 %% Update Count    : 596
 %% Status          : Seems to work 


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

verbose(on).                            % Tell me what's happening
installbase("/home/clip/Systems/ciao/bin/").
ciaolibname("ciao").                    % To link with -lciao
entry_point_suffix("_entry_point").     %


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


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% make_exec_lib(FilesAndOptions, MakeWhat, FinalFileName): make a
 %% library or an executable file depending on Type. 
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_exec_lib(Files, Type, ExecName):-
        Files = [_|_],
        make_exec_list_files(Files, Type, ExecName).
make_exec_lib(File, Type, ExecName):-
        atom(File),
        make_exec_list_files([File], Type, ExecName).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% make_exec_list_files(FilesAndOptions, Type, ExecOrLibName):
 %% process a list of options and file names to produce an executable
 %% or a library.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_exec_list_files(Args, Type, ExecOrLib):-
        name(ExecOrLib, ExecOrLibStr),
        classify_args(Args, Names, Libraries, LibDirs),
        make_c_files_from_prolog(Names, CFileNames),
        (
            Type = lib ->
            append("lib", ExecOrLibStr, ExecOrLibName)
        ;
            ExecOrLibStr = ExecOrLibName
        ),
        write_main_file(Type,Names,Libraries,ExecOrLibName,MainFileName),
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

where_goes_each(lib(L), Pl, [LString|Lib], LDir, Pl, Lib, LDir):-
        atom(L), !, name(L, LString).
where_goes_each(libpath(LDir), Pl, Lib, [LDirStr|LDirs], Pl, Lib, LDirs):-
        atom(LDir), !, name(LDir, LDirStr).
where_goes_each(P, [PStr|Pl], Lib, LDir, Pl, Lib, LDir):-
        atom(P), !, name(P, PStrWithPl),
        (
            append(BareName, ".pl", PStrWithPl) ->
            PStr = BareName
        ;
            PStrWithPl = PStr
        ).
where_goes_each(What, Pl, Lib, LDir, Pl, Lib, LDir):-
        format(user_error, "Ignoring object ~w~n", [What]).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% generate a main C file which loads the ql code and calls the main
 %% function in the ciao library.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_main_file(Type, Objects, Libraries, ExecName, MainFile):-
        concat_strings(ExecName, ".c", MainFile),
        name(Main, MainFile),
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
add_lib_prefix([L|Ls], [[0'l,0'i,0'b|L]|OLs]):- add_lib_prefix(Ls, OLs).

choose_function_name_and_args(exec, _ExecName, _EPS):-
        format("~n~nint main(argc, argv)~n", []),
        format("    int argc;~n", []),
        format("    char *argv[];~n", []),
        format("{~n", []),
        format("  ciao_init(argc, argv);~n~n", []).
choose_function_name_and_args(lib, LibName, EPS):-
        format("~n~nint ~s~s()~n", [LibName, EPS]),
        format("{~n", []).

write_entry_point_calls([], _EPS).
write_entry_point_calls([EntryPoint|Rest], EPS):-
        format("  ~s~s();~n", [EntryPoint, EPS]),
        write_entry_point_calls(Rest, EPS).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Link all the objects and libraries, possibly specifying some
 %% non-standard library directories, to give an executable or a
 %% library. This is actually an interface to the UNIX ld
 %% command. Options might depend on the operating system.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

link_stuff(Type, Objects, Libs, LibDirs, ExecOrLibName):-
        multi_append(Objects, AllO),
        message("Linking ~s to produce ~s... ", [AllO, ExecOrLibName]),
        add_ciao_libs(Type, Libs, LibDirs, CiaoLibs, CiaoLibDirs),
        convert_libs_to_options(CiaoLibs, CiaoLibDirs, Options),
        link_command(Type, Objects, ExecOrLibName, Options, CommandString),
        execute_string(CommandString),
        message("done!~n", []).


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
        multi_append([Linker, " ", DLOption, " -o ", LibName
                     | Options], Command1),
        multi_append([Command1|Objects], CommandString).

link_command(exec, Objects, Exec, OtherLibs, CommandString):- 
        get_arch(Arch),
        compiler(Arch, Compiler),
        link_exec_options(Arch, Options),
        multi_append([Compiler, " ", Options, " -o ", Exec
                      | OtherLibs], Command1),
        multi_append([Command1|Objects], CommandString).


convert_libs_to_options(Libs, LDirs, Options):-
        (
            get_arch('SUNSOL') ->
            add_options(LDirs, 0'R, Options, R_Options)
        ;
            R_Options = Options
        ),
        add_options(LDirs, 0'L, R_Options, MoreOpts),
        add_options(Libs, 0'l, MoreOpts, []).

add_options([], _Option, Options, Options).
add_options([What|Rest], OptChar, [[0' ,0'-,OptChar|What]|MoreOpt], Options):-
        add_options(Rest, OptChar, MoreOpt, Options).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% make_c_files_from_prolog(PrologFileNames, CFileNames): make a C
 %% file for each Prolog file, containing the QL code in a static
 %% array, its size and a function which loads it into the core of the
 %% ciao library. The names of the objects are taken from the file
 %% names. 
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_c_files_from_prolog([], []).
make_c_files_from_prolog([Pname|Pnames], [CFile|CFiles]):-
        make_one_c_file(Pname, CFile),
        make_c_files_from_prolog(Pnames, CFiles).

make_one_c_file(Name, CFileName):-
        concat_strings(Name, ".c", CFileName),
        concat_strings(Name, ".ql", QlFileName),
        concat_strings(Name, ".pl", PlFileName),
        name(PlAtom, PlFileName),
        name(QlAtom, QlFileName),
        name(CAtom, CFileName),
        make_ql_from_pl(PlAtom, QlAtom),
        make_c_from_ql(QlAtom, CAtom, Name).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Create a .ql file from a .pl file, if needed
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_ql_from_pl(PlFile, QlFile):- 
        younger_than(QlFile, PlFile) ->
        'SYSCALL'(fcompile2(PlFile))
 ;
        true.


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Create a .c file from a .ql file, if needed
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_c_from_ql(QlFile, CFile, Name):-
        younger_than(CFile, QlFile) ->
        message("Translating ~w to ~w... ", [QlFile, CFile]),
        entry_point_suffix(EPS),
        concat_strings(Name, EPS, EPName),
        concat_strings(Name, "_ql", QlArrayName),
        seeing(OldInput), 
        telling(OldInput),
        see(QlFile),
        tell(CFile),
        dump_header,
        dump_array_name(QlArrayName),
        translate_ql_code_to_c_syntax(QlArraySize),
        dump_epilogue(EPName, QlArrayName, QlArraySize),
        seen, 
        told,
        see(OldInput),
        tell(OldInput),
        message("done (~w bytes of ql code)~n", [QlArraySize])
 ;
        true.
make_c_from_ql(_QlFile, CFile, _Name):-
        seen, 
        told,
        format(user_error, "Panic! Error while creating C file ~w~n", [CFile]).

dump_header:-
        format("/* ~n", []),
        format("  This file has automatically been generated with~n",[]),
        format("  software produced by the folks at the CLIP lab~n",[]),
        format("  (C) Clip Lab 1996, 1997~n", []),
        format("  Reach 'em at http://www.clip.dia.fi.upm.es/~n", []),
        format("*/~n~n~n", []).

dump_array_name(Name):- format("static char ~s[] = {~n", [Name]).

translate_ql_code_to_c_syntax(Size):-
        get0(Code),
        translate_ql_code_to_c_syntax(Code, 1, Size).

translate_ql_code_to_c_syntax(-1, Size, Size):- !,
        format("    0~n};~n~n", []).
translate_ql_code_to_c_syntax(Code, SizeIn, SizeOut):-
        Code > -1,
        format("    ~w,~n", [Code]),
        get0(NewCode),
        SizeMid is SizeIn + 1,
        translate_ql_code_to_c_syntax(NewCode,SizeMid, SizeOut).

dump_epilogue(EPName, QlArrayName, QlSize):- 
        format("~s(){ciao_load_internal_ql(~s, ~w);}~n",
        [EPName, QlArrayName, QlSize]).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Compile a list of C files to .o files
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compile_c_files([], _Type, []).
compile_c_files([CFile|CFiles], Type, [" ", ObjFile |ObjFiles]):-
        compile_one_c_file(CFile, Type, ObjFile),
        compile_c_files(CFiles, Type, ObjFiles).

compile_one_c_file(CFile, Type, ObjFile):-
        append(Name, ".c", CFile),
        append(Name, ".o", ObjFile),
        name(CAtom, CFile),
        name(ObjAtom, ObjFile),
        (
            younger_than(ObjAtom, CAtom) ->
            message("Making object code file for ~s... ", [CFile]),
            compile_command(Type, COptions),
            append(COptions, CFile, CommandString),
            execute_string(CommandString),
            message(" done~n", [])
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
        append(C, " -g -c ", Command).
compile_command(lib,  Command):- 
        get_arch(Arch),
        dyn_compiling(Arch, Compiler, Options),
        multi_append([Compiler, " ", Options, " "], Command).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Miscellaneous predicates
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ciaolibdir(CiaoLibDir):-
        get_arch(Arch),
        name(Arch, ArchStr),
        installbase(Base),
        append(Base, ArchStr, CiaoLibDir).


concat_strings(A, B, C):- append(A, B, C).

 %% append([], L, L).
 %% append([X|Xs], Ys, [X|Zs]):- append(Xs, Ys, Zs).

 %% member(X, [X|_Xs]).
 %% member(X, [_X|Xs]):- member(X, Xs).

multi_append([], []).
multi_append([List|Lists], AllAppended):-
        multi_append(Lists, Partial),
        append(List, Partial, AllAppended).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% message(Format, List): identical to format/2, but writes only if
 %% the verbose falg is on.
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

message(F, L):-
        verbose(V),
        message(V, F, L).
message(on, F, L):- format(F, L), ttyflush.
message(off, _F, _L).


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

execute_string(CommandString):-
        name(Command, CommandString),
        format("~nCommand: ~w~n", [Command]),
        unix(shell(Command)).


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Database with options regarding compiling and linking for shared
 %% libraries in several systems 
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dyn_compiling(System, C, "-fPIC -g -c"):-
        System = 'LINUX',
        compiler(System, C).
dyn_compiling(System, C, "-fPIC -g -c"):-
        System = 'SUN4',
        compiler(System, C).
dyn_compiling(System, C, "-g -c"):-
        System = 'SUNSOL',
        compiler(System, C).


dyn_linking('LINUX', "gcc", "-shared").
dyn_linking('SUN4', "ld", "-assert pure-text").
dyn_linking('SUNSOL', "ld", "-G").


link_exec_options('LINUX', "-rdynamic").
link_exec_options('SUNSOL', "").
link_exec_options('SUN4', "").

compiler(_X, "gcc").


 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %% Oh, gosh... every system seems to have its own preferences in
 %% everything...
 %% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_lib_name(Arch, Lib, Libname):-
        lib_suffix(Arch, Suffix),
        append(Lib, Suffix, Libname).

lib_suffix('SUN4', ".so.1.0").
lib_suffix('SUNSOL', ".so").
lib_suffix('LINUX', ".a").
