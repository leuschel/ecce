:- module(internals, [
        load_lib/2, load_so/2, load_po/1, dynlink/2, dynunlink/1,
        initialize_module/1, initialized/1,
        term_to_meta/2,
        module_concat/3,
        last_module_exp/5,
        goal_trans/2,
        '$atom_mode'/2, /* write.pl */
%        '$eng_call'/6,  % Concurrency hook
        '$bootversion'/0, '$open'/3,
        '$purge'/1, '$erase'/1, '$ptr_ref'/2, '$inserta'/2,
        '$insertz'/2, '$make_bytecode_object'/4, '$abolish'/1,
        '$compile_term'/2,'$instance'/3,
        '$define_predicate'/2, '$erase_clause'/1, '$clause_number'/2,
        '$compiled_clause'/4, '$empty_gcdef_bin'/0, '$set_property'/2,
        '$ddt'/1, '$qread'/2, '$push_qlinfo'/0, '$pop_qlinfo'/0,
        '$prompt'/2, '$frozen'/2, '$defrost'/2, '$setarg'/4,
        '$undo_goal'/1, 
        '$exit'/1,
        '$unknown'/2,
        '$compiling'/2,
        '$ferror_flag'/2,
        '$quiet_flag'/2,
        '$ciao_version'/2,
%        '$prolog_radix'/2,
	'$constraint_list'/2, '$eq'/2,
        '$large_data'/3, '$interpreted_clause'/2,
        '$set_global_logical_var'/2, '$get_global_logical_var'/2,
        '$erase_atom'/1,
        /* system.pl */
        '$unix_popen'/3, '$exec'/8,
        '$unix_argv'/1,
        '$find_file'/8,
        '$format_print_float'/3, '$format_print_integer'/3, /* format.pl */
        '$runtime'/1,  '$usertime'/1,  '$systemtime'/1,  '$walltime'/1,
	'$runclick'/1, '$userclick'/1, '$systemclick'/1, '$wallclick'/1,
        '$userclockfreq'/1, '$systemclockfreq'/1, '$wallclockfreq'/1,
        '$termheap_usage'/1, '$envstack_usage'/1,
        '$trail_usage'/1, '$choice_usage'/1, '$stack_shift_usage'/1,
        '$internal_symbol_usage'/1, '$program_usage'/1, '$total_usage'/1,
        '$gc_mode'/2, '$gc_trace'/2, '$gc_margin'/2, '$gc_usage'/1,
        '$current_predicate'/2, '$predicate_property'/3,
        '$current_clauses'/2, '$first_instance'/2, '$current_instance'/5,
        '$emulated_clause_counters'/4, '$counter_values'/3,
        '$close_predicate'/1, '$open_predicate'/1, '$unlock_predicate'/1,
        '$reset_counters'/2, poversion/1,
	initialization/1,
         % JF: will be moved to a different module when possible...
	 filetype/3,
	 po_filename/2,
	 itf_filename/2,
	 so_filename/2,
	 asr_filename/2,
	 product_filename/3,
	 find_po_filename/2,
	 find_so_filename/2,
	 find_pl_filename/4,
	 find_c_filename/4,
	 opt_suff/1
        ],
	[assertions]).

:- use_module(user, [main/0, main/1, aborting/0]).

:- comment(title,"Engine Internal Predicates").  

:- comment(module,"

This library lists a set of internal predicates (written in C) used by
the system code. They should not be used in user code. The file itself
provides handles for the module system into the internal definitions.

").

:- use_module(engine(hiord_rt), ['SYSCALL'/1, '$nodebug_call'/1, '$meta_call'/1]).

:- impl_defined([dynlink/2, dynunlink/1,
        '$atom_mode'/2, 
        % '$eng_call'/6,
        '$bootversion'/0, '$open'/3, '$purge'/1, '$erase'/1, '$ptr_ref'/2,
        '$inserta'/2, '$insertz'/2, '$make_bytecode_object'/4, '$abolish'/1,
        '$compile_term'/2, '$instance'/3, '$define_predicate'/2,
        '$erase_clause'/1, '$clause_number'/2, '$compiled_clause'/4,
        '$empty_gcdef_bin'/0, '$set_property'/2, '$ddt'/1, '$qread'/2,
        '$push_qlinfo'/0, '$pop_qlinfo'/0, '$prompt'/2, '$frozen'/2,
        '$defrost'/2, '$setarg'/4, '$undo_goal'/1, 
	'$exit'/1, '$unknown'/2, '$compiling'/2,
        '$ferror_flag'/2, '$quiet_flag'/2, '$ciao_version'/2,
%	'$prolog_radix'/2,
	'$constraint_list'/2, '$eq'/2,
        '$large_data'/3, '$interpreted_clause'/2, '$unix_popen'/3,
        '$exec'/8, '$unix_argv'/1, '$find_file'/8,
	'$path_is_absolute'/1, '$expand_file_name'/2,
        '$format_print_float'/3, '$format_print_integer'/3, 
        '$set_global_logical_var'/2, '$get_global_logical_var'/2,
        '$erase_atom'/1,
        '$runtime'/1,  '$usertime'/1,  '$systemtime'/1,  '$walltime'/1,
	'$runclick'/1, '$userclick'/1, '$systemclick'/1, '$wallclick'/1,
        '$userclockfreq'/1, '$systemclockfreq'/1, '$wallclockfreq'/1,
        '$termheap_usage'/1, '$envstack_usage'/1, '$trail_usage'/1,
        '$choice_usage'/1, '$stack_shift_usage'/1, '$program_usage'/1,
        '$internal_symbol_usage'/1, '$total_usage'/1,
        '$gc_mode'/2, '$gc_trace'/2, '$gc_margin'/2, '$gc_usage'/1,
        '$current_predicate'/2, '$predicate_property'/3, '$current_clauses'/2,
        '$first_instance'/2, '$current_instance'/5,
        '$emulated_clause_counters'/4, '$counter_values'/3,
        '$close_predicate'/1, '$open_predicate'/1, '$unlock_predicate'/1,
        '$reset_counters'/2, main_module/1]).

%:- primitive_meta_predicate('$eng_call'(goal, ?, ?, ?, ?, ?)).

:- data all_loaded/0.

:- use_module(engine(debugger_support), ['$debugger_mode'/0, '$debugger_state'/2]).
%:- use_module(library('debugger/debugger'), ['$debugger_mode'/0, '$debugger_state'/2]).

boot:-
        setup_paths,
        ( '$load_libs' ; true ),
	initialize_debugger_state,
	initialize_global_vars,
	initialize,
        asserta_fact(all_loaded),
        gomain.
boot:-
        message(error,'Predicates user:main/0 and user:main/1 undefined, exiting...'),
        halt(1).

gomain :-
        '$predicate_property'('user:main',_,_), !,
	( '$nodebug_call'(main) -> true ; global_failure ).
gomain :-
        '$predicate_property'('user:main'(_),_,_), !,
        current_prolog_flag(argv, Args),
        ( '$nodebug_call'(main(Args)) -> true ; global_failure ).

global_failure :-
        message('{Program ended with failure}'),
        halt(2).

reboot:-
        all_loaded,
	initialize_debugger_state,
	initialize_global_vars,
	reinitialize,
	'$nodebug_call'(aborting), !.

initialize :-
	initialize_debugger_state,
	initialize_global_vars,
	initialize_2.

initialize_2 :-
	main_module(M),
        initialize_module(M),
	fail.
initialize_2.

% hack: since '$debugger_state' loads a global variable with a reference
% to a heap term, once executed you should not fail...
initialize_debugger_state :-
	'$current_module'(debugger), !,
	'SYSCALL'('debugger:initialize_debugger_state').
initialize_debugger_state.

:- data initialized/1.

initialize_module(M) :- current_fact(initialized(M)), !.
initialize_module(M) :- asserta_fact(initialized(M)),
                        do_initialize_module(M).

do_initialize_module(M) :-
        '$u'(M, N),
        initialize_module(N),
        fail.
do_initialize_module(M) :-
        '$initialization'(M),
        fail.
do_initialize_module(_).

reinitialize:-
	'$on_abort'(_),
	fail.
reinitialize.

% warp internal predicate, as requested by Jose Manuel
initialization(M) :- '$initialization'(M). 

% ---------------------------------------------------------------------------
% Backtrackable global variables
% --jfran

initialize_global_vars :-
	'$global_vars_set_root'('$glb'(0,0,0,0,0,0,0,0,
	                               0,0,0,0,0,0,0,0,
				       0,0,0,0,0,0,0,0,
				       0,0,0,0,0,0,0,0)).

:- export('$global_vars_set'/2).
'$global_vars_set'(I, X) :-
        '$global_vars_get_root'(R),
        '$setarg'(I, R, X, on).
%       '$setarg'(I, R, X, true).

:- export('$global_vars_get'/2).
'$global_vars_get'(I, X) :-
        '$global_vars_get_root'(R),
        arg(I, R, X).

:- impl_defined('$global_vars_get_root'/1).
:- impl_defined('$global_vars_set_root'/1).

% ---------------------------------------------------------------------------

:- initialization(fail).
:- on_abort(fail).

control_c_handler :- throw(control_c).

%% Module expansion

% Called from engine(mexpand)
uses_runtime_module_expansion.
ciaopp_expansion :- fail.

:- data goal_trans/2.

:- include(mexpand).
mexpand_meta_args(M, P, Primitive) :-
	'$meta_args'(M, P),
	( '$primitive_meta_predicate'(P, M) ->
	    Primitive = true
	; Primitive = fail
	).
mexpand_imports(M, IM, F, N, EM) :-
	'$imports'(M, IM, F, N, EM).
mexpand_defines(M, F, N) :-
	'$defines'(M, F, N).
mexpand_multifile(M, F, N) :-
	'$multifile'(M, F, N).

redefining(_,_,_). % Avoid imported_needs_qual warnings

module_warning(not_defined(F, N, M)) :- !,
        ( '$unknown'(fail,fail) -> true
        ; message(warning, ['Predicate ',~~(F/N),' undefined in module ',M])
        ).
module_warning(not_imported(F, N, M, QM)) :- !,
        message(error, ['Bad module qualification of ',~~(F/N),
                        ', module ',M,
                        ' does not import the predicate from module ',QM]).
module_warning(bad_pred_abs(PA)) :- !,
        message(error, ['Bad predicate abstraction ',~~(PA),
                          ' : head functor should be ''''']).
module_warning(big_pred_abs(PA, N)) :- !,
        message(error, ['Predicate abstraction ',~~(PA),
                          ' has too many arguments: should be ',N]).
module_warning(short_pred_abs(PA, N)) :- !,
        message(error, ['Predicate abstraction ',~~(PA),
                          ' has too few arguments: should be ',N]).

:- pred term_to_meta/2 # "Transforms a normal term to a meta-term.".

term_to_meta(X, '$:'(X)).

% These two backwards compatibility
last_module_exp(T, Type, M, QM, NT) :-
        rt_module_exp(T, Type, M, QM, true, NT).

mid_module_exp(T, Type, M, QM, NT) :-
        rt_module_exp(T, Type, M, QM, fail, NT).



rt_module_exp(V, _Type, _M, _QM, _Pr, V) :- var(V), !.
rt_module_exp(T, _Type, _M, _QM,  Pr, NT) :-
        T = '$:'(Tx), !, % already expanded
        ( Pr = true -> NT = Tx ; NT = T).
rt_module_exp(QM:T, Type, M,_QM, Pr, NT) :- !,
        ( var(QM) ->
            meta_expansion_type(Type, T, M, _, Pr, NT, no, no)
        ; rt_module_exp(T, Type, M, QM, Pr, NT)
        ).
rt_module_exp(T, Type, M, QM, Pr, NT) :-
        do_module_exp(QM, T, M, Pr, Type, NT), !.
rt_module_exp(T,_Type,_M,_QM,_Pr, T).

do_module_exp(QM, T, M, Primitive, Type, NT) :-
        nonvar(QM),
        functor(QM, XM, 1), !,
%        accessible_in(M, XM, mod_exp, 5),
        arg(1, QM, XQM),
        atom_concat(XM, ':mod_exp', PX), % XM:mod_exp/5 makes the expansion
        functor(GEXP, PX, 5),
        '$predicate_property'(GEXP, _, _),
        arg(1,GEXP,Type),
        arg(2,GEXP,T),
        arg(3,GEXP,M),
        arg(4,GEXP,XQM),
        arg(5,GEXP,XT),
        '$meta_call'(GEXP),
        term_to_meta_or_primitive(Primitive, XT, NT).
do_module_exp(QM, T, M, Primitive, Type, NT) :-
	meta_expansion_type(Type, T, M, QM, Primitive, NT, no, no).

module_concat(user(_), X0, X) :- !,
        module_concat(user, X0, X).
module_concat(Module, X0, X) :-
	X0 =.. [F0|Args],
        atom(F0), !,
	atom_concat(Module, ':', Mc),
	atom_concat(Mc, F0, F),
	X =.. [F|Args].
module_concat(_, X, X). % If a number, do not change to complain later

%------ call with continuations -----%
% Called from within the emulator, as possible boot goal for a wam

call_with_cont([](Goal, OnSuccess, _OnFailure)):-
        'SYSCALL'(Goal),
        'SYSCALL'(OnSuccess).
call_with_cont([](_Goal_, _OnSuccess, OnFailure)):-
        'SYSCALL'(OnFailure).

%------ load_lib ------%

load_lib(Module,_File) :-
        '$current_module'(Module), !.
load_lib(Module, File) :- % loads both .so and .po - JFMC
        prolog_flag(fileerrors, OldFE, off),
        ( find_pl_filename(File, _, Base, _),
	  so_filename(Base, SoName),
	  file_exists(SoName, 0),
	    dynlink(SoName, Module),
%            assertz_fact(current_module(Module)),
	    fail
	; true
	),
        ( find_po_filename(File, PoName),
            poload(PoName),
            ldlibs(Module),
	    fail
        ; true
	),
        set_prolog_flag(fileerrors, OldFE),
        check_module_loaded(Module, File).

ldlibs(X) :- '$ldlibs'(X).

%------ load_lib for lazy load ------%

load_lib_lazy(Module,_File) :-
        '$current_module'(Module), !.
load_lib_lazy(Module, File) :- % loads both .so and .po - JFMC
        del_stumps(Module),
        prolog_flag(fileerrors, OldFE, off),
        ( find_po_filename(File, PoName),
            poload(PoName),
	    fail
        ; true
	),
	( find_so_filename(File, SoName),
	    dynlink(SoName, Module),
%            assertz_fact(current_module(Module)),
	    fail
        ; true
	),
        set_prolog_flag(fileerrors, OldFE),
        retractall_fact(initialized(Module)),
        initialize_module(Module),
        check_module_loaded(Module, File).

check_module_loaded(Module,_File) :- '$current_module'(Module), !.
check_module_loaded(_Module,File) :-
        message(error,['library ',File,' not found, exiting...']),
        halt(1).

:- multifile stump/2.
:- data stump/2.

del_stumps(Module) :-
        retract_fact(stump(Module, Pred)),
        '$abolish'(Pred),
        fail.
del_stumps(_).

%------ low-level loading of objects ------%

poload(AbsName) :-
	'$push_qlinfo',
        '$open'(AbsName, r, Stream),            % Gives errors
	'$qread'(Stream, Version),
	poversion(Version), !,
	repeat,
	  '$qread'(Stream, Goal),
          (   Goal= -1
          ;   'SYSCALL'(Goal), fail
          ), !,
	'$pop_qlinfo',
	close(Stream).
poload(AbsName) :-
        message(error,[AbsName,' - wrong .po version number']),
        halt(1).

load_so(Module, File) :-
	find_so_filename(File, SoName),
        dynlink(SoName, Module).

load_po(File) :-
	find_po_filename(File, PoName),
        poload(PoName).

poversion(version(67)).

% ---------------------------------------------------------------------------
% JF: New absolute file name library. I need it to fix some pending issues
% of the foreign interface and future problems with the compilation to C

% A product is any output of a compilation 
% A source is the input of a compilation

%------ paths ------%

:- multifile file_search_path/2.
:- multifile library_directory/1.

:- dynamic file_search_path/2.
:- dynamic library_directory/1.

file_search_path(library, Lib) :- library_directory(Lib).
file_search_path(.,.).

setup_paths :-
        ciaolibdir(Path),
        atom_concat(Path,'/lib',LibPath),
        assertz_fact(library_directory(LibPath)),
        atom_concat(Path,'/library',LibraryPath),
        assertz_fact(library_directory(LibraryPath)),
        atom_concat(Path,'/contrib',ContribPath),
        assertz_fact(library_directory(ContribPath)),
        atom_concat(LibPath, '/engine', Engine),
        assertz_fact(file_search_path(engine, Engine)).

%JF: filename for some type of files
po_filename(Base, Name) :-
	product_filename(prolog_object, Base, Name).
itf_filename(Base, Name) :-
	product_filename(prolog_itf, Base, Name).
asr_filename(Base, Name) :-
	product_filename(prolog_assertion, Base, Name).
so_filename(Base, Name) :-
	product_filename(gluecode_so, Base, Name).

% JF: Name of a file
product_filename(Type, Base0, Name) :-
%	( Type = prolog_object -> display(po_filename(Base0)), nl ; true ),
	filetype(Type, Ext, ArchDep),
	( 
            ArchDep = noarch ->
	    Suffix = Ext
	; 
            get_os_arch_suffix(OsArchSuffix),
            glue_suffix(Type, GlueSuffix),
            atom_concat(OsArchSuffix, GlueSuffix, Suffix0),
            atom_concat(Suffix0, Ext, Suffix)
	),
	translate_base(Base0, Base),
	atom_concat(Base, Suffix, Name),
%	( Type = prolog_object -> display(po_filename_2(Name)), nl ; true ).
	true.

% %TO DEACTIVATE
translate_base(Base, Base) :- !.
% %TO ACTIVATE
translate_base(Base, Base2) :-
	atom_codes(Base, Codes),
	translate_base_2(Codes, Codes2),
	Codes3 = "/tmp/ciaobin/"||Codes2,
	atom_codes(Base2, Codes3),
	display(user_error, Base), nl(user_error),
	display(user_error, Base2), nl(user_error).

translate_base_2("/"||Xs0, "D_"||Xs) :- !,
	translate_base_2(Xs0, Xs).
translate_base_2([X|Xs0], [X|Xs]) :- !,
	translate_base_2(Xs0, Xs).
translate_base_2([], []).

find_so_filename(File, Abs) :-
        get_os_arch_suffix(OsArchSuffix),
        my_absolute_file_name(File, OsArchSuffix, '.so', '.', Abs, Base, _),
        Abs \== Base. % Has .so extension

get_os_arch_suffix(OsArchSuffix) :-
        get_os(Os),
        get_arch(Arch),
        atom_concat(Os, Arch, OsArch),
        atom_concat('_', OsArch, OsArchSuffix).

find_po_filename(File, Abs) :-
	opt_suff(Opt),
        my_absolute_file_name(File, Opt, '.po', '.', Abs, Base, _),
	Abs \== Base. % Has .po extension

find_pl_filename(File, PlName, Base, Dir) :-
% jf-TODO: if file is a path, ok... but if file is a module desc,
% this is not correct (= in ciao 1.9)
	atom(File), !,
        opt_suff(Opt),
	( my_find_file('.', File, Opt, '.pl', true, PlName, Base, Dir) ->
	    true
	; my_find_file('.', File, Opt, '', true, PlName, Base, Dir)
	).
find_pl_filename(File, PlName, Base, Dir) :- 
        opt_suff(Opt),
	my_absolute_file_name(File, Opt, '.pl', '.', PlName, Base, Dir).

find_c_filename(File, CName, Base, Dir) :- 
	my_absolute_file_name(File, [], '.c', '.', CName, Base, Dir).

%:- true pred absolute_file_name(+sourcename,+atm,+atm,+atm,-atm,-atm,-atm).

my_absolute_file_name(Spec, Opt, Suffix, _CurrDir, AbsFile, AbsBase, AbsDir) :-
        nonvar(Spec),
        functor(Spec, Alias, 1),
        arg(1,Spec,Name),
        atom(Name), !,
        (
            file_search_path(Alias, Dir),
            atom(Dir),
            my_find_file(Dir, Name, Opt, Suffix, true, AbsFile, AbsBase, AbsDir) ->
                true
        ;
            (
                '$ferror_flag'(on, on) ->
                throw(error(existence_error(source_sink,Spec),
                            absolute_file_name/7-1))
            ;
                fail
            )
        ).
my_absolute_file_name(Name, Opt, Suffix, CurrDir, AbsFile, AbsBase, AbsDir) :-
        atom(Name), !,
        my_find_file(CurrDir, Name, Opt, Suffix, _, AbsFile, AbsBase, AbsDir).
my_absolute_file_name(X, _, _, _, _, _, _) :-
        throw(error(domain_error(source_sink, X), absolute_file_name/7-1)).

remove_last_slash(Path0, Path) :-
	atom_concat(Path, '/', Path0), !.
remove_last_slash(Path, Path).

% TOpt is 0 if Path does not exists or is a directory,
% else, it is the modification time
modif_time0_nodir(Path, Time) :-
	( my_modif_time0(Path, Time),
	  \+ Time = 0,
	  \+ is_dir(Path) ->
	    true
	; Time = 0
	).

my_modif_time0(Path, Time) :-
        prolog_flag(fileerrors, OldFE, off),
        ( file_properties(Path, [], [], T, [], []), !
        ; T = 0
        ),
        set_prolog_flag(fileerrors, OldFE),
        Time = T.

:- use_module(library(system)).

is_dir(Path) :- !,
        prolog_flag(fileerrors, OldFE, off),
        file_properties(Path, directory, [], [], [], []),
        set_prolog_flag(fileerrors, OldFE).

my_find_file(LibDir0, Path0, Opt, Suffix, Exists, AbsFile, AbsBase, AbsDir) :-
	( atom_concat(Path1, Suffix, Path0) -> % remove the extension
	    true
	; Path1 = Path0
	),
	compose_paths(LibDir0, Path1, Path2),
	'$expand_file_name'(Path2, Path),
	( my_find_file_exists(Path, Opt, Suffix, AbsFile) ->
	    Exists = true,
	    atom_concat(AbsBase, Suffix, AbsFile) % without extension
	; my_find_file_no_exists(Path, AbsFile) ->
	    Exists = false,
	    AbsBase = AbsFile
	; fail
	),
	split_dir_name(AbsFile, AbsDir, _). % without last filename

compose_paths(Path0, Rel, Path) :-
        ( '$path_is_absolute'(Rel) ->
	    Path = Rel
	; remove_last_slash(Path0, Path1),
	  atom_concat(Path1, '/', Path2),
	  atom_concat(Path2, Rel, Path)
	).

% 1) newer non-directory of {path+opt+suffix, path+suffix}
% 2) path, if path does not exists
% 3) path, if a non-directory
% 4) recursive call with duplicated file name in path, if path is a directory

my_find_file_exists(Path, Opt, Suffix, AbsFile) :-
	( my_find_file_3(Path, Opt, Suffix, AbsFile) ->
	    true
	; is_dir(Path) ->
	    duplicate_dir_name(Path, DupPath), % search inside
	    my_find_file_3(DupPath, Opt, Suffix, AbsFile)
	; fail
	).

my_find_file_no_exists(Path, AbsFile) :-
	( is_dir(Path) ->
	    duplicate_dir_name(Path, DupPath), % search inside
	    \+ file_exists(DupPath, 0),
	    AbsFile = DupPath
	; \+ file_exists(Path, 0),
	  AbsFile = Path
	).

% newer non-directory of {path+opt+suffix, path+suffix}
my_find_file_3(Path, Opt, Suffix, AbsFile) :-
	( \+ Opt = '', atom_concat(Path, Opt, PathOpt),
	  atom_concat(PathOpt, Suffix, PathOptSuffix) ->
	    my_modif_time0(PathOptSuffix, TOpt)
        ; TOpt = 0
	),
	( /*\+ Suffix = '',*/ atom_concat(Path, Suffix, PathSuffix) ->
	    my_modif_time0(PathSuffix, TPri)
        ; TPri = 0
	),
	( TPri > TOpt -> % path+suffix exists, path+opt+suffix older|absent
	    AbsFile = PathSuffix
	; TOpt > 0 -> % newer path+opt+suffix exists
	    AbsFile = PathOptSuffix
	; fail
	).

duplicate_dir_name(Path, Path2) :-
	split_dir_name(Path, _, Name),
	atom_concat(Path, '/', Path1),
	atom_concat(Path1, Name, Path2).

split_dir_name(Path, Dir, Name) :-
	atom_codes(Path, Codes),
	reverse(Codes, RCodes),
	split_dir_name_2(RCodes, RDirCodes, RNameCodes),
	reverse(RDirCodes, DirCodes),
	reverse(RNameCodes, NameCodes),
	atom_codes(Dir, DirCodes),
	atom_codes(Name, NameCodes).

reverse(Xs,Ys):- reverse_2(Xs,[],Ys).

reverse_2([], L, L).
reverse_2([E|Es],L,R) :- reverse_2(Es,[E|L],R).

split_dir_name_2("/"||Dir, Dir, []) :- !.
split_dir_name_2([X|Path], Dir, [X|Name]) :- !,
	split_dir_name_2(Path, Dir, Name).
split_dir_name_2([], [], []).

% JF: Information about file types involved in compilation
% - THIS IS NOT A MIME-like LIST: 
filetype(prolog_object,     '.po',  noarch).
filetype(prolog_itf,        '.itf', noarch).
filetype(prolog_assertion,  '.asr', noarch).
filetype(gluecode_so,       '.so',  arch).
filetype(gluecode_c,        '.c',   arch).
filetype(gluecode_o,        '.o',   arch).
filetype(gluecode_unique_o, '.o',   arch).

% MCL: not all arch-dep files need to have the '_glue' marker
glue_suffix(gluecode_so,       '').
glue_suffix(gluecode_c,        '_glue').
glue_suffix(gluecode_o,        '_glue').
glue_suffix(gluecode_unique_o, '_glue').


:- data opt_suff/1.

opt_suff('_opt').

%------ attributed variables ------%
:- include( attributed_variables ).


%------ internal builtin errors ------%

% Called from within the emulator
error(Type, PredName, PredArity, Arg, Culprit) :-
%        display('In Error'(Type, Culprit)), nl,
        error_term(Type, Culprit, Error_Term),
%        display(error_term_is(Error_Term)), nl,
        where_term(Arg, PredName, PredArity, Where_Error),
        throw(error(Error_Term, Where_Error)).

in_range(Type, Code, WhichWithinType):-
        range_per_error(Range),
        error_start(Type, Section),
        Start is Section * Range,
        Code >= Start,
        Code < Start + Range,
        WhichWithinType is Code - Start.

error_term(  1, _, instantiation_error) :- !.
error_term(Code, _, system_error) :-   in_range(system, Code, _), !.
error_term(Code, _, syntax_error) :-   in_range(syntax, Code, _), !.
error_term(Code, _, resource_error) :- in_range(res,    Code, _), !.
error_term(Code, _, user_error) :-     in_range(user,   Code, _), !.
error_term(N, Culprit, evaluation_error(Type, Culprit)) :-
        in_range(eval, N, Code), !,
        evaluation_code(Code, Type).
error_term(N, Culprit, representation_error(Type, Culprit)) :-
        in_range(repres, N, Code), !,
        representation_code(Code, Type).
error_term(N, Culprit, type_error(Type, Culprit)) :-
        in_range(type, N, Code),
        type_code(Code, Type).
error_term(N, Culprit, domain_error(Type, Culprit)) :-
        in_range(dom, N, Code),
        domain_code(Code, Type).
error_term(N, Culprit, existence_error(Type, Culprit)) :-
        in_range(exist, N, Code),
        existence_code(Code, Type).
error_term(N, Culprit, permission_error(Object, Permission, Culprit)) :-
        in_range(perm, N, Code),
        get_obj_perm(Code,Obj,Per),
        permission_type_code(Per, Permission),
        permission_object_code(Obj, Object).


%% Check error type and return get Code for every class of error.  This should
%% be made more modularly (i.e., with an C interface - but is it worth?)

 %% is_evaluation_error(N,Code) :-     N>120, N<126, Code is N-121.
 %% 
 %% is_representation_error(N,Code) :- N>114, N<121, Code is N-115.
 %% 
 %% is_type_error(N,Code) :-           N>1, N<15, Code is N-2.
 %% 
 %% is_domain_error(N,Code) :-         N>14, N<32, Code is N-15.
 %% 
 %% is_existence_error(N,Code) :-      N>31, N<35, Code is N-32.
 %% 
 %% is_permission_error(N,Code) :-     N>34, N<115, Code is N-35.

get_obj_perm(Code, Obj, Perm) :-
        Obj is Code mod 10,
        Perm is Code // 10.
             

 %% culprit_stream([], S) :- !, current_input(S).
 %% culprit_stream(S,S).

%% This is the Prolog counterpart of the definitions in support.h.  Please 
%% have a look there!

range_per_error(100).

error_start(inst,   0).
error_start(type,   1).
error_start(dom,    2).
error_start(exist,  3).
error_start(perm,   4).
error_start(repres, 5).
error_start(eval,   6).
error_start(res,    7).
error_start(syntax, 8).
error_start(system, 9).
error_start(user,   10).

type_code(0, atom).
type_code(1, atomic).
type_code(2, byte).
type_code(3, character).
type_code(4, compound).
type_code(5, evaluable).
type_code(6, in_byte).
type_code(7, integer).
type_code(8, list).
type_code(9, number).
type_code(10, predicate_indicator).
type_code(11, variable).
type_code(12, callable).


domain_code(0, character_code_list).
domain_code(1, source_sink).
domain_code(2, stream).
domain_code(3, io_mode).
domain_code(4, not_empty_list).
domain_code(5, not_less_than_zero).
domain_code(6, operator_priority).
domain_code(7, prolog_flag).
domain_code(8, read_option).
domain_code(9, flag_value).
domain_code(10, close_option).
domain_code(11, stream_option).
domain_code(12, stream_or_alias).
domain_code(13, stream_position).
domain_code(14, stream_property).
domain_code(15, write_option).
domain_code(16, operator_specifier).



existence_code(0, procedure).
existence_code(1, source_sink).
existence_code(2, stream).








permission_type_code(0, access).
permission_type_code(1, creation).
permission_type_code(2, input).
permission_type_code(3, modification).
permission_type_code(4, opening).
permission_type_code(5, output).
permission_type_code(6, reposition).





permission_object_code(0, binary_stream).
permission_object_code(1, source_sink).
permission_object_code(2, stream).
permission_object_code(3, text_stream).
permission_object_code(4, flag).
permission_object_code(5, operator).
permission_object_code(6, past_end_of_stream).
permission_object_code(7, private_procedure).
permission_object_code(8, static_procedure).

representation_code(0, character_code_list).
representation_code(1, in_character_code).
representation_code(2, max_arity).
representation_code(3, character).
representation_code(4, max_integer).
representation_code(5, min_integer).
representation_code(6, character_code).


evaluation_code(0, float_overflow).
evaluation_code(1, int_overflow).
evaluation_code(2, undefined).
evaluation_code(3, underflow).
evaluation_code(4, zero_divisor).

where_term(0, PredName, PredArity, PredName/PredArity) :- !.
where_term(Arg, PredName, PredArity, PredName/PredArity-Arg).
