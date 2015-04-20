/* $Header: /tmp_mnt/usr/share/src/local/sicstus/sicstus0.7/Library/RCS/foreign.pl,v 1.5 90/08/28 15:16:23 matsc Exp Locker: matsc $ */
/* Copyright (C) 1988, Swedish Institute of Computer Science. */

% SICStus Prolog: Foreign function interface.

:- public
	load_foreign_files/2,
	prepare_foreign_files/1.

%:- use_module(streams).

:- syntax([dcg,assertions]).

load_foreign_files(FileList, LibraryList) :-
	(   collect_functions(FileList, AbsFileList, FuncList, DeclList) ->
	    true
	;   format(user_error,
	           '{Warning: ~q - illegal usage}~n',
		   [load_foreign_files(FileList, LibraryList)]),
	    fail
        ),
	(   '$foreign_base'(AddressBase),
	    incore_stubs(DeclList, AddressBase),
	    '$load_foreign_files'(AbsFileList, LibraryList, FuncList,
	                          DeclList) -> true
	;   format(user_error,
	           '{Warning: load of foreign files not completed}~n', []),
	    fail
	).

prepare_foreign_files(FileList) :-
	'$runtime'([T0|_]),
	(   collect_functions(FileList, _AbsFileList, FuncList, DeclList) ->
	    true
	;   format(user_error,
	           '{Warning: ~q - illegal usage}~n',
		   [prepare_foreign_files(FileList)]),
	    fail
        ),
	(   '$prepare_foreign_files'(FuncList, DeclList, 'flinkage.c') ->
	    '$runtime'([T1|_]),
	    format(user, '{flinkage.c generated, ~d msec}~n', [T1-T0])
	;   format(user_error,
	           '{Warning: prepare of foreign files not completed}~n', []),
	    fail
	).

collect_functions([], [], [], []).
collect_functions([File|Files], [AbsFile|AbsFiles], Funcs0, Decls0) :-
	(   absolute_file_name(File, '', '.o', '.', AbsFile, _, _) -> true
	;   format(user_error, 
		   '{Warning: ~q - file not found}~n', [File]),
	    fail
	),
	(   
	    call_user_def(foreign_file(File, Functions)) ->
	    collect_declarations(Functions, Funcs0, Funcs, Decls0, Decls)
	;   format(user_error, 
		   '{Warning: ~q - file not declared}~n', [File]),
	    fail
	),
        collect_functions(Files, AbsFiles, Funcs, Decls).

collect_declarations([], Funcs, Funcs, Decls, Decls).
collect_declarations([F|Fs], [F|Funcs0], Funcs, [D|Decls0], Decls) :-
	(   atom(F), call_user_def(foreign(F, D)) -> true
	;   atom(F), call_user_def(foreign(F, c, D)) -> true
	;   format(user_error, 
	 	   '{Warning: ~q - C declaration not found}~n', [F]),
	    fail
	), 
	collect_declarations(Fs, Funcs0, Funcs, Decls0, Decls).

incore_stubs([], _).
incore_stubs([Decl|Decls], Address) :-
	nonvar(Decl), 
	functor(Decl, F, Ar), 
	f_stub(Decl, Address, Code, []), 
	asm_insns(Code, [], 0, Size, Tokens, []), 
	'$make_bytecode_object'(Size, 2, Tokens, Obj), 
	'$define_predicate'(F/Ar, unprofiled),
	'$compiled_clause'(F/Ar, Obj, unprofiled, f(2'11111,_)), !,
	Addresses is Address+1,
	incore_stubs(Decls, Addresses).
incore_stubs([Decl|_], _) :-
	format(user_error, 
	       '{Warning: ~q - illegal C declaration}~n', [Decl]),
	fail.

f_stub(Proc, Address) -->
	{   '$predicate_property'(Proc, _, Bits) ->
	    Bits/\16'1 =:= 0			% prop.public, xref nondet.c
        ;   true
        },
	{abolish_defined_in(Proc)},
	{functor(Proc, _, Arity)},
	f_join(Alloc0, Alloc),
	[ci_inarg(ValSize,ArglSize7)],
	f_stub_args(Proc, 0, Arity, 0, ValSize, 0, AuxSize, Ret0, Ret),
	{ArglSize is (Arity<<1)+1},
	{ArglSize7 is (ArglSize<<3)+7},
	{f_ensure_space(Arity, ValSize, ArglSize, AuxSize, Alloc0, Alloc)},
	[ci_call(ValSize,Address)],
	f_join(Ret0, Ret),
	[proceed].

f_ensure_space(Arity, S1, S2, S3) -->
	(   {S is S1+S2+S3, S>1152} -> [heapmargin_call(S,Arity)]
	;   {S is S1+128, S>1152} -> [heapmargin_call(S,Arity)]
	;   []
	).

f_stub_size(-string(N), 0, I) :- !, I is ((N-1)>>2)+1.
f_stub_size(+string(N), 0, I) :- !, I is ((N-1)>>2)+1.
f_stub_size(-float, 4, 3) :- !.
f_stub_size(-integer, 3, 1) :- !.
f_stub_size(-_, 0, 1).
f_stub_size(+_, 0, 0).

f_stub_args(_, A, A, VA, VA, AA, AA, S, S) --> !.
f_stub_args(Proc, A0, A, VA0, VA, AA0, AA, S0, S) -->
	{A1 is A0+1},
	{arg(A1, Proc, Spec0)},
	f_stub_args(Spec0, Spec, A0, S0, S1),
	{f_stub_size(Spec, VX, AX)},
	{VA1 is VA0+VX, AA1 is AA0+AX},
	f_stub_args(Proc, A1, A, VA1, VA, AA1, AA, S1, S).

f_stub_args(+Spec, +Spec, A0, S, S) -->
	[ci_inarg(A0,Val)],
	{f_stub_spec(Spec, Val)}.
f_stub_args(-Spec, -Spec, A0, [ci_retval(A0,Val)|S], S) -->
	[ci_outarg(A0,Val)],
	{f_stub_spec(Spec, Val)}.
f_stub_args([-Spec], -Spec, A0, [ci_retval(Ret,Val)|S], S) -->
	{f_stub_spec(Spec, Val0)},
	{Val0/\7 =:= 4 -> Val is Val0+1; Val=Val0},
	{Ret is A0+256}.

f_stub_spec(-, _) :- !, fail.
f_stub_spec(integer, 0).
f_stub_spec(float, 1).
f_stub_spec(atom, 2).
f_stub_spec(string, 3).
f_stub_spec(string(N), Val) :- f_stub_spec(N, 4, Val).
f_stub_spec(address, 6).
f_stub_spec(address(_), 6).

f_stub_spec(N, Type, Val) :-
	integer(N), N>=0, N<8192,
	Val is Type+(N<<3).

f_join(S0, S, S0, S).
