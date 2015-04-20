/*             Copyright (C)1990-2002 UPM-CLIP				*/

% SICStus Compiler Back-End Version 0.5 (shallow backtracking).
%
% ** Copyright (C) 1986 Swedish Institute of Computer Science.
% ** All rights reserved.

% Modifed to generate pwam instructions by R. Warren and M. Hermenegildo

asm_insns([ifshallow,neck(N),else,endif|Insns],Off0,Off,CInsns) --> !,
	{Off1 is Off0+2, collapse(Insns,CInsns,[])},
	[4],
	asm_cinsns([neck(N)|CInsns],_,Off1,Off).
asm_insns(Insns,Off0,Off,CInsns) -->
	{Off1 is Off0+2, collapse(Insns,CInsns,[])},
	[ElseOff],
	asm_cinsns(CInsns,ElseOff,Off1,Off).

asm_cinsns([],Off,Off,Off) --> [].
asm_cinsns([ifshallow|Is],ElseOff,Off0,Off) --> 
	asm_cinsns(Is,ElseOff,Off0,Off).
asm_cinsns([else|Is],Off1,Off0,Off) --> 
	{insn_format(branch(_),_,Opcode)},
	{Off1 is Off0+4},
	[Opcode, branch(Off0,EndifOff)], 
	asm_cinsns(Is,EndifOff,Off1,Off).
asm_cinsns([endif|Is],Off0,Off0,Off) --> 
	asm_cinsns(Is,_,Off0,Off).
asm_cinsns([label(Off1)|Is],XOff,Off0,Off) -->
	align_target(Off0,Off1),
	!, asm_cinsns(Is,XOff,Off1,Off).
asm_cinsns([I|Is],XOff,Off0,Off) --> 
	{insn_format(I,Fmt,Opcode)},
	asm_args(Fmt,I,Off0,Off1,Opcode),
	!, asm_cinsns(Is,XOff,Off1,Off).
asm_cinsns([I|Is],XOff,Off0,Off) -->
	{write(user,'Instruction '),
	 write(user,I),
	 write(user,' doesnt exist'),
	 nl(user)
        },
	asm_cinsns(Is,XOff,Off0,Off).


collapse([]) --> [].
collapse([Insn|Insns]) --> 
	collapse(Insn,Insns).

collapse_voids([unify_void|Insns0],Insns,I,N) :-
	!, I1 is I+1, collapse_voids(Insns0,Insns,I1,N).
collapse_voids(Insns,Insns,N,N).


collapse(ground(X),Insns) -->
	[ground_1(X)], !, collapse(Insns).
collapse(ground(X,Y),Insns) -->
	[ground_2(X,Y)], !, collapse(Insns).
collapse(ground(X,Y,Z),Insns) -->
	[ground_3(X,Y,Z)], !, collapse(Insns).

collapse(call(Y,X),Insns) -->
	!, collapse_call(call(Y,X),Insns,L,L).
collapse(call_or(Y,X),Insns) -->
	!, collapse_call(call_or(Y,X),Insns,L,L).
collapse(push_call(Y,X),Insns) -->
	!, collapse_call(push_call(Y,X),Insns,L,L).
collapse(put_x_void(X),Insns) -->
	!, collapse_call(put_x_void(X),Insns,L,L).
collapse(put_x_variable(Y,X),Insns) -->
	!, collapse_call(put_x_variable(Y,X),Insns,L,L).
collapse(put_y_value(Y,X),Insns) -->
	!, collapse_call(put_y_value(Y,X),Insns,L,L).
collapse(put_y_unsafe_value(Y,X),Insns) -->
	!, collapse_call(put_y_unsafe_value(Y,X),Insns,L,L).
collapse(call(X,Y),Insns) -->
	[call([],X,Y)], !, collapse(Insns).
collapse(call_or(X,Y),Insns) -->
	[call([],X,Y)], !, collapse(Insns).
collapse(push_call(X,Y),Insns) -->
	[push_call([],X,Y)], !, collapse(Insns).
collapse(execute_or(X),Insns) -->
	[execute(X)], !, collapse(Insns).
collapse(allocate,[Insn|Insns]) -->
	!, collapse_allocate(Insn,Insns).
collapse(deallocate,[Insn|Insns]) -->
	!, collapse_deallocate(Insn,Insns).
collapse(get_x_variable(To,From),[get_x_variable(To1,From1)|Insns]) -->
	[get_xvar_xvar(To,From,To1,From1)], !, collapse(Insns).
collapse(get_y_variable(To,From),[get_y_variable(To1,From1)|Insns]) -->
	[get_yvar_yvar(To,From,To1,From1)], !, collapse(Insns).
collapse(get_x_value(C,X),[proceed|Insns]) -->
	[get_x_value_proceed(C,X)], !, collapse(Insns).
collapse(get_y_value(C,X),[deallocate,proceed|Insns]) -->
	[get_y_value_return(C,X)], !, collapse(Insns).
collapse(get_constant(C,X),[proceed|Insns]) -->
	[get_constant_proceed(C,X)], !, collapse(Insns).
collapse(get_nil(X),[proceed|Insns]) -->
	[get_nil_proceed(X)], !, collapse(Insns).
collapse(get_constant_x0(C),[proceed|Insns]) -->
	[get_constant_x0_proceed(C)], !, collapse(Insns).
collapse(get_nil_x0,[proceed|Insns]) -->
	[get_nil_x0_proceed], !, collapse(Insns).
collapse(put_x_value(From,To),[put_x_value(From1,To1)|Insns]) -->
	[put_xval_xval(From,To,From1,To1)], !, collapse(Insns).
collapse(init(L),[call(M,S)|Insns]) -->
	[firstcall(L,M,S)], !, collapse(Insns).
collapse(init(L),[pcall(M,S,D)|Insns]) -->
	[init_pcall(L,M,S,D)], !, collapse(Insns).
collapse(init(L),[call_or(M,S)|Insns]) -->
	[firstcall(L,M,S)], !, collapse(Insns).
collapse(put_y_variable(Y,X),[put_y_variable(Y1,X1)|Insns]) -->
	[put_yvar_yvar(Y,X,Y1,X1)], !, collapse(Insns).
collapse(unify_constant(C),[proceed|Insns]) -->
	[unify_constant_proceed(C)], !, collapse(Insns).
collapse(unify_nil,[proceed|Insns]) -->
	[unify_nil_proceed], !, collapse(Insns).
collapse(unify_void,Insns0) -->
	{collapse_voids(Insns0,[deallocate,proceed|Insns],1,X)},
	[unify_void_return(X)], !, collapse(Insns).
collapse(unify_void,Insns0) -->
	{collapse_voids(Insns0,[Insn|Insns],1,X)},
	!, collapse_u2_void(Insn,Insns,X).
collapse(unify_x_variable(X),[Insn|Insns]) -->
	!, collapse_u2_xvar(Insn,Insns,X).
collapse(unify_y_variable(X),[Insn|Insns]) -->
	!, collapse_u2_yvar(Insn,Insns,X).
collapse(unify_x_value(X),[proceed|Insns]) -->
	[unify_x_value_proceed(X)], !, collapse(Insns).
collapse(unify_x_value(X),[Insn|Insns]) -->
	!, collapse_u2_xval(Insn,Insns,X).
collapse(unify_x_local_value(X),[proceed|Insns]) -->
	[unify_x_local_value_proceed(X)], !, collapse(Insns).
collapse(unify_x_local_value(X),[Insn|Insns]) -->
	!, collapse_u2_xlval(Insn,Insns,X).
collapse(unify_y_first_value(X),[Insn|Insns]) -->
	!, collapse_u2_yfval(Insn,Insns,X).
collapse(unify_y_value(X),[deallocate,proceed|Insns]) -->
	[unify_y_value_return(X)], !, collapse(Insns).
collapse(unify_y_value(X),[Insn|Insns]) -->
	!, collapse_u2_yval(Insn,Insns,X).
collapse(unify_y_local_value(X),[deallocate,proceed|Insns]) -->
	[unify_y_local_value_return(X)], !, collapse(Insns).
collapse(unify_y_local_value(X),[Insn|Insns]) -->
	!, collapse_u2_ylval(Insn,Insns,X).
collapse(cutb,[neck(_)|Insns]) -->
	[cutb_neck], !, collapse(Insns).
collapse(cute,[neck(_)|Insns]) -->
	[cute_neck], !, collapse(Insns).
collapse(cutb_x(A),[neck(_)|Insns]) -->
	[cutb_x_neck(A)], !, collapse(Insns).
collapse(cute_x(A),[neck(_)|Insns]) -->
	[cute_x_neck(A)], !, collapse(Insns).
collapse(cutb_x(A),[proceed|Insns]) -->
	[cut_proceed(A)], !, collapse(Insns).
collapse(cut_y(0),[deallocate,proceed|Insns]) -->
	[cut_ret], !, collapse(Insns).
collapse(I,Insns) -->				% default
	[I], collapse(Insns).

collapse_call_1([put_y_value(Y,I)|S0],S,I,L0,L) :-
	I1 is I+1, !, collapse_call_1(S0,S,I1,L0,[v(Y)|L]).
collapse_call_1([put_y_unsafe_value(Y,I)|S0],S,I,L0,L) :-
	I1 is I+1, !, collapse_call_1(S0,S,I1,L0,[u(Y)|L]).
collapse_call_1([Insn|S0],[Insn|S],I,L0,L) :- collapse_call_1(S0,S,I,L0,L).
collapse_call_1([],[],_,L,L).

collapse_call_2([put_y_value(Y,X)|Insns]) -->
	!, collapse_call_2_yval(Insns,Y,X).
collapse_call_2([put_y_unsafe_value(Y,X)|Insns]) -->
	!, collapse_call_2_yuval(Insns,Y,X).
collapse_call_2([I|Insns]) --> [I], collapse_call_2(Insns).
collapse_call_2([]) --> [].

collapse_call_2_yval([put_y_value(Y1,X1)|Insns],Y,X) -->
	[put_yval_yval(Y,X,Y1,X1)],
	!, collapse_call_2(Insns).
collapse_call_2_yval([put_y_unsafe_value(Y1,X1)|Insns],Y,X) -->
	[put_yval_yuval(Y,X,Y1,X1)],
	!, collapse_call_2(Insns).
collapse_call_2_yval([I|Insn],Y,X) --> [I], collapse_call_2_yval(Insn,Y,X).
collapse_call_2_yval([],Y,X) --> [put_y_value(Y,X)].

collapse_call_2_yuval([put_y_value(Y1,X1)|Insns],Y,X) -->
	[put_yuval_yval(Y,X,Y1,X1)],
	!, collapse_call_2(Insns).
collapse_call_2_yuval([put_y_unsafe_value(Y1,X1)|Insns],Y,X) -->
	[put_yuval_yuval(Y,X,Y1,X1)],
	!, collapse_call_2(Insns).
collapse_call_2_yuval([I|Insn],Y,X) --> [I], collapse_call_2_yuval(Insn,Y,X).
collapse_call_2_yuval([],Y,X) --> [put_y_unsafe_value(Y,X)].


collapse_call(put_x_void(X),[Insn|Insns],[put_x_void(X)|L0],L) -->
	!, collapse_call(Insn,Insns,L0,L).
collapse_call(put_x_variable(Y,X),[Insn|Insns],[put_x_variable(Y,X)|L0],L) -->
	!, collapse_call(Insn,Insns,L0,L).
collapse_call(put_y_value(Y,X),[Insn|Insns],[put_y_value(Y,X)|L0],L) -->
	!, collapse_call(Insn,Insns,L0,L).
collapse_call(put_y_unsafe_value(Y,X),[Insn|Insns],
	      [put_y_unsafe_value(Y,X)|L0],L) -->
	!, collapse_call(Insn,Insns,L0,L).
collapse_call(call(X,Y),Insns,[],L) -->
	{collapse_call_1(L,L1,0,Arg,[])},
	collapse_call_2(L1),
	[call(Arg,X,Y)],
	!, collapse(Insns).
collapse_call(call_or(X,Y),Insns,[],L) -->
	{collapse_call_1(L,L1,0,Arg,[])},
	collapse_call_2(L1),
	[call(Arg,X,Y)],
	!, collapse(Insns).
collapse_call(push_call(X,Y),Insns,[],L) -->
	{collapse_call_1(L,L1,0,Arg,[])},
	collapse_call_2(L1),
	[push_call(Arg,X,Y)],
	!, collapse(Insns).
collapse_call(deallocate,[execute(X)|Insns],[],L) -->
	{collapse_call_1(L,L1,0,Arg,[])},
	collapse_call_2(L1),
	[lastcall(Arg,X)],
	!, collapse(Insns).
collapse_call(deallocate,[execute_or(X)|Insns],[],L) -->
	{collapse_call_1(L,L1,0,Arg,[])},
	collapse_call_2(L1),
	[lastcall(Arg,X)],
	!, collapse(Insns).
collapse_call(Insn,Insns,[],L) -->
	collapse_call_2(L),
	collapse(Insn,Insns).
	 
collapse_allocate(init(_),[call(L,S)|Insns]) -->
	[initcall(L,S)], !, collapse(Insns).
collapse_allocate(init(A),[pcall(L,S,D)|Insns]) -->
	[allocate_pcall(A,L,S,D)], !, collapse(Insns).
collapse_allocate(init(_),[call_or(L,S)|Insns]) -->
	[initcall(L,S)], !, collapse(Insns).
collapse_allocate(choice_y(Y),Insns) -->
	[choice_yf(Y)], !, collapse(Insns).
collapse_allocate(put_y_variable(Y,X),[put_y_variable(Y1,X1)|Insns]) -->
	[put_yfvar_yvar(Y,X,Y1,X1)], !, collapse(Insns).
collapse_allocate(put_y_variable(Y,X),Insns) -->
	[put_y_first_variable(Y,X)], !, collapse(Insns).
collapse_allocate(get_y_variable(To,From),[get_y_variable(To1,From1)|Insns]) -->
	[get_yfvar_yvar(To,From,To1,From1)], !, collapse(Insns).
collapse_allocate(get_y_variable(X,Y),Insns) -->
	[get_y_first_variable(X,Y)], !, collapse(Insns).
collapse_allocate(unify_y_variable(X),[Insn|Insns]) -->
	!, collapse_u2_yfvar(Insn,Insns,X).

collapse_deallocate(execute_or(X),Insns) -->
	[lastcall([],X)], !, collapse(Insns).
collapse_deallocate(execute(X),Insns) -->
	[lastcall([],X)], !, collapse(Insns).
collapse_deallocate(proceed,Insns) -->
	[ret], collapse(Insns).

collapse_u2_yfvar(unify_void,Insns0,X) -->
	{collapse_voids(Insns0,Insns,1,Y)},
	[u2_yfvar_void(X,Y)], !, collapse(Insns).
collapse_u2_yfvar(unify_x_variable(Y),Insns,X) -->
	[u2_yfvar_xvar(X,Y)], !, collapse(Insns).
collapse_u2_yfvar(unify_y_variable(Y),Insns,X) -->
	[u2_yfvar_yvar(X,Y)], !, collapse(Insns).
collapse_u2_yfvar(unify_x_value(Y),Insns,X) -->
	[u2_yfvar_xval(X,Y)], !, collapse(Insns).
collapse_u2_yfvar(unify_x_local_value(Y),Insns,X) -->
	[u2_yfvar_xlval(X,Y)], !, collapse(Insns).
collapse_u2_yfvar(unify_y_value(Y),Insns,X) -->
	[u2_yfvar_yval(X,Y)], !, collapse(Insns).
collapse_u2_yfvar(unify_y_local_value(Y),Insns,X) -->
	[u2_yfvar_ylval(X,Y)], !, collapse(Insns).
collapse_u2_yfvar(Insn,Insns,X) -->
	[unify_y_first_variable(X)], collapse(Insn,Insns).

collapse_u2_void(proceed,Insns,X) -->
	[unify_void_proceed(X)], !, collapse(Insns).
collapse_u2_void(unify_x_variable(Y),Insns,X) -->
	[u2_void_xvar(X,Y)], !, collapse(Insns).
collapse_u2_void(allocate,[unify_y_variable(Y)|Insns],X) -->
	[u2_void_yfvar(X,Y)], !, collapse(Insns).
collapse_u2_void(unify_y_variable(Y),Insns,X) -->
	[u2_void_yvar(X,Y)], !, collapse(Insns).
collapse_u2_void(unify_x_value(Y),[proceed|Insns],X) -->
	[u2_void_xval_proceed(X,Y)], !, collapse(Insns).
collapse_u2_void(unify_x_value(Y),Insns,X) -->
	[u2_void_xval(X,Y)], !, collapse(Insns).
collapse_u2_void(unify_x_local_value(Y),[proceed|Insns],X) -->
	[u2_void_xlval_proceed(X,Y)], !, collapse(Insns).
collapse_u2_void(unify_x_local_value(Y),Insns,X) -->
	[u2_void_xlval(X,Y)], !, collapse(Insns).
collapse_u2_void(unify_y_value(Y),[deallocate,proceed|Insns],X) -->
	[u2_void_yval_return(X,Y)], !, collapse(Insns).
collapse_u2_void(unify_y_first_value(Y),Insns,X) -->
	[u2_void_yfval(X,Y)], !, collapse(Insns).
collapse_u2_void(unify_y_value(Y),Insns,X) -->
	[u2_void_yval(X,Y)], !, collapse(Insns).
collapse_u2_void(unify_y_local_value(Y),[deallocate,proceed|Insns],X) -->
	[u2_void_ylval_return(X,Y)], !, collapse(Insns).
collapse_u2_void(unify_y_local_value(Y),Insns,X) -->
	[u2_void_ylval(X,Y)], !, collapse(Insns).
collapse_u2_void(Insn,Insns,X) -->
	[unify_void(X)], collapse(Insn,Insns).

collapse_u2_xvar(unify_void,Insns0,X) -->
	{collapse_voids(Insns0,Insns,1,Y)},
	[u2_xvar_void(X,Y)], !, collapse(Insns).
collapse_u2_xvar(unify_x_variable(Y),Insns,X) -->
	[u2_xvar_xvar(X,Y)], !, collapse(Insns).
collapse_u2_xvar(allocate,[unify_y_variable(Y)|Insns],X) -->
	[u2_xvar_yfvar(X,Y)], !, collapse(Insns).
collapse_u2_xvar(unify_y_variable(Y),Insns,X) -->
	[u2_xvar_yvar(X,Y)], !, collapse(Insns).
collapse_u2_xvar(unify_x_value(Y),[proceed|Insns],X) -->
	[u2_xvar_xval_proceed(X,Y)], !, collapse(Insns).
collapse_u2_xvar(unify_x_value(Y),Insns,X) -->
	[u2_xvar_xval(X,Y)], !, collapse(Insns).
collapse_u2_xvar(unify_y_first_value(Y),Insns,X) -->
	[u2_xvar_yfval(X,Y)], !, collapse(Insns).
collapse_u2_xvar(unify_y_value(Y),Insns,X) -->
	[u2_xvar_yval(X,Y)], !, collapse(Insns).
collapse_u2_xvar(unify_y_local_value(Y),Insns,X) -->
	[u2_xvar_ylval(X,Y)], !, collapse(Insns).
collapse_u2_xvar(Insn,Insns,X) -->
	[unify_x_variable(X)], collapse(Insn,Insns).

collapse_u2_yvar(unify_void,Insns0,X) -->
	{collapse_voids(Insns0,Insns,1,Y)},
	[u2_yvar_void(X,Y)], !, collapse(Insns).
collapse_u2_yvar(unify_x_variable(Y),Insns,X) -->
	[u2_yvar_xvar(X,Y)], !, collapse(Insns).
collapse_u2_yvar(unify_y_variable(Y),Insns,X) -->
	[u2_yvar_yvar(X,Y)], !, collapse(Insns).
collapse_u2_yvar(unify_x_value(Y),Insns,X) -->
	[u2_yvar_xval(X,Y)], !, collapse(Insns).
collapse_u2_yvar(unify_x_local_value(Y),Insns,X) -->
	[u2_yvar_xlval(X,Y)], !, collapse(Insns).
collapse_u2_yvar(unify_y_value(Y),Insns,X) -->
	[u2_yvar_yval(X,Y)], !, collapse(Insns).
collapse_u2_yvar(unify_y_local_value(Y),Insns,X) -->
	[u2_yvar_ylval(X,Y)], !, collapse(Insns).
collapse_u2_yvar(Insn,Insns,X) -->
	[unify_y_variable(X)], collapse(Insn,Insns).

collapse_u2_xval(unify_void,Insns0,X) -->
	{collapse_voids(Insns0,[proceed|Insns],1,Y)},
	[u2_xval_void_proceed(X,Y)], !, collapse(Insns).
collapse_u2_xval(unify_void,Insns0,X) -->
	{collapse_voids(Insns0,Insns,1,Y)},
	[u2_xval_void(X,Y)], !, collapse(Insns).
collapse_u2_xval(unify_x_variable(Y),Insns,X) -->
	[u2_xval_xvar(X,Y)], !, collapse(Insns).
collapse_u2_xval(allocate,[unify_y_variable(Y)|Insns],X) -->
	[u2_xval_yfvar(X,Y)], !, collapse(Insns).
collapse_u2_xval(unify_y_variable(Y),Insns,X) -->
	[u2_xval_yvar(X,Y)], !, collapse(Insns).
collapse_u2_xval(unify_x_value(Y),[proceed|Insns],X) -->
	[u2_xval_xval_proceed(X,Y)], !, collapse(Insns).
collapse_u2_xval(unify_x_value(Y),Insns,X) -->
	[u2_xval_xval(X,Y)], !, collapse(Insns).
collapse_u2_xval(unify_x_local_value(Y),[proceed|Insns],X) -->
	[u2_xval_xlval_proceed(X,Y)], !, collapse(Insns).
collapse_u2_xval(unify_x_local_value(Y),Insns,X) -->
	[u2_xval_xlval(X,Y)], !, collapse(Insns).
collapse_u2_xval(unify_y_value(Y),[deallocate,proceed|Insns],X) -->
	[u2_xval_yval_return(X,Y)], !, collapse(Insns).
collapse_u2_xval(unify_y_first_value(Y),Insns,X) -->
	[u2_xval_yfval(X,Y)], !, collapse(Insns).
collapse_u2_xval(unify_y_value(Y),Insns,X) -->
	[u2_xval_yval(X,Y)], !, collapse(Insns).
collapse_u2_xval(unify_y_local_value(Y),[deallocate,proceed|Insns],X) -->
	[u2_xval_ylval_return(X,Y)], !, collapse(Insns).
collapse_u2_xval(unify_y_local_value(Y),Insns,X) -->
	[u2_xval_ylval(X,Y)], !, collapse(Insns).
collapse_u2_xval(Insn,Insns,X) -->
	[unify_x_value(X)], collapse(Insn,Insns).

collapse_u2_xlval(unify_void,Insns0,X) -->
	{collapse_voids(Insns0,[proceed|Insns],1,Y)},
	[u2_xlval_void_proceed(X,Y)], !, collapse(Insns).
collapse_u2_xlval(unify_void,Insns0,X) -->
	{collapse_voids(Insns0,Insns,1,Y)},
	[u2_xlval_void(X,Y)], !, collapse(Insns).
collapse_u2_xlval(unify_x_variable(Y),Insns,X) -->
	[u2_xlval_xvar(X,Y)], !, collapse(Insns).
collapse_u2_xlval(allocate,[unify_y_variable(Y)|Insns],X) -->
	[u2_xlval_yfvar(X,Y)], !, collapse(Insns).
collapse_u2_xlval(unify_y_variable(Y),Insns,X) -->
	[u2_xlval_yvar(X,Y)], !, collapse(Insns).
collapse_u2_xlval(unify_x_value(Y),[proceed|Insns],X) -->
	[u2_xlval_xval_proceed(X,Y)], !, collapse(Insns).
collapse_u2_xlval(unify_x_value(Y),Insns,X) -->
	[u2_xlval_xval(X,Y)], !, collapse(Insns).
collapse_u2_xlval(unify_x_local_value(Y),[proceed|Insns],X) -->
	[u2_xlval_xlval_proceed(X,Y)], !, collapse(Insns).
collapse_u2_xlval(unify_x_local_value(Y),Insns,X) -->
	[u2_xlval_xlval(X,Y)], !, collapse(Insns).
collapse_u2_xlval(unify_y_value(Y),[deallocate,proceed|Insns],X) -->
	[u2_xlval_yval_return(X,Y)], !, collapse(Insns).
collapse_u2_xlval(unify_y_first_value(Y),Insns,X) -->
	[u2_xlval_yfval(X,Y)], !, collapse(Insns).
collapse_u2_xlval(unify_y_value(Y),Insns,X) -->
	[u2_xlval_yval(X,Y)], !, collapse(Insns).
collapse_u2_xlval(unify_y_local_value(Y),[deallocate,proceed|Insns],X) -->
	[u2_xlval_ylval_return(X,Y)], !, collapse(Insns).
collapse_u2_xlval(unify_y_local_value(Y),Insns,X) -->
	[u2_xlval_ylval(X,Y)], !, collapse(Insns).
collapse_u2_xlval(Insn,Insns,X) -->
	[unify_x_local_value(X)], collapse(Insn,Insns).

collapse_u2_yval(unify_void,Insns0,X) -->
	{collapse_voids(Insns0,[deallocate,proceed|Insns],1,Y)},
	[u2_yval_void_return(X,Y)], !, collapse(Insns).
collapse_u2_yval(unify_void,Insns0,X) -->
	{collapse_voids(Insns0,Insns,1,Y)},
	[u2_yval_void(X,Y)], !, collapse(Insns).
collapse_u2_yval(unify_x_variable(Y),Insns,X) -->
	[u2_yval_xvar(X,Y)], !, collapse(Insns).
collapse_u2_yval(unify_y_variable(Y),Insns,X) -->
	[u2_yval_yvar(X,Y)], !, collapse(Insns).
collapse_u2_yval(unify_x_value(Y),[deallocate,proceed|Insns],X) -->
	[u2_yval_xval_return(X,Y)], !, collapse(Insns).
collapse_u2_yval(unify_x_value(Y),Insns,X) -->
	[u2_yval_xval(X,Y)], !, collapse(Insns).
collapse_u2_yval(unify_x_local_value(Y),[deallocate,proceed|Insns],X) -->
	[u2_yval_xlval_return(X,Y)], !, collapse(Insns).
collapse_u2_yval(unify_x_local_value(Y),Insns,X) -->
	[u2_yval_xlval(X,Y)], !, collapse(Insns).
collapse_u2_yval(unify_y_value(Y),[deallocate,proceed|Insns],X) -->
	[u2_yval_yval_return(X,Y)], !, collapse(Insns).
collapse_u2_yval(unify_y_first_value(Y),Insns,X) -->
	[u2_yval_yfval(X,Y)], !, collapse(Insns).
collapse_u2_yval(unify_y_value(Y),Insns,X) -->
	[u2_yval_yval(X,Y)], !, collapse(Insns).
collapse_u2_yval(unify_y_local_value(Y),[deallocate,proceed|Insns],X) -->
	[u2_yval_ylval_return(X,Y)], !, collapse(Insns).
collapse_u2_yval(unify_y_local_value(Y),Insns,X) -->
	[u2_yval_ylval(X,Y)], !, collapse(Insns).
collapse_u2_yval(Insn,Insns,X) -->
	[unify_y_value(X)], collapse(Insn,Insns).

collapse_u2_yfval(unify_void,Insns0,X) -->
	{collapse_voids(Insns0,Insns,1,Y)},
	[u2_yfval_void(X,Y)], !, collapse(Insns).
collapse_u2_yfval(unify_x_variable(Y),Insns,X) -->
	[u2_yfval_xvar(X,Y)], !, collapse(Insns).
collapse_u2_yfval(unify_x_value(Y),Insns,X) -->
	[u2_yfval_xval(X,Y)], !, collapse(Insns).
collapse_u2_yfval(unify_x_local_value(Y),Insns,X) -->
	[u2_yfval_xlval(X,Y)], !, collapse(Insns).
collapse_u2_yfval(unify_y_first_value(Y),Insns,X) -->
	[u2_yfval_yfval(X,Y)], !, collapse(Insns).
collapse_u2_yfval(unify_y_value(Y),Insns,X) -->
	[u2_yfval_yval(X,Y)], !, collapse(Insns).
collapse_u2_yfval(unify_y_local_value(Y),Insns,X) -->
	[u2_yfval_ylval(X,Y)], !, collapse(Insns).
collapse_u2_yfval(Insn,Insns,X) -->
	[unify_y_first_value(X)], collapse(Insn,Insns).

collapse_u2_ylval(unify_void,Insns0,X) -->
	{collapse_voids(Insns0,[deallocate,proceed|Insns],1,Y)},
	[u2_ylval_void_return(X,Y)], !, collapse(Insns).
collapse_u2_ylval(unify_void,Insns0,X) -->
	{collapse_voids(Insns0,Insns,1,Y)},
	[u2_ylval_void(X,Y)], !, collapse(Insns).
collapse_u2_ylval(unify_x_variable(Y),Insns,X) -->
	[u2_ylval_xvar(X,Y)], !, collapse(Insns).
collapse_u2_ylval(unify_y_variable(Y),Insns,X) -->
	[u2_ylval_yvar(X,Y)], !, collapse(Insns).
collapse_u2_ylval(unify_x_value(Y),[deallocate,proceed|Insns],X) -->
	[u2_ylval_xval_return(X,Y)], !, collapse(Insns).
collapse_u2_ylval(unify_x_value(Y),Insns,X) -->
	[u2_ylval_xval(X,Y)], !, collapse(Insns).
collapse_u2_ylval(unify_x_local_value(Y),[deallocate,proceed|Insns],X) -->
	[u2_ylval_xlval_return(X,Y)], !, collapse(Insns).
collapse_u2_ylval(unify_x_local_value(Y),Insns,X) -->
	[u2_ylval_xlval(X,Y)], !, collapse(Insns).
collapse_u2_ylval(unify_y_value(Y),[deallocate,proceed|Insns],X) -->
	[u2_ylval_yval_return(X,Y)], !, collapse(Insns).
collapse_u2_ylval(unify_y_first_value(Y),Insns,X) -->
	[u2_ylval_yfval(X,Y)], !, collapse(Insns).
collapse_u2_ylval(unify_y_value(Y),Insns,X) -->
	[u2_ylval_yval(X,Y)], !, collapse(Insns).
collapse_u2_ylval(unify_y_local_value(Y),[deallocate,proceed|Insns],X) -->
	[u2_ylval_ylval_return(X,Y)], !, collapse(Insns).
collapse_u2_ylval(unify_y_local_value(Y),Insns,X) -->
	[u2_ylval_ylval(X,Y)], !, collapse(Insns).
collapse_u2_ylval(Insn,Insns,X) -->
	[unify_y_local_value(X)], collapse(Insn,Insns).







/* Useful for looking at sequences.
tr_asm_args(Fmt,I,Off,Off1,Opcode) -->
   {ttynl, write(user,'>'), writeq(user,I)},
   asm_args(Fmt,I,Off,Off1,Opcode).
*/

/*
 | Offsets in *real* bytes, i.e. Opcode (1 SIC-byte) has +Off 2!! %FJBC%
 */

:- mode asm_args(+,?,?,?,?,?,?).
asm_args(none,_,Off,Off1,Opcode) --> 
	{Off1 is Off+2},
	[Opcode].
asm_args(builtin_x,I,Off,Off1,Opcode) -->
	evenop(Opcode,Off,Off0),
        {Off1 is Off0+6, arg(1,I,N), arg(2,I,A)},
	xop(A),
	{name_of_builtin(N,1,Op)},
        [builtin(Op)].
asm_args(builtin_x_x,I,Off,Off1,Opcode) -->
	oddop(Opcode,Off,Off0),
        {Off1 is Off0+8, arg(1,I,N), arg(2,I,A), arg(3,I,B)},
	xop(A), xop(B),
	{name_of_builtin(N,2,Op)},
        [builtin(Op)].
asm_args(builtin_x_x_x,I,Off,Off1,Opcode) -->
	evenop(Opcode,Off,Off0),
        {Off1 is Off0+10, arg(1,I,N), arg(2,I,A), arg(3,I,B), arg(4,I,C)},
	xop(A), xop(B), xop(C),
	{name_of_builtin(N,3,Op)},
        [builtin(Op)].
asm_args(function_x_x,I,Off,Off1,Opcode) -->
	oddop(Opcode,Off,Off0),
        {Off1 is Off0+8, arg(1,I,N), arg(2,I,A), arg(3,I,B)},
	xop(A), xop(B),
	{name_of_function(N,1,Op)},
        [builtin(Op)].
asm_args(function_x_x_x,I,Off,Off1,Opcode) -->
	evenop(Opcode,Off,Off0),
        {Off1 is Off0+10, arg(1,I,N), arg(2,I,A), arg(3,I,B), arg(4,I,C)},
	xop(A), xop(B), xop(C),
	{name_of_function(N,2,Op)},
        [builtin(Op)].
asm_args(i,I,Off,Off1,Opcode) -->
	{Off1 is Off+2, arg(1,I,Int), Int<5, Opcode1 is Opcode+Int},
	[Opcode1].
asm_args(i,I,Off,Off1,Opcode) -->
	{Off1 is Off+4, arg(1,I,Int)},
	[Opcode, Int].
asm_args(x,I,Off,Off1,Opcode) -->
	{Off1 is Off+4, arg(1,I,X)}, 
	[Opcode], xop(X).
asm_args(y,I,Off,Off1,Opcode) --> 
	{Off1 is Off+4, arg(1,I,Y)},
	[Opcode], yop(Y).
asm_args(rev_x_x,I,Off,Off1,Opcode) -->
	{Off1 is Off+6, arg(1,I,X1), arg(2,I,X)},
	[Opcode], xop(X), xop(X1).
asm_args(rev_y_x,I,Off,Off1,Opcode) -->
	{Off1 is Off+6, arg(1,I,Y), arg(2,I,X)},
	[Opcode], xop(X), yop(Y).
asm_args(rev_x_x_x_x,I,Off,Off1,Opcode) -->
	{Off1 is Off+10, arg(1,I,X1), arg(2,I,X2), arg(3,I,X3), arg(4,I,X4)},
	[Opcode], xop(X2), xop(X1), xop(X4), xop(X3).
asm_args(rev_y_x_y_x,I,Off,Off1,Opcode) -->
	{Off1 is Off+10, arg(1,I,X1), arg(2,I,X2), arg(3,I,X3), arg(4,I,X4)},
	[Opcode], xop(X2), yop(X1), xop(X4), yop(X3).
asm_args(i_x,I,Off,Off1,Opcode) -->
	{Off1 is Off+6, arg(1,I,Int), arg(2,I,X)},
	[Opcode, Int], xop(X).
asm_args(i_y,I,Off,Off1,Opcode) -->
	{Off1 is Off+6, arg(1,I,Int), arg(2,I,Y)},
	[Opcode, Int], yop(Y).
asm_args(x_i,I,Off,Off1,Opcode) -->
	{Off1 is Off+6, arg(1,I,X1), arg(2,I,X2)},
	[Opcode], xop(X1), [X2].
asm_args(x_x,I,Off,Off1,Opcode) -->
	{Off1 is Off+6, arg(1,I,X1), arg(2,I,X2)},
	[Opcode], xop(X1), xop(X2).
asm_args(x_y,I,Off,Off1,Opcode) -->
	{Off1 is Off+6, arg(1,I,X1), arg(2,I,X2)},
	[Opcode], xop(X1), yop(X2).
asm_args(y_i,I,Off,Off1,Opcode) -->
	{Off1 is Off+6, arg(1,I,X1), arg(2,I,X2)},
	[Opcode], yop(X1), [X2].
asm_args(y_x,I,Off,Off1,Opcode) -->
	{Off1 is Off+6, arg(1,I,X1), arg(2,I,X2)},
	[Opcode], yop(X1), xop(X2).
asm_args(y_y,I,Off,Off1,Opcode) -->
	{Off1 is Off+6, arg(1,I,X1), arg(2,I,X2)},
	[Opcode], yop(X1), yop(X2).
asm_args(size,I,Off,Off1,Opcode) -->
	{Off1 is Off+4, arg(1,I,Size)},
	[Opcode], sizeop(Size).
asm_args(ylist_label_size,I,Off,Off2,Opcode) -->
	{arg(1,I,List), length(List,N)},
	ylist_opcode(N,Opcode,Off,Off1),
        subop_ylist(List),
	{arg(2,I,Label), arg(3,I,Size), Off2 is Off1+6},
	[label(Label)], sizeop(Size).
asm_args(zlist_label_size,I,Off,Off2,Opcode) -->
	{arg(1,I,List), length(List,N)},
	ylist_opcode(N,Opcode,Off,Off1),
        subop_zlist(List),
	{arg(2,I,Label), arg(3,I,Size), Off2 is Off1+6},
	[label(Label)], sizeop(Size).
asm_args(zlist_label,I,Off,Off2,Opcode) -->
	{arg(1,I,List), length(List,N)},
	ylist_opcode(N,Opcode,Off,Off1),
        subop_zlist(List),
	{arg(2,I,Label), Off2 is Off1+4},
	[label(Label)].
asm_args(label_size,I,Off,Off1,Opcode) -->
	oddop(Opcode,Off,Off0),
	{Off1 is Off0+6, arg(1,I,Label), arg(2,I,Size)},
	[label(Label)], 
	sizeop(Size).
asm_args(label,I,Off,Off1,Opcode) -->
	oddop(Opcode,Off,Off0),
	{Off1 is Off0+4, arg(1,I,Label)},
	[label(Label)].
asm_args(tagged,I,Off,Off1,Opcode) -->
	oddop(Opcode,Off,Off0),
	{Off1 is Off0+4, arg(1,I,C)}, 
	[tagged(C)].
asm_args(functor,I,Off,Off1,Opcode) -->
	oddop(Opcode,Off,Off0),
	{Off1 is Off0+4, arg(1,I,C)}, 
	[functor(C)].
asm_args(tagged_x,I,Off,Off1,Opcode) -->
	evenop(Opcode,Off,Off0),
	{arg(2,I,X), arg(1,I,C), Off1 is Off0+6}, 
	xop(X), [tagged(C)].
asm_args(functor_x,I,Off,Off1,Opcode) -->
	evenop(Opcode,Off,Off0),
	{arg(2,I,X), arg(1,I,C), Off1 is Off0+6}, 
	xop(X), [functor(C)].
asm_args(x_x_x,I,Off,Off1,Opcode) -->					%% R.W.
	{Off1 is Off+8, arg(1,I,X1), arg(2,I,X2), arg(3,I,X3)},
	[Opcode], xop(X1), xop(X2), xop(X3).

asm_args(ylist_i_labels_size,I,Off,Offset,Opcode) -->	% :- R.W. , DCG & FJBC.

	{arg(1,I,List), length(List,N),
	 arg(2,I,Int),
	 arg(3,I,Labels),
	 arg(4,I,X),
	 Off0 is Off+N+N+2                   % 2 per op + 2 for counter 
         },
	evenop(Opcode,Off0,Off1),
	[N],                                 
	subop_ylist(List),
	[Int],
	{Off2 is Off1+2,                    %  Off1+sizeof("[Int]" counter)
	 OffBranch is Off2 - 4 },			%Offs. for branchs *Not the real Off* 
	asm_label_list(Labels,OffBranch,Off2,Off3),
	{Offset is Off3+2},  		     %  Off3 +2 for sizeop(X) 
	sizeop(X).
	% **** NOTICE ****
	% **** Refer to file "$AND_PROLOG_ROOT"/BUGS/alignment-qload for further explanation
	% **** %FJBC,DCG% 



asm_args(i_labels_size,I,Off,Offset,Opcode) -->	%% :-  R.W. , DCG & FJBC.
	{arg(1,I,Int),
	 arg(2,I,Labels),
	 arg(3,I,X)},
	evenop(Opcode,Off,Off1),
	[Int],
	{Off2 is Off1+2,             %  +2 for Int 
	Offbranch is Off2 - 4},      % Offset for branchs *Not the real Off*  
	asm_label_list(Labels,Offbranch,Off2,Off3), 
	{Offset is Off3+2},          % +2 for sizeop(X) 
	sizeop(X).

asm_label_list([],_,X,X)
    --> [].
asm_label_list([Lab|Labs],Off,Oin,Out)
    --> [Lab],
	{arg(1,Lab,Off), 
	 OffN is Oin+2},                   %% was +4, but offsets are 2 bytes MH KG
	asm_label_list(Labs,Off,OffN,Out).


xop(X) --> {Op is X<<2}, [Op].

yop(X) --> {Op is (64-X)<<2}, [Op].

zop(v(X)) --> {Op is (64-X)<<2}, [Op].		% Y value
zop(u(X)) --> {Op is 1+(64-X)<<2}, [Op].	% Y unsafe value

sizeop(X) --> {Op is (X+2)<<2}, [Op].

ylist_opcode(N,Opcode,Off,Off1) -->
	{N<9, !, Off0 is Off+N+N, Op is Opcode+N+N}, oddop(Op,Off0,Off1);
	{Off0 is Off+2+N+N, Op is Opcode+18}, oddop(Op,Off0,Off1), [N].

align_target(Off,Off1) -->					% R.W.
	{(Off mod 4) =:= 0, !, Off1 = Off};
	{Off1 is Off+2},[0].

oddop(Op,Off,Off1) -->
	{(Off mod 4) =:= 0, !, Off1 is Off+4, Op1 is Op-1}, [Op1, 0];
	{Off1 is Off+2}, [Op].

evenop(Op,Off,Off1) -->
	{(Off mod 4) =\= 0, !, Off1 is Off+4, Op1 is Op-1}, [Op1, 0];
	{Off1 is Off+2}, [Op].


subop_ylist([]) --> !.
subop_ylist([Op1|Ops]) -->
   yop(Op1), subop_ylist(Ops).

subop_zlist([]) --> !.
subop_zlist([Op1|Ops]) -->
   zop(Op1), subop_zlist(Ops).

:- mode ql_compile_file_emit(+).
ql_compile_file_emit(predicate(_,_)). %ignore!
ql_compile_file_emit(clause(Pred/_,Code,type_key(Type,Key,_),EffArity)) :-
   incore_parse_key(Key,EffKey),
   asm_insns(Code,0,Size,_CInsns,Tokens,[]),
   qdump_load_dbnode(0,Size),
   qdump(Tokens,0,Dic),
   dic_lookup(Sdic,Goal,0),
   ql_compile_file_emit_directive('$emulated_clause'(Pred,Type,EffKey,EffArity,
						     Goal),Dic,Sdic).
ql_compile_file_emit(call(Goal)) :-
   ql_compile_file_emit_directive(call(Goal),_,_).
ql_compile_file_emit((dynamic L)) :-
   ql_compile_file_emit_directive(dynamic0(L),_,_).
ql_compile_file_emit((public L)) :-
   ql_compile_file_emit_directive(public0(L),_,_).
ql_compile_file_emit(declare_predicate(Pred,_,Flag)) :-
   ql_compile_file_emit_directive('$emulated_predicate'(Pred,Flag),_,_).

ql_compile_file_emit_directive(D,Dic,Sdic) :-
   qlval_begin(R0,Q0),
   qdump_postlude(Dic,Sdic,R0,R1,Q0,Q1),
   lookup_term(D,S1,Sdic,R1,R,Q1,Q),
   qlval_end(R,Q),
   qdump_return(S1).		% run S1

:- mode incore_compile_file_emit(+).
incore_compile_file_emit(predicate(_,_)). %ignore!
incore_compile_file_emit(clause(Pred/_,Code,type_key(Type,Key,_),EffArity)) :-
   incore_parse_key(Key,EffKey),
   asm_insns(Code,0,Size,_CInsns,Tokens,[]),
   '$make_bytecode_object'(Size,Tokens,Obj),
   '$emulated_clause'(Pred,Type,EffKey,EffArity,Obj).
incore_compile_file_emit(call(Goal)) :-
	call(Goal).
incore_compile_file_emit((dynamic Goal)) :-
	dynamic0(Goal).
incore_compile_file_emit((public Goal)) :-
	public0(Goal).
incore_compile_file_emit(declare_predicate(Pred,_,Flag)) :-
   '$emulated_predicate'(Pred,Flag).

% Added to include lpwam symbolic and encoded files options PBC & MGB

:- mode lpwam_compile_file_emit(+).
%lpwam_compile_file_emit(predicate(_,_)). %ignore!
lpwam_compile_file_emit(clause(Pred/_,Code,type_key(_Type,_Key,_),_EffArity)) :- !,
%   incore_parse_key(Key,EffKey),
   asm_insns(Code,0,_Size,CInsns,Tokens,[]),
   dump_both(Pred,CInsns,Tokens).
%   qdump_load_dbnode(0,Size),               % OUT  FOR NOW
%   qdump(Tokens,0,Dic),
%   dic_lookup(Sdic,Goal,0),
%   lpwam_compile_file_emit_directive('$emulated_clause'(Pred,Type,EffKey,EffArity,
%						     Goal),Dic,Sdic).

lpwam_compile_file_emit(_).

%lpwam_compile_file_emit(call(Goal)) :-
%   lpwam_compile_file_emit_directive(call(Goal),_,_).
%lpwam_compile_file_emit((dynamic L)) :-
%   lpwam_compile_file_emit_directive(dynamic0(L),_,_).
%lpwam_compile_file_emit((public L)) :-
%   lpwam_compile_file_emit_directive(public0(L),_,_).
%lpwam_compile_file_emit(declare_predicate(Pred,_,Flag)) :-
%   lpwam_compile_file_emit_directive('$emulated_predicate'(Pred,Flag),_,_).

%lpwam_compile_file_emit_directive(_,_,_).

dump_both(Pred, CInsns, Token):-
   recorded_internal(streams,[Encoded,Symbolic],_),
   dump_clause(Symbolic,Pred,CInsns),
   dump_clause(Encoded,Pred,Token).
dump_both(_, _, _).

dump_clause(Stream, Pred, CInsns):-
   nl(Stream), nl(Stream),
   write(Stream,'Clause: '),
   write(Stream,Pred),
   nl(Stream), nl(Stream),
   plprint(CInsns,Stream).
dump_clause(_, _, _).

plprint([One|More],Stream):-
   write(Stream,'['),
   write(Stream,One),
   plprint_inner(More,Stream),
   write(Stream,']').

plprint_inner([],_).
plprint_inner([One|More],Stream):-
   write(Stream,','),
   nl(Stream),
   write(Stream,' '),
   write(Stream,One),
   plprint_inner(More,Stream).

:- mode incore_parse_key(+,?).
incore_parse_key(hash(N/A),F) :- functor(F,N,A).
incore_parse_key(hash(K),K) :- atomic(K).
incore_parse_key(nohash,_).

qdump_postlude(D,_,R0,R,Q0,Q) :- var(D), !, R0=R, Q0=Q.
qdump_postlude(dic(K,V,Le,Ri),Sdic,R0,R,Q0,Q) :-
   qdump_prev_last(V,0,Offset),
   qdump_postlude(Le,Sdic,R0,R1,Q0,Q1),
   qdump_post(K,Offset,Sdic,R1,R2,Q1,Q2),
   !, qdump_postlude(Ri,Sdic,R2,R,Q2,Q).

qdump_prev_last(V,Prev0,Prev) :- var(V), !, Prev0=Prev.
qdump_prev_last([I0|V],_,Prev) :- qdump_prev_last(V,I0,Prev).

:- mode qdump_post(+,?,?,?,?,?,?).
qdump_post(functor(Fu),Offset,_,R,R,Q0,Q) :-
   get_qlval(Fu,Sreg,Q0,Q),
   qdump_reloc_pointer(Sreg,Offset).
qdump_post(tagged(F),Offset,Sdic,R0,R,Q0,Q) :-
   lookup_term(F,Sreg,Sdic,R0,R,Q0,Q),
   qdump_reloc_pointer(Sreg,Offset).
qdump_post(label(Spec),Offset,Sdic,R0,R,Q0,Q) :-
   lookup_term(Spec,Sreg,Sdic,R0,R,Q0,Q),
   qdump_reloc_entrypoint(Sreg,Offset).

lookup_term(Term,Sreg,Sdic,R0,R,Q,Q) :-
   var(Term), !,
   dic_lookup(Sdic,Term,Sreg),
   (integer(Sreg), R0=R;
    R0=Sreg, R is R0-1,
    qdump_load_variable(R0)).
lookup_term(Term,Sreg,_,R,R,Q0,Q) :-
   atomic(Term), !,
   get_qlval(Term,Sreg,Q0,Q).
lookup_term([X|Y],R0,Sdic,R0,R,Q0,Q) :-
   !, 
   R1 is R0-1, 
   lookup_term(X,Sreg1,Sdic,R1,R2,Q0,Q1), 
   lookup_term(Y,Sreg2,Sdic,R2,R,Q1,Q),
   qdump_load_list(R0), 
   qdump_load_argument(Sreg1),
   qdump_load_argument(Sreg2).
lookup_term(Term,R0,Sdic,R0,R,Q0,Q) :-
   functor(Term,F,A), !,
   R1 is R0-1, 
   get_qlval(F/A,S0,Q0,Q1),
   lookup_args(0,A,Term,Sdic,R1,R,Q1,Q,Ss,[]),
   qdump_load_tuple(R0), 
   qdump_args([S0|Ss]).

lookup_args(A,A,_,_,R,R,Q,Q) --> !.
lookup_args(I,A,Term,Sdic,R0,R,Q0,Q) -->
   [Sreg],
   {I<A, I1 is I+1, 
    arg(I1,Term,Arg),
    lookup_term(Arg,Sreg,Sdic,R0,R1,Q0,Q1)},
   !, lookup_args(I1,A,Term,Sdic,R1,R,Q1,Q).

qlval_begin(-1,Q) :-
	recorded_internal('FREE_QLVAL',qlval(Q,Q),Ref), !, erase(Ref);
	Q=1.

qlval_end(_,Q) :-
	recorda_internal('FREE_QLVAL',qlval(Q,Q),_).

get_qlval(N/A,Sreg,Q,Q) :-
	functor(Fu,N,A),
	recorded_internal(Fu,qlval(Fu,Sreg),_),
	!.
get_qlval(N/A,Q0,Q0,Q) :-
	functor(Fu,N,A),
	recorda_internal(Fu,qlval(Fu,Q0),_),
	Q1 is Q0+1,
	get_qlval(N,Sreg,Q1,Q),
	qdump_load_functor(Q0,Sreg,A),
	!.
get_qlval(S,Sreg,Q,Q) :-
	atomic(S),
	recorded_internal(S,qlval(S,Sreg),_),
	!.
get_qlval([],Q0,Q0,Q) :-
	recorda_internal([],qlval([],Q0),_),
	Q is Q0+1,
	qdump_load_nil(Q0),
	!.
get_qlval(A,Q0,Q0,Q) :-
	atom(A),
	recorda_internal(A,qlval(A,Q0),_),
	Q is Q0+1,
	qdump_load_atom(Q0,A),
	!.
get_qlval(N,Q0,Q0,Q) :-
	number(N),
	recorda_internal(N,qlval(N,Q0),_),
	Q is Q0+1,
	qdump_load_number(Q0,N),
	!.


:- mode qdump(+,?,?).
qdump([],_,_) :- 
   put_code(0).				% end of program
qdump([Tok|Toks],Off,Dic) :-
   qdump_token(Tok,Off,Off1,Dic),
   !, qdump(Toks,Off1,Dic).

qdump_token(X,O,O1,_) :-
   integer(X), O1 is O+2, qdump_short(X).
qdump_token(branch(L1,L2),O,O1,_) :-
   X is L2-L1-2, O1 is O+2, qdump_short(X).
qdump_token(builtin(Op),O,O4,_) :- 
   O4 is O+4, put_code(0'C), qdump_long(Op).
qdump_token(Reloc,O,O4,Dic) :- 
   O4 is O+4,
   qdump_patch(Reloc,Old,O,Dic), put_code(0'+), qdump_long(Old).

qdump_patch(Key,Oldval,Newval,Olddic) :-
   dic_lookup(Olddic,Key,List),
   qdump_prev_last(List,0,Oldval,Newval).

qdump_prev_last(V,Prev,Prev,Last) :- var(V), !, V=[Last|_].
qdump_prev_last([I0|V],_,Prev,Last) :- qdump_prev_last(V,I0,Prev,Last).

qdump_load_atom(Li,Str) :-
   put_code(0'A), qdump_short(Li), qdump_atom(Str).
				% These values (0'A etc.) match those in 
qdump_load_functor(Li,L2,A) :-	% qinstrdefs.h (M.H.)
   put_code(0'B), qdump_short(Li), qdump_short(L2), qdump_short(A).

qdump_load_number(Li,Num) :-
   integer(Num), -32768 =< Num, Num =< 32767, !,
   put_code(0'C), qdump_short(Li), qdump_short(Num).
qdump_load_number(Li,Num) :-
   integer(Num), !,
   put_code(0'D), qdump_short(Li), qdump_long(Num).
qdump_load_number(Li,Num) :-
   float(Num), !,
   put_code(0'E), qdump_short(Li), qdump_float(Num).

qdump_load_variable(Li) :-
   put_code(0'F), qdump_short(Li).

qdump_load_nil(Li) :-
   put_code(0'G), qdump_short(Li).

qdump_load_list(Li) :-
   put_code(0'H), qdump_short(Li).

qdump_load_tuple(Li) :-
   put_code(0'I), qdump_short(Li).

qdump_load_argument(Li) :-
   put_code(0'J), qdump_short(Li).

qdump_load_dbnode(Li,Size) :-		% Dumps partial insn only.
   put_code(0'K), qdump_short(Li), qdump_short(Size).

qdump_reloc_pointer(Li,Label) :-
   put_code(0'L), qdump_short(Li), qdump_long(Label).

qdump_reloc_entrypoint(Li,Label) :-
   put_code(0'M), qdump_short(Li), qdump_long(Label).

qdump_reloc_offset(Target,Label) :-
   put_code(0'N), qdump_long(Target), qdump_long(Label).

qdump_return(Li) :-
   put_code(0'O), qdump_short(Li).

qdump_args([]).
qdump_args([S|Ss]) :- qdump_load_argument(S), qdump_args(Ss).

qdump_atom(X) :-
   '$display'(X), put_code(0).		% string arg.

qdump_short(X) :-
   '$display'(X), put_code(0).		% short arg.

qdump_long(X) :-
   '$display'(X), put_code(0).		% long arg.

qdump_float(X) :-
   '$display'(X), put_code(0).		% float arg.
