% This program generates some .c files of the engine

% TODO: Continue factorization

:- module(_, _, [dcg, functions, assertions]).

:- use_module(library(read)).
:- use_module(library(format)).
:- use_module(library(streams)).
:- use_module(library(lists)).
:- use_module(library(system), [system/1, modif_time/2]).

main :-
	emit_file(r, String, []),
	write_string_to_file('wamread.c', String),
	emit_file(w, String2, []),
	write_string_to_file('wamwrite.c', String2),
	emit_all_ins_op(String3, []),
	write_string_to_file('instrdefs.h', String3).

write_string_to_file(File, String) :-
	OutStream = ~open_output(File),
	format("~s", [String]),
	close_output(OutStream).

% ---------------------------------------------------------------------------
% Helper predicates

uppercase([]) := [].
uppercase([X|Xs]) := [~uppercase_2(X)|~uppercase(Xs)].

uppercase_2(X0) := X :- X0 >= 0'a, X0 =< 0'z, !,
	X is X0 + 0'A - 0'a.
uppercase_2(X) := X.

emit_uppercase(X) -->
	{ Codes = ~uppercase(~atom_codes(X)) },
	emit_string(Codes).

emit_atom(X) -->
	{ Codes = ~atom_codes(X) },
	emit_string(Codes).

emit_number(X) -->
	{ Codes = ~number_codes(X) },
	emit_string(Codes).

% ---------------------------------------------------------------------------
% Emit emulator instructions

emit_label(Ins, r) --> !, "r_", emit_atom(Ins).
emit_label(Ins, w) --> !, "w_", emit_atom(Ins).

emit_string([]) --> [].
emit_string([X|Xs]) --> [X], emit_string(Xs).

% Emit an instruction entry (label, case and code)
emit_ins(Ins, Mode) -->
	emit_label(Ins, Mode), ":\n",
	"case ", emit_uppercase(Ins), ":\n",
	emit_ins_2(Ins, Mode).

% Emit an instruction entry (no label, case and code)
emit_ins_no_label(Ins, Mode) -->
	"case ", emit_uppercase(Ins), ":\n",
	emit_ins_2(Ins, Mode).

% Emit a goto statement to the specified instruction and mode
emit_goto_ins(Ins, Mode) -->
	"goto ", emit_label(Ins, Mode), ";\n".

% Emit the code of the instruction and mode
emit_inline_ins(Ins, Mode) -->
	emit_ins_2(Ins, Mode).

% Emit the initialization of Y variables
emit_init_yvars(Count) -->
	"for (t0 = ", emit_string(Count), "-sizeof(TAGGED);\n",
	"     t0 >= EToY0*sizeof(TAGGED);\n",
	"     t0 -= sizeof(TAGGED)) {\n",
	"  LoadSVA(Yb(t0));\n",
	"}\n".

% Emit the code to put a Y argument (may be unsafe)
emit_putarg(Xn) -->
	"  if (t1&1) {\n",
	"    RefStackUnsafe(X(", emit_string(Xn), "),&Yb(t1+1));\n",
	"  } else {\n",
	"    RefStack(X(", emit_string(Xn), "),&Yb(t1));\n",
	"  }\n".

% Move the program counter to discard an argument
emit_shift("Q") -->
	"P++;\n".

% Move the program counter and go to read mode
emit_dispatch_r(Size) -->
	"DISPATCH_R(", emit_string(Size), ");\n".

% Move the program counter and go to write mode
emit_dispatch_w(Size) -->
	"DISPATCH_W(", emit_string(Size), ");\n".

:- discontiguous ins_op_format/3.
:- discontiguous emit_ins_2/4.

ins_op_format(inittrue, 260, "e"). % Descr size
emit_ins_2(inittrue,r) -->
	"LoadH;\n",
	emit_goto_ins(inittrue,w).
emit_ins_2(inittrue,w) -->
	"ComputeE;\n",
	emit_init_yvars("P1"),
	"goto firsttrue;\n".

ins_op_format(firsttrue_n, 261, "Y"). % Descr ylist_size
emit_ins_2(firsttrue_n,r) -->
	"LoadH;\n",
	emit_goto_ins(firsttrue_n,w).
emit_ins_2(firsttrue_n,w) -->
	"for (i = SP1, P++; i>0; --i)\n",
	"  PUT_YVOID;\n",
	"firsttrue:\n",
	"E->next_insn = w->next_insn;\n",
	"E->frame = w->frame;\n",
	"w->frame = E;\n",
	"w->next_insn = Pplus2;\n",
	"w->local_top = StackCharOffset(E,P1);\n",
	"if (OffStacktop(E,Stack_Warn))\n",
	"  SetEvent;\n",
	emit_dispatch_w("1").

ins_op_format(initcallq, 0, "QEev").
emit_ins_2(initcallq,r) -->
	emit_shift("Q"),
	emit_goto_ins(initcall,r).
emit_ins_2(initcallq,w) -->
	emit_shift("Q"),
	emit_goto_ins(initcall,w).

ins_op_format(initcall, 1, "Eev"). % Descr label_size
emit_ins_2(initcall,r) -->
	"LoadH;\n",
	emit_goto_ins(initcall,w).
emit_ins_2(initcall,w) -->
	"ComputeE;\n",
	emit_init_yvars("*(P+BPTP)"),
	emit_goto_ins(firstcall,w).

ins_op_format(firstcall_nq, 20, "QYEev").
emit_ins_2(firstcall_nq,r) -->
	emit_shift("Q"),
	emit_goto_ins(firstcall_n,r).
emit_ins_2(firstcall_nq,w) -->
	emit_shift("Q"),
	emit_goto_ins(firstcall_n,w).

ins_op_format(firstcall_n, 21, "YEev").
emit_ins_2(firstcall_n,r) -->
	"LoadH;\n",
	emit_goto_ins(firstcall_n,w).
emit_ins_2(firstcall_n,w) -->
	"for (i = SP1, P++; i>8; --i)\n",
	"  PUT_YVOID;\n",
	emit_goto_ins(firstcall_8,w).

ins_op_format(firstcall_8q, 18, "QyvyvyvyvyvyvyvyvEev").
emit_ins_2(firstcall_8q,r) -->
	emit_shift("Q"),
	emit_goto_ins(firstcall_8,r).
emit_ins_2(firstcall_8q,w) -->
	emit_shift("Q"),
	emit_goto_ins(firstcall_8,w).

ins_op_format(firstcall_8, 19, "yvyvyvyvyvyvyvyvEev").
emit_ins_2(firstcall_8,r) -->
	"LoadH;\n",
	emit_goto_ins(firstcall_8,w).
emit_ins_2(firstcall_8,w) -->
	"PUT_YVOID;\n",
	emit_goto_ins(firstcall_7,w).

ins_op_format(firstcall_7q, 16, "QyvyvyvyvyvyvyvEev").
emit_ins_2(firstcall_7q,r) -->
	emit_shift("Q"),
	emit_goto_ins(firstcall_7,r).
emit_ins_2(firstcall_7q,w) -->
	emit_shift("Q"),
	emit_goto_ins(firstcall_7,w).

ins_op_format(firstcall_7, 17, "yvyvyvyvyvyvyvEev").
emit_ins_2(firstcall_7,r) -->
	"LoadH;\n",
	emit_goto_ins(firstcall_7,w).
emit_ins_2(firstcall_7,w) -->
	"PUT_YVOID;\n",
	emit_goto_ins(firstcall_6,w).

ins_op_format(firstcall_6q, 14, "QyvyvyvyvyvyvEev").
emit_ins_2(firstcall_6q,r) -->
	emit_shift("Q"),
	emit_goto_ins(firstcall_6,r).
emit_ins_2(firstcall_6q,w) -->
	emit_shift("Q"),
	emit_goto_ins(firstcall_6,w).

ins_op_format(firstcall_6, 15, "yvyvyvyvyvyvEev").
emit_ins_2(firstcall_6,r) -->
	"LoadH;\n",
	emit_goto_ins(firstcall_6,w).
emit_ins_2(firstcall_6,w) -->
	"PUT_YVOID;\n",
	emit_goto_ins(firstcall_5,w).

ins_op_format(firstcall_5q, 12, "QyvyvyvyvyvEev").
emit_ins_2(firstcall_5q,r) -->
	emit_shift("Q"),
	emit_goto_ins(firstcall_5,r).
emit_ins_2(firstcall_5q,w) -->
	emit_shift("Q"),
	emit_goto_ins(firstcall_5,w).

ins_op_format(firstcall_5, 13, "yvyvyvyvyvEev").
emit_ins_2(firstcall_5,r) -->
	"LoadH;\n",
	emit_goto_ins(firstcall_5,w).
emit_ins_2(firstcall_5,w) -->
	"PUT_YVOID;\n",
	emit_goto_ins(firstcall_4,w).

ins_op_format(firstcall_4q, 10, "QyvyvyvyvEev").
emit_ins_2(firstcall_4q,r) -->
	emit_shift("Q"),
	emit_goto_ins(firstcall_4,r).
emit_ins_2(firstcall_4q,w) -->
	emit_shift("Q"),
	emit_goto_ins(firstcall_4,w).

ins_op_format(firstcall_4, 11, "yvyvyvyvEev").
emit_ins_2(firstcall_4,r) -->
	"LoadH;\n",
	emit_goto_ins(firstcall_4,w).
emit_ins_2(firstcall_4,w) -->
	"PUT_YVOID;\n",
	emit_goto_ins(firstcall_3,w).

ins_op_format(firstcall_3q, 8, "QyvyvyvEev").
emit_ins_2(firstcall_3q,r) -->
	emit_shift("Q"),
	emit_goto_ins(firstcall_3,r).
emit_ins_2(firstcall_3q,w) -->
	emit_shift("Q"),
	emit_goto_ins(firstcall_3,w).

ins_op_format(firstcall_3, 9, "yvyvyvEev").
emit_ins_2(firstcall_3,r) -->
	"LoadH;\n",
	emit_goto_ins(firstcall_3,w).
emit_ins_2(firstcall_3,w) -->
	"PUT_YVOID;\n",
	emit_goto_ins(firstcall_2,w).

ins_op_format(firstcall_2q, 6, "QyvyvEev").
emit_ins_2(firstcall_2q,r) -->
	emit_shift("Q"),
	emit_goto_ins(firstcall_2,r).
emit_ins_2(firstcall_2q,w) -->
	emit_shift("Q"),
	emit_goto_ins(firstcall_2,w).

ins_op_format(firstcall_2, 7, "yvyvEev").
emit_ins_2(firstcall_2,r) -->
	"LoadH;\n",
	emit_goto_ins(firstcall_2,w).
emit_ins_2(firstcall_2,w) -->
	"PUT_YVOID;\n",
	emit_goto_ins(firstcall_1,w).

ins_op_format(firstcall_1q, 4, "QyvEev").
emit_ins_2(firstcall_1q,r) -->
	emit_shift("Q"),
	emit_goto_ins(firstcall_1,r).
emit_ins_2(firstcall_1q,w) -->
	emit_shift("Q"),
	emit_goto_ins(firstcall_1,w).

ins_op_format(firstcall_1, 5, "yvEev").
emit_ins_2(firstcall_1,r) -->
	"LoadH;\n",
	emit_goto_ins(firstcall_1,w).
emit_ins_2(firstcall_1,w) -->
	"PUT_YVOID;\n",
	emit_goto_ins(firstcall,w).

ins_op_format(firstcallq, 2, "QEev").
emit_ins_2(firstcallq,r) -->
	emit_shift("Q"),
	emit_goto_ins(firstcall,r).
emit_ins_2(firstcallq,w) -->
	emit_shift("Q"),
	emit_goto_ins(firstcall,w).

ins_op_format(firstcall, 3, "Eev"). % Descr ylist_label_size
emit_ins_2(firstcall,r) -->
	"LoadH;\n",
	emit_goto_ins(firstcall,w).
emit_ins_2(firstcall,w) -->
	"E->next_insn = w->next_insn;\n",
	"E->frame = w->frame;\n",
	"w->frame = E;\n",
	"w->next_insn = P+BPTP+LOffset;\n",
	"w->local_top = StackCharOffset(E,*(P+BPTP));\n",
	"P = BP1;\n",
	"if (OffStacktop(E,Stack_Warn))\n",
	"  SetEvent;\n",
	"goto enter_predicate;\n".

ins_op_format(call_nq, 40, "QZEev").
emit_ins_2(call_nq,r) -->
	emit_shift("Q"),
	emit_goto_ins(call_n,r).
emit_ins_2(call_nq,w) -->
	emit_shift("Q"),
	emit_goto_ins(call_n,w).

ins_op_format(call_n, 41, "ZEev").
emit_ins_2(call_n,r) -->
	"LoadH;\n",
	emit_goto_ins(call_n,w).
emit_ins_2(call_n,w) -->
	"for (i = SP1, P++; i>8; --i) {\n",
	"  t1 = P1, P++;\n",
	emit_putarg("i-1"),
	"}\n",
	emit_goto_ins(call_8,w).

ins_op_format(call_8q, 38, "QzzzzzzzzEev").
emit_ins_2(call_8q,r) -->
	emit_shift("Q"),
	emit_goto_ins(call_8,r).
emit_ins_2(call_8q,w) -->
	emit_shift("Q"),
	emit_goto_ins(call_8,w).

ins_op_format(call_8, 39, "zzzzzzzzEev").
emit_ins_2(call_8,r) -->
	"LoadH;\n",
	emit_goto_ins(call_8,w).
emit_ins_2(call_8,w) -->
	"t1 = P1, P++;\n",
	emit_putarg("7"),
	emit_goto_ins(call_7,w).

ins_op_format(call_7q, 36, "QzzzzzzzEev").
emit_ins_2(call_7q,r) -->
	emit_shift("Q"),
	emit_goto_ins(call_7,r).
emit_ins_2(call_7q,w) -->
	emit_shift("Q"),
	emit_goto_ins(call_7,w).

ins_op_format(call_7, 37, "zzzzzzzEev").
emit_ins_2(call_7,r) -->
	"LoadH;\n",
	emit_goto_ins(call_7,w).
emit_ins_2(call_7,w) -->
	"t1 = P1, P++;\n",
	emit_putarg("6"),
	emit_goto_ins(call_6,w).

ins_op_format(call_6q, 34, "QzzzzzzEev").
emit_ins_2(call_6q,r) -->
	emit_shift("Q"),
	emit_goto_ins(call_6,r).
emit_ins_2(call_6q,w) -->
	emit_shift("Q"),
	emit_goto_ins(call_6,w).

ins_op_format(call_6, 35, "zzzzzzEev").
emit_ins_2(call_6,r) -->
	"LoadH;\n",
	emit_goto_ins(call_6,w).
emit_ins_2(call_6,w) -->
	"t1 = P1, P++;\n",
	emit_putarg("5"),
	emit_goto_ins(call_5,w).

ins_op_format(call_5q, 32, "QzzzzzEev").
emit_ins_2(call_5q,r) -->
	emit_shift("Q"),
	emit_goto_ins(call_5,r).
emit_ins_2(call_5q,w) -->
	emit_shift("Q"),
	emit_goto_ins(call_5,w).

ins_op_format(call_5, 33, "zzzzzEev").
emit_ins_2(call_5,r) -->
	"LoadH;\n",
	emit_goto_ins(call_5,w).
emit_ins_2(call_5,w) -->
	"t1 = P1, P++;\n",
	emit_putarg("4"),
	emit_goto_ins(call_4,w).

ins_op_format(call_4q, 30, "QzzzzEev").
emit_ins_2(call_4q,r) -->
	emit_shift("Q"),
	emit_goto_ins(call_4,r).
emit_ins_2(call_4q,w) -->
	emit_shift("Q"),
	emit_goto_ins(call_4,w).

ins_op_format(call_4, 31, "zzzzEev").
emit_ins_2(call_4,r) -->
	"LoadH;\n",
	emit_goto_ins(call_4,w).
emit_ins_2(call_4,w) -->
	"t1 = P1, P++;\n",
	emit_putarg("3"),
	emit_goto_ins(call_3,w).

ins_op_format(call_3q, 28, "QzzzEev").
emit_ins_2(call_3q,r) -->
	emit_shift("Q"),
	emit_goto_ins(call_3,r).
emit_ins_2(call_3q,w) -->
	emit_shift("Q"),
	emit_goto_ins(call_3,w).

ins_op_format(call_3, 29, "zzzEev").
emit_ins_2(call_3,r) -->
	"LoadH;\n",
	emit_goto_ins(call_3,w).
emit_ins_2(call_3,w) -->
	"t1 = P1, P++;\n",
	emit_putarg("2"),
	emit_goto_ins(call_2,w).

ins_op_format(call_2q, 26, "QzzEev").
emit_ins_2(call_2q,r) -->
	emit_shift("Q"),
	emit_goto_ins(call_2,r).
emit_ins_2(call_2q,w) -->
	emit_shift("Q"),
	emit_goto_ins(call_2,w).

ins_op_format(call_2, 27, "zzEev").
emit_ins_2(call_2,r) -->
	"LoadH;\n",
	emit_goto_ins(call_2,w).
emit_ins_2(call_2,w) -->
	"t1 = P1, P++;\n",
	emit_putarg("1"),
	emit_goto_ins(call_1,w).

ins_op_format(call_1q, 24, "QzEev").
emit_ins_2(call_1q,r) -->
	emit_shift("Q"),
	emit_goto_ins(call_1,r).
emit_ins_2(call_1q,w) -->
	emit_shift("Q"),
	emit_goto_ins(call_1,w).

ins_op_format(call_1, 25, "zEev").
emit_ins_2(call_1,r) -->
	"LoadH;\n",
	emit_goto_ins(call_1,w).
emit_ins_2(call_1,w) -->
	"t1 = P1, P++;\n",
	emit_putarg("0"),
	emit_goto_ins(call,w).

ins_op_format(callq, 22, "QEev").
emit_ins_2(callq,r) -->
	emit_shift("Q"),
	emit_goto_ins(call,r).
emit_ins_2(callq,w) -->
	emit_shift("Q"),
	emit_goto_ins(call,w).

ins_op_format(call, 23, "Eev"). % Descr zlist_label_size
emit_ins_2(call,r) -->
	"LoadH;\n",
	emit_goto_ins(call,w).
emit_ins_2(call,w) -->
	"w->next_insn = P+BPTP+LOffset;\n",
	"P = BP1;\n",
	"goto enter_predicate;\n".

ins_op_format(lastcall_nq, 60, "QZE").
emit_ins_2(lastcall_nq,r) -->
	emit_shift("Q"),
	emit_goto_ins(lastcall_n,r).
emit_ins_2(lastcall_nq,w) -->
	emit_shift("Q"),
	emit_goto_ins(lastcall_n,w).

ins_op_format(lastcall_n, 61, "ZE").
emit_ins_2(lastcall_n,r) -->
	"LoadH;\n",
	emit_goto_ins(lastcall_n,w).
emit_ins_2(lastcall_n,w) -->
	"for (i = SP1, P++; i>8; --i) {\n",
	"    t1 = P1, P++;\n",
	emit_putarg("i-1"),
	"  }\n",
	emit_goto_ins(lastcall_8,w).

ins_op_format(lastcall_8q, 58, "QzzzzzzzzE").
emit_ins_2(lastcall_8q,r) -->
	emit_shift("Q"),
	emit_goto_ins(lastcall_8,r).
emit_ins_2(lastcall_8q,w) -->
	emit_shift("Q"),
	emit_goto_ins(lastcall_8,w).

ins_op_format(lastcall_8, 59, "zzzzzzzzE").
emit_ins_2(lastcall_8,r) -->
	"LoadH;\n",
	emit_goto_ins(lastcall_8,w).
emit_ins_2(lastcall_8,w) -->
	"t1 = P1, P++;\n",
	emit_putarg("7"),
	emit_goto_ins(lastcall_7,w).

ins_op_format(lastcall_7q, 56, "QzzzzzzzE").
emit_ins_2(lastcall_7q,r) -->
	emit_shift("Q"),
	emit_goto_ins(lastcall_7,r).
emit_ins_2(lastcall_7q,w) -->
	emit_shift("Q"),
	emit_goto_ins(lastcall_7,w).

ins_op_format(lastcall_7, 57, "zzzzzzzE").
emit_ins_2(lastcall_7,r) -->
	"LoadH;\n",
	emit_goto_ins(lastcall_7,w).
emit_ins_2(lastcall_7,w) -->
	"t1 = P1, P++;\n",
	emit_putarg("6"),
	emit_goto_ins(lastcall_6,w).

ins_op_format(lastcall_6q, 54, "QzzzzzzE").
emit_ins_2(lastcall_6q,r) -->
	emit_shift("Q"),
	emit_goto_ins(lastcall_6,r).
emit_ins_2(lastcall_6q,w) -->
	emit_shift("Q"),
	emit_goto_ins(lastcall_6,w).

ins_op_format(lastcall_6, 55, "zzzzzzE").
emit_ins_2(lastcall_6,r) -->
	"LoadH;\n",
	emit_goto_ins(lastcall_6,w).
emit_ins_2(lastcall_6,w) -->
	"t1 = P1, P++;\n",
	emit_putarg("5"),
	emit_goto_ins(lastcall_5,w).

ins_op_format(lastcall_5q, 52, "QzzzzzE").
emit_ins_2(lastcall_5q,r) -->
	emit_shift("Q"),
	emit_goto_ins(lastcall_5,r).
emit_ins_2(lastcall_5q,w) -->
	emit_shift("Q"),
	emit_goto_ins(lastcall_5,w).

ins_op_format(lastcall_5, 53, "zzzzzE").
emit_ins_2(lastcall_5,r) -->
	"LoadH;\n",
	emit_goto_ins(lastcall_5,w).
emit_ins_2(lastcall_5,w) -->
	"t1 = P1, P++;\n",
	emit_putarg("4"),
	emit_goto_ins(lastcall_4,w).

ins_op_format(lastcall_4q, 50, "QzzzzE").
emit_ins_2(lastcall_4q,r) -->
	emit_shift("Q"),
	emit_goto_ins(lastcall_4,r).
emit_ins_2(lastcall_4q,w) -->
	emit_shift("Q"),
	emit_goto_ins(lastcall_4,w).

ins_op_format(lastcall_4, 51, "zzzzE").
emit_ins_2(lastcall_4,r) -->
	"LoadH;\n",
	emit_goto_ins(lastcall_4,w).
emit_ins_2(lastcall_4,w) -->
	"t1 = P1, P++;\n",
	emit_putarg("3"),
	emit_goto_ins(lastcall_3,w).

ins_op_format(lastcall_3q, 48, "QzzzE").
emit_ins_2(lastcall_3q,r) -->
	emit_shift("Q"),
	emit_goto_ins(lastcall_3,r).
emit_ins_2(lastcall_3q,w) -->
	emit_shift("Q"),
	emit_goto_ins(lastcall_3,w).

ins_op_format(lastcall_3, 49, "zzzE").
emit_ins_2(lastcall_3,r) -->
	"LoadH;\n",
	emit_goto_ins(lastcall_3,w).
emit_ins_2(lastcall_3,w) -->
	"t1 = P1, P++;\n",
	emit_putarg("2"),
	emit_goto_ins(lastcall_2,w).

ins_op_format(lastcall_2q, 46, "QzzE").
emit_ins_2(lastcall_2q,r) -->
	emit_shift("Q"),
	emit_goto_ins(lastcall_2,r).
emit_ins_2(lastcall_2q,w) -->
	emit_shift("Q"),
	emit_goto_ins(lastcall_2,w).

ins_op_format(lastcall_2, 47, "zzE").
emit_ins_2(lastcall_2,r) -->
	"LoadH;\n",
	emit_goto_ins(lastcall_2,w).
emit_ins_2(lastcall_2,w) -->
	"t1 = P1, P++;\n",
	emit_putarg("1"),
	emit_goto_ins(lastcall_1,w).

ins_op_format(lastcall_1q, 44, "QzE").
emit_ins_2(lastcall_1q,r) -->
	emit_shift("Q"),
	emit_goto_ins(lastcall_1,r).
emit_ins_2(lastcall_1q,w) -->
	emit_shift("Q"),
	emit_goto_ins(lastcall_1,w).

ins_op_format(lastcall_1, 45, "zE").
emit_ins_2(lastcall_1,r) -->
	"LoadH;\n",
	emit_goto_ins(lastcall_1,w).
emit_ins_2(lastcall_1,w) -->
	"t1 = P1, P++;\n",
	emit_putarg("0"),
	emit_goto_ins(lastcall,w).

ins_op_format(lastcallq, 42, "QE").
emit_ins_2(lastcallq,r) -->
	emit_shift("Q"),
	emit_goto_ins(lastcall,r).
emit_ins_2(lastcallq,w) -->
	emit_shift("Q"),
	emit_goto_ins(lastcall,w).

ins_op_format(lastcall, 43, "E"). % Descr zlist_label
emit_ins_2(lastcall,r) -->
	"LoadH;\n",
	emit_goto_ins(lastcall,w).
emit_ins_2(lastcall,w) -->
	"DEALLOCATE;\n",
	emit_goto_ins(execute,w).

ins_op_format(executeq, 62, "QE").
emit_ins_2(executeq,r) -->
	"LoadH;\n",
	"P = BP2;\n",
	"goto enter_predicate;\n".
emit_ins_2(executeq,w) -->
	"P = BP2;\n",
	"goto enter_predicate;\n".

ins_op_format(execute, 63, "E"). % Descr label
emit_ins_2(execute,r) -->
	"LoadH;\n",
	"P = BP1;\n",
	"goto enter_predicate;\n".
emit_ins_2(execute,w) -->
	"P = BP1;\n",
	"goto enter_predicate;\n".

ins_op_format(put_x_void, 69, "x"). % Descr x
emit_ins_2(put_x_void,r) -->
	"LoadH;\n",
	emit_goto_ins(put_x_void,w).
emit_ins_2(put_x_void,w) -->
	"LoadHVA(Xb(P1),H);\n",
	emit_dispatch_w("1").

ins_op_format(put_x_variable, 70, "xx"). % Descr rev_x_x
emit_ins_2(put_x_variable,r) -->
	"LoadH;\n",
	emit_goto_ins(put_x_variable,w).
emit_ins_2(put_x_variable,w) -->
	"Load2HVA(Xb(P1),Xb(P2),H);\n",
	emit_dispatch_w("2").

ins_op_format(put_xval_xval, 85, "xxxx"). % Descr rev_x_x_x_x
emit_ins_2(put_xval_xval,r) -->
	"Xb(P1) = Xb(P2);\n",
	"Xb(P3) = Xb(P4);\n",
	emit_dispatch_r("4").
emit_ins_2(put_xval_xval,w) -->
	"Xb(P1) = Xb(P2);\n",
	"Xb(P3) = Xb(P4);\n",
	emit_dispatch_w("4").

ins_op_format(put_x_value, 71, "xx"). % Descr rev_x_x
emit_ins_2(put_x_value,r) -->
	"Xb(P1) = Xb(P2);\n",
	emit_dispatch_r("2").
emit_ins_2(put_x_value,w) -->
	"Xb(P1) = Xb(P2);\n",
	emit_dispatch_w("2").

ins_op_format(put_x_unsafe_value, 72, "xx"). % Descr rev_x_x
emit_ins_2(put_x_unsafe_value,r) -->
	"LoadH;\n",
	emit_goto_ins(put_x_unsafe_value,w).
emit_ins_2(put_x_unsafe_value,w) -->
	"RefStackUnsafe(Xb(P1),&Xb(P2));\n",
	"Xb(P2) = t0;\n",
	emit_dispatch_w("2").

ins_op_format(put_y_first_variable, 73, "xyv"). % Descr rev_yv_x
emit_ins_2(put_y_first_variable,r) -->
	"LoadH;\n",
	emit_goto_ins(put_y_first_variable,w).
emit_ins_2(put_y_first_variable,w) -->
	"ComputeE;\n",
	emit_goto_ins(put_y_variable,w).

ins_op_format(put_y_variable, 74, "xyv"). % Descr rev_yv_x
emit_ins_2(put_y_variable,r) -->
	"LoadH;\n",
	emit_goto_ins(put_y_variable,w).
emit_ins_2(put_y_variable,w) -->
	"t0 = P2;\n",
	"Load2SVA(Xb(P1),Yb(t0));\n",
	emit_dispatch_w("2").

ins_op_format(put_yfvar_yvar, 83, "xyvxyv"). % Descr rev_yv_x_yv_x
emit_ins_2(put_yfvar_yvar,r) -->
	"LoadH;\n",
	emit_goto_ins(put_yfvar_yvar,w).
emit_ins_2(put_yfvar_yvar,w) -->
	"ComputeE;\n",
	emit_goto_ins(put_yvar_yvar,w).

ins_op_format(put_yvar_yvar, 84, "xyvxyv"). % Descr rev_yv_x_yv_x
emit_ins_2(put_yvar_yvar,r) -->
	"LoadH;\n",
	emit_goto_ins(put_yvar_yvar,w).
emit_ins_2(put_yvar_yvar,w) -->
	"t0 = P2;\n",
	"Load2SVA(Xb(P1),Yb(t0));\n",
	"t0 = P4;\n",
	"Load2SVA(Xb(P3),Yb(t0));\n",
	emit_dispatch_w("4").

ins_op_format(put_yval_yval, 86, "xyxy"). % Descr rev_y_x_y_x
emit_ins_2(put_yval_yval,r) -->
	"RefStack(Xb(P1),&Yb(P2));\n",
	"RefStack(Xb(P3),&Yb(P4));\n",
	emit_dispatch_r("4").
emit_ins_2(put_yval_yval,w) -->
	"RefStack(Xb(P1),&Yb(P2));\n",
	"RefStack(Xb(P3),&Yb(P4));\n",
	emit_dispatch_w("4").

ins_op_format(put_y_value, 75, "xy"). % Descr rev_y_x
emit_ins_2(put_y_value,r) -->
	"RefStack(Xb(P1),&Yb(P2));\n",
	emit_dispatch_r("2").
emit_ins_2(put_y_value,w) -->
	"RefStack(Xb(P1),&Yb(P2));\n",
	emit_dispatch_w("2").

ins_op_format(put_y_unsafe_value, 76, "xy"). % Descr rev_y_x
emit_ins_2(put_y_unsafe_value,r) -->
	"LoadH;\n",
	emit_goto_ins(put_y_unsafe_value,w).
emit_ins_2(put_y_unsafe_value,w) -->
	"RefStackUnsafe(Xb(P1),&Yb(P2));\n",
	emit_dispatch_w("2").

ins_op_format(put_constantq, 77, "Qxt").
emit_ins_2(put_constantq,r) -->
	"Xb(P2) = T3;\n",
	emit_dispatch_r("2+BPTP").
emit_ins_2(put_constantq,w) -->
	"Xb(P2) = T3;\n",
	emit_dispatch_w("2+BPTP").

ins_op_format(put_constant, 78, "xt"). % Descr tagged_x
emit_ins_2(put_constant,r) -->
	"Xb(P1) = T2;\n",
	emit_dispatch_r("1+BPTP").
emit_ins_2(put_constant,w) -->
	"Xb(P1) = T2;\n",
	emit_dispatch_w("1+BPTP").

ins_op_format(put_nil, 81, "x"). % Descr x
emit_ins_2(put_nil,r) -->
	"Xb(P1) = atom_nil;\n",
	emit_dispatch_r("1").
emit_ins_2(put_nil,w) -->
	"Xb(P1) = atom_nil;\n",
	emit_dispatch_w("1").

ins_op_format(put_largeq, 252, "QxL").
emit_ins_2(put_largeq,r) -->
	"LoadH;\n",
	emit_goto_ins(put_largeq,w).
emit_ins_2(put_largeq,w) -->
	"StoreH;\n",
	"Xb(P2) = MakeLarge(Arg,Pplus3);\n",
	"LoadH;\n",
	emit_dispatch_w("2+LargeInsns(T3)").

ins_op_format(put_large, 253, "xL"). % Descr large_x
emit_ins_2(put_large,r) -->
	"LoadH;\n",
	emit_goto_ins(put_large,w).
emit_ins_2(put_large,w) -->
	"StoreH;\n",
	"Xb(P1) = MakeLarge(Arg,Pplus2);\n",
	"LoadH;\n",
	emit_dispatch_w("1+LargeInsns(T2)").

ins_op_format(put_structureq, 79, "Qxf").
emit_ins_2(put_structureq,r) -->
	"LoadH;\n",
	emit_goto_ins(put_structureq,w).
emit_ins_2(put_structureq,w) -->
	"Xb(P2) = Tag(STR,H);\n",
	"HeapPush(H,T3);\n",
	emit_dispatch_w("2+BPTP").

ins_op_format(put_structure, 80, "xf"). % Descr functor_x
emit_ins_2(put_structure,r) -->
	"LoadH;\n",
	emit_goto_ins(put_structure,w).
emit_ins_2(put_structure,w) -->
	"Xb(P1) = Tag(STR,H);\n",
	"HeapPush(H,T2);\n",
	emit_dispatch_w("1+BPTP").

ins_op_format(put_list, 82, "x"). % Descr x
emit_ins_2(put_list,r) -->
	"LoadH;\n",
	emit_goto_ins(put_list,w).
emit_ins_2(put_list,w) -->
	"Xb(P1) = Tag(LST,H);\n",
	emit_dispatch_w("1").

ins_op_format(put_yval_yuval, 87, "xyxy"). % Descr rev_y_x_y_x
emit_ins_2(put_yval_yuval,r) -->
	"LoadH;\n",
	emit_goto_ins(put_yval_yuval,w).
emit_ins_2(put_yval_yuval,w) -->
	"RefStack(Xb(P1),&Yb(P2));\n",
	"RefStackUnsafe(Xb(P3),&Yb(P4));\n",
	emit_dispatch_w("4").

ins_op_format(put_yuval_yval, 88, "xyxy"). % Descr rev_y_x_y_x
emit_ins_2(put_yuval_yval,r) -->
	"LoadH;\n",
	emit_goto_ins(put_yuval_yval,w).
emit_ins_2(put_yuval_yval,w) -->
	"RefStackUnsafe(Xb(P1),&Yb(P2));\n",
	"RefStack(Xb(P3),&Yb(P4));\n",
	emit_dispatch_w("4").

ins_op_format(put_yuval_yuval, 89, "xyxy"). % Descr rev_y_x_y_x
emit_ins_2(put_yuval_yuval,r) -->
	"LoadH;\n",
	emit_goto_ins(put_yuval_yuval,w).
emit_ins_2(put_yuval_yuval,w) -->
	"RefStackUnsafe(Xb(P1),&Yb(P2));\n",
	"RefStackUnsafe(Xb(P3),&Yb(P4));\n",
	emit_dispatch_w("4").

ins_op_format(get_x_value, 91, "xx"). % Descr rev_x_x
emit_ins_2(get_x_value,r) -->
	"EUNIFY(Xb(P2),Xb(P1),2);\n".
emit_ins_2(get_x_value,w) -->
	"StoreH;\n",
	emit_goto_ins(get_x_value,r).

ins_op_format(get_y_first_value, 94, "xy"). % Descr rev_y_x
emit_ins_2(get_y_first_value,r) -->
	"GetFirstValue(Yb(P2),Xb(P1));\n",
	emit_dispatch_r("2").
emit_ins_2(get_y_first_value,w) -->
	"StoreH;\n",
	emit_goto_ins(get_y_first_value,r).

ins_op_format(get_y_value, 95, "xy"). % Descr rev_y_x
emit_ins_2(get_y_value,r) -->
	"RefStack(t1,&Yb(P2));\n",
	"EUNIFY(Xb(P1),t1,2);\n".
emit_ins_2(get_y_value,w) -->
	"StoreH;\n",
	emit_goto_ins(get_y_value,r).

ins_op_format(get_constantq, 96, "Qxt").
emit_ins_2(get_constantq,r) -->
	emit_shift("Q"),
	emit_goto_ins(get_constant,r).
emit_ins_2(get_constantq,w) -->
	emit_shift("Q"),
	emit_goto_ins(get_constant,w).

ins_op_format(get_constant, 97, "xt"). % Descr tagged_x
emit_ins_2(get_constant,r) -->
	"Unify_atom(T2,Xb(P1));\n",
	emit_dispatch_r("1+BPTP").
emit_ins_2(get_constant,w) -->
	"StoreH;\n",
	emit_goto_ins(get_constant,r).

ins_op_format(get_largeq, 254, "QxL").
emit_ins_2(get_largeq,r) -->
	emit_shift("Q"),
	emit_goto_ins(get_large,r).
emit_ins_2(get_largeq,w) -->
	emit_shift("Q"),
	emit_goto_ins(get_large,w).

ins_op_format(get_large, 255, "xL"). % Descr large_x
emit_ins_2(get_large,r) -->
	"Unify_large(Arg,Pplus2, Xb(P1));\n",
	emit_dispatch_r("1+LargeInsns(T2)").
emit_ins_2(get_large,w) -->
	"StoreH;\n",
	emit_goto_ins(get_large,r).

ins_op_format(get_structureq, 98, "Qxf").
emit_ins_2(get_structureq,r) -->
	emit_shift("Q"),
	emit_goto_ins(get_structure,r).
emit_ins_2(get_structureq,w) -->
	emit_shift("Q"),
	emit_goto_ins(get_structure,w).

ins_op_format(get_structure, 99, "xf"). % Descr functor_x
emit_ins_2(get_structure,r) -->
	"Unify_structure(T2,Xb(P1),\n",
	"                {", emit_dispatch_r("1+BPTP"), "},\n",
	"                {", emit_dispatch_w("1+BPTP"), "})\n".
emit_ins_2(get_structure,w) -->
	"StoreH;\n",
	emit_goto_ins(get_structure,r).

ins_op_format(get_nil, 100, "x"). % Descr x
emit_ins_2(get_nil,r) -->
	"Unify_atom(atom_nil,Xb(P1));\n",
	emit_dispatch_r("1").
emit_ins_2(get_nil,w) -->
	"StoreH;\n",
	emit_goto_ins(get_nil,r).

ins_op_format(get_list, 101, "x"). % Descr x
emit_ins_2(get_list,r) -->
	"Unify_list(Xb(P1),\n",
	"           {", emit_dispatch_r("1"), "},\n",
	"           {", emit_dispatch_w("1"), "})\n".
emit_ins_2(get_list,w) -->
	"StoreH;\n",
	emit_goto_ins(get_list,r).

ins_op_format(get_constant_neck_proceedq, 111, "Qxt").
emit_ins_2(get_constant_neck_proceedq,r) -->
	emit_shift("Q"),
	emit_goto_ins(get_constant_neck_proceed,r).
emit_ins_2(get_constant_neck_proceedq,w) -->
	emit_shift("Q"),
	emit_goto_ins(get_constant_neck_proceed,w).

ins_op_format(get_constant_neck_proceed, 112, "xt"). % Descr tagged_x
emit_ins_2(get_constant_neck_proceed,r) -->
	"Unify_atom(T2,Xb(P1));\n",
	"LoadH;\n",
	emit_goto_ins(neck_proceed,w).
emit_ins_2(get_constant_neck_proceed,w) -->
	"StoreH;\n",
	emit_goto_ins(get_constant_neck_proceed,r).

ins_op_format(get_nil_neck_proceed, 113, "x"). % Descr x
emit_ins_2(get_nil_neck_proceed,r) -->
	"Unify_atom(atom_nil,Xb(P1));\n",
	"LoadH;\n",
	emit_goto_ins(neck_proceed,w).
emit_ins_2(get_nil_neck_proceed,w) -->
	"StoreH;\n",
	emit_goto_ins(get_nil_neck_proceed,r).

ins_op_format(cutb_x, 208, "x"). % Descr x
emit_ins_2(cutb_x,r) -->
	"w->local_top = 0; /* may get hole at top of local stack */\n",
	"w->next_node = ChoiceFromInt(Xb(P1));\n",
	"DOCUT;\n",
	emit_dispatch_r("1").
emit_ins_2(cutb_x,w) -->
	"StoreH;\n",
	emit_goto_ins(cutb_x,r).

ins_op_format(cutb_x_neck, 210, "x"). % Descr x
emit_ins_2(cutb_x_neck,r) -->
	"w->local_top = 0; /* may get hole at top of local stack */\n",
	"w->next_node = ChoiceFromInt(Xb(Pnext));\n".
emit_ins_2(cutb_x_neck,w) -->
	"StoreH;\n",
	emit_goto_ins(cutb_x_neck,r).

ins_op_format(cutb_neck, 211, ""). % Descr none
emit_ins_2(cutb_neck,r) -->
	"DOCUT;\n",
	"if (w->next_alt) {\n",
	"  w->next_alt = NULL;\n",
	"  if (ChoiceYounger(ChoiceOffset(B,CHOICEPAD),w->trail_top))\n",
	"    choice_overflow(Arg,CHOICEPAD);\n",
	"}\n",
	emit_dispatch_r("0").
emit_ins_2(cutb_neck,w) -->
	"StoreH;\n",
	emit_goto_ins(cutb_neck,r).

ins_op_format(cutb_x_neck_proceed, 212, "x"). % Descr x
emit_ins_2(cutb_x_neck_proceed,r) -->
	"w->next_node = ChoiceFromInt(Xb(P1)); /* P++ */\n",
	"/* w->local_top = 0; done by CODE_PROCEED */\n",
	emit_goto_ins(cutb_neck_proceed,r).
emit_ins_2(cutb_x_neck_proceed,w) -->
	"StoreH;\n",
	emit_goto_ins(cutb_x_neck_proceed,r).

ins_op_format(cutb_neck_proceed, 213, ""). % Descr none
emit_ins_2(cutb_neck_proceed,r) -->
	"DOCUT;\n",
	"if (w->next_alt) {\n",
	"  w->next_alt = NULL;\n",
	"  if (ChoiceYounger(ChoiceOffset(B,CHOICEPAD),w->trail_top))\n",
	"    choice_overflow(Arg,CHOICEPAD);\n",
	"}\n",
	"goto proceed_r;\n".
emit_ins_2(cutb_neck_proceed,w) -->
	"StoreH;\n",
	emit_goto_ins(cutb_neck_proceed,r).

ins_op_format(cute_x, 214, "x"). % Descr x
emit_ins_2(cute_x,r) -->
	"w->next_node = ChoiceFromInt(Xb(P1));\n",
	"w->local_top = E; /* w->local_top may be 0 here. */\n",
	"DOCUT;\n",
	"SetE(w->local_top);\n",
	emit_dispatch_r("1").
emit_ins_2(cute_x,w) -->
	"StoreH;\n",
	emit_goto_ins(cute_x,r).

ins_op_format(cute_x_neck, 216, "x"). % Descr x
emit_ins_2(cute_x_neck,r) -->
	"w->next_node = ChoiceFromInt(Xb(Pnext));\n",
	emit_goto_ins(cute_neck,r).
emit_ins_2(cute_x_neck,w) -->
	"StoreH;\n",
	emit_goto_ins(cute_x_neck,r).

ins_op_format(cute_neck, 217, ""). % Descr none
emit_ins_2(cute_neck,r) -->
	"w->local_top = E; /* w->local_top may be 0 here. */\n",
	"DOCUT;\n",
	"/* w->next_alt can't be NULL here */\n",
	"w->next_alt = NULL;\n",
	"if (ChoiceYounger(ChoiceOffset(B,CHOICEPAD),w->trail_top))\n",
	"     choice_overflow(Arg,CHOICEPAD);\n",
	"SetE(w->local_top);\n",
	emit_dispatch_r("0").
emit_ins_2(cute_neck,w) -->
	"StoreH;\n",
	emit_goto_ins(cute_neck,r).

ins_op_format(cutf_x, 215, "x"). % Descr x
emit_ins_2(cutf_x,r) -->
	"w->next_node = ChoiceFromInt(Xb(Pnext));\n",
	emit_goto_ins(cutf,r).
emit_ins_2(cutf_x,w) -->
	"StoreH;\n",
	emit_goto_ins(cutf_x,r).

ins_op_format(cutf, 209, ""). % Descr none
emit_ins_2(cutf,r) -->
	"DOCUT;\n",
	"SetE(w->frame);\n",
	emit_dispatch_r("0").
emit_ins_2(cutf,w) -->
	"StoreH;\n",
	emit_goto_ins(cutf,r).

ins_op_format(cut_y, 218, "y"). % Descr y
emit_ins_2(cut_y,r) -->
	"RefStack(t1,&Yb(P1));\n",
	"w->next_node = ChoiceFromInt(t1);\n",
	"DOCUT;\n",
	"SetE(w->frame);\n",
	emit_dispatch_r("1").
emit_ins_2(cut_y,w) -->
	"StoreH;\n",
	emit_goto_ins(cut_y,r).

ins_op_format(choice_x, 219, "x"). % Descr x
emit_ins_2(choice_x,r) -->
	"Xb(P1) = ChoiceToInt(w->next_node);\n",
	emit_dispatch_r("1").
emit_ins_2(choice_x,w) -->
	"Xb(P1) = ChoiceToInt(w->next_node);\n",
	emit_dispatch_w("1").

ins_op_format(choice_yf, 220, "y"). % Descr y
emit_ins_2(choice_yf,r) -->
	"ComputeE;\n",
	emit_goto_ins(choice_y,r).
emit_ins_2(choice_yf,w) -->
	"ComputeE;\n",
	emit_goto_ins(choice_y,w).

ins_op_format(choice_y, 221, "y"). % Descr y
emit_ins_2(choice_y,r) -->
	"Yb(P1) = ChoiceToInt(w->next_node);\n",
	emit_dispatch_r("1").
emit_ins_2(choice_y,w) -->
	"Yb(P1) = ChoiceToInt(w->next_node);\n",
	emit_dispatch_w("1").

ins_op_format(kontinue, 233, "").
emit_ins_2(kontinue,r) -->
	"LoadH;\n",
	emit_goto_ins(kontinue,w).
emit_ins_2(kontinue,w) -->
	"/* after wakeup, write mode! */",
	"Setfunc(TagToFunctor(Y(0)));\n",
	"for(i=0; i<Func->arity; i++) X(i) = Y(i+1);\n",
	"DEALLOCATE;\n",
	"goto enter_predicate;\n".

ins_op_format(leave, 234, "").
emit_ins_2(leave,r) -->
	emit_goto_ins(exit_toplevel,r).
emit_ins_2(leave,w) -->
	"StoreH;\n",
	emit_goto_ins(leave,r).

ins_op_format(exit_toplevel, 235, "").
emit_ins_2(exit_toplevel,r) -->
	"goto exit_toplevel;\n".
emit_ins_2(exit_toplevel,w) -->
	"StoreH;\n",
	emit_goto_ins(exit_toplevel,r).

ins_op_format(retry_cq, 237, "QC").
emit_ins_2(retry_cq,r) -->
	"if (w->next_alt)\n",
	"  B->next_alt = w->next_alt, w->next_alt = NULL;\n",
	"if (!C2(Arg))\n",
	"  goto fail;\n",
	"goto proceed_r;\n".
emit_ins_2(retry_cq,w) -->
	"StoreH;\n",
	emit_goto_ins(retry_cq,r).

ins_op_format(retry_c, 238, "C").
emit_ins_2(retry_c,r) -->
	"if (w->next_alt)\n",
	"  B->next_alt = w->next_alt, w->next_alt = NULL;\n",
	"if (!C1(Arg))\n",
	"  goto fail;\n",
	"goto proceed_r;\n".
emit_ins_2(retry_c,w) -->
	"StoreH;\n",
	emit_goto_ins(retry_c,r).

ins_op_format(get_structure_x0q, 104, "Qf").
emit_ins_2(get_structure_x0q,r) -->
	"S = TagToArg(t0,1);\n",
	emit_dispatch_r("1+BPTP").
emit_ins_2(get_structure_x0q,w) -->
	emit_shift("Q"),
	emit_goto_ins(get_structure_x0,w).

ins_op_format(get_structure_x0, 105, "f"). % Descr functor
emit_ins_2(get_structure_x0,r) -->
	"S = TagToArg(t0,1);\n",
	emit_dispatch_r("0+BPTP").
emit_ins_2(get_structure_x0,w) -->
	"t1 = Tag(STR,H);\n",
	"if (TagIsHVAw(t0))\n",
	"  BindHVA(t0,t1)\n",
	"else if (t0 & TagBitSVA)\n",
	"  BindSVA(t0,t1)\n",
	"else\n",
	"  { BindCVA(t0,t1); Wake; }\n",
	"HeapPush(H,T1);\n",
	emit_dispatch_w("0+BPTP").

ins_op_format(get_large_x0q, 256, "QL").
emit_ins_2(get_large_x0q,r) -->
	"Unify_large(Arg,Pplus2, t0);\n",
	emit_dispatch_r("1+LargeInsns(T2)").
emit_ins_2(get_large_x0q,w) -->
	emit_shift("Q"),
	emit_goto_ins(get_large_x0,w).

ins_op_format(get_large_x0, 257, "L"). % Descr large
emit_ins_2(get_large_x0,r) -->
	"Unify_large(Arg,Pplus1, t0);\n",
	emit_dispatch_r("0+LargeInsns(T1)").
emit_ins_2(get_large_x0,w) -->
	"StoreH;\n",
	"t1 = MakeLarge(Arg,Pplus1);\n",
	"LoadH;\n",
	"if (TagIsHVAw(t0))\n",
	"  BindHVA(t0,t1)\n",
	"else if (t0 & TagBitSVA)\n",
	"  BindSVA(t0,t1)\n",
	"else\n",
	"  { BindCVA(t0,t1); Wake; }\n",
	emit_dispatch_w("0+LargeInsns(T1)").

ins_op_format(get_constant_x0q, 102, "Qt").
emit_ins_2(get_constant_x0q,r) -->
	emit_dispatch_r("1+BPTP").
emit_ins_2(get_constant_x0q,w) -->
	emit_shift("Q"),
	emit_goto_ins(get_constant_x0,w).

ins_op_format(get_constant_x0, 103, "t"). % Descr tagged
emit_ins_2(get_constant_x0,r) -->
	emit_dispatch_r("0+BPTP").
emit_ins_2(get_constant_x0,w) -->
	"if (TagIsHVAw(t0))\n",
	"  BindHVA(t0,T1)\n",
	"else if (t0 & TagBitSVA)\n",
	"  BindSVA(t0,T1)\n",
	"else\n",
	"  { BindCVA(t0,T1); Wake; }\n",
	emit_dispatch_w("0+BPTP").

ins_op_format(get_nil_x0, 106, ""). % Descr none
emit_ins_2(get_nil_x0,r) -->
	emit_dispatch_r("0").
emit_ins_2(get_nil_x0,w) -->
	"if (TagIsHVAw(t0))\n",
	"  BindHVA(t0,atom_nil)\n",
	"else if (t0 & TagBitSVA)\n",
	"  BindSVA(t0,atom_nil)\n",
	"else\n",
	"  { BindCVA(t0,atom_nil); Wake; }\n",
	emit_dispatch_w("0").

ins_op_format(get_list_x0, 107, ""). % Descr none
emit_ins_2(get_list_x0,r) -->
	"S = TagToLST(t0);\n",
	emit_dispatch_r("0").
emit_ins_2(get_list_x0,w) -->
	"t1 = Tag(LST,H);\n",
	"if (TagIsHVAw(t0))\n",
	"  BindHVA(t0,t1)\n",
	"else if (t0 & TagBitSVA)\n",
	"  BindSVA(t0,t1)\n",
	"else\n",
	"  { BindCVA(t0,t1); Wake; }\n",
	emit_dispatch_w("0").

ins_op_format(get_xvar_xvar, 108, "xxxx"). % Descr rev_x_x_x_x
emit_ins_2(get_xvar_xvar,r) -->
	"Xb(P2) = Xb(P1);\n",
	"Xb(P4) = Xb(P3);\n",
	emit_dispatch_r("4").
emit_ins_2(get_xvar_xvar,w) -->
	"Xb(P2) = Xb(P1);\n",
	"Xb(P4) = Xb(P3);\n",
	emit_dispatch_w("4").

ins_op_format(get_x_variable, 90, "xx"). % Descr rev_x_x
emit_ins_2(get_x_variable,r) -->
	"Xb(P2) = Xb(P1);\n",
	emit_dispatch_r("2").
emit_ins_2(get_x_variable,w) -->
	"Xb(P2) = Xb(P1);\n",
	emit_dispatch_w("2").

ins_op_format(get_y_first_variable, 92, "xy"). % Descr rev_y_x
emit_ins_2(get_y_first_variable,r) -->
	"ComputeE;\n",
	emit_goto_ins(get_y_variable,r).
emit_ins_2(get_y_first_variable,w) -->
	"ComputeE;\n",
	emit_goto_ins(get_y_variable,w).

ins_op_format(get_y_variable, 93, "xy"). % Descr rev_y_x
emit_ins_2(get_y_variable,r) -->
	"Yb(P2) = Xb(P1);\n",
	emit_dispatch_r("2").
emit_ins_2(get_y_variable,w) -->
	"Yb(P2) = Xb(P1);\n",
	emit_dispatch_w("2").

ins_op_format(get_yfvar_yvar, 109, "xyxy"). % Descr rev_y_x_y_x
emit_ins_2(get_yfvar_yvar,r) -->
	"ComputeE;\n",
	emit_goto_ins(get_yvar_yvar,r).
emit_ins_2(get_yfvar_yvar,w) -->
	"ComputeE;\n",
	emit_goto_ins(get_yvar_yvar,w).

ins_op_format(get_yvar_yvar, 110, "xyxy"). % Descr rev_y_x_y_x
emit_ins_2(get_yvar_yvar,r) -->
	"Yb(P2) = Xb(P1);\n",
	"Yb(P4) = Xb(P3);\n",
	emit_dispatch_r("4").
emit_ins_2(get_yvar_yvar,w) -->
	"Yb(P2) = Xb(P1);\n",
	"Yb(P4) = Xb(P3);\n",
	emit_dispatch_w("4").

ins_op_format(branch, 68, "i"). % Descr i
emit_ins_2(branch,r) -->
	"P = Pdeep;\n",
	emit_dispatch_r("0").
emit_ins_2(branch,w) -->
	"P = Pdeep;\n",
	emit_dispatch_w("0").

ins_op_format(function_1q, 222, "QxxCli").
emit_ins_2(function_1q,r) -->
	"Numstack_End = NULL;\n",
	"if (ERRORTAG==(Xb(P2) = (TAGGED)C4(Arg,Xb(P3),Pplus6)))\n",
	"     goto fail;\n",
	emit_dispatch_r("4+2*BPTP").
emit_ins_2(function_1q,w) -->
	"StoreH;\n",
	emit_goto_ins(function_1q,r).

ins_op_format(function_1, 223, "xxCli"). % Descr function_x_x
emit_ins_2(function_1,r) -->
	"Numstack_End = NULL;\n",
	"if (ERRORTAG==(Xb(P1) = (TAGGED)C3(Arg,Xb(P2),Pplus5)))\n",
	"     goto fail;\n",
	emit_dispatch_r("3+2*BPTP").
emit_ins_2(function_1,w) -->
	"StoreH;\n",
	emit_goto_ins(function_1,r).

ins_op_format(function_2q, 224, "QxxxCli").
emit_ins_2(function_2q,r) -->
	"Numstack_End = NULL;\n",
	"if (ERRORTAG==(Xb(P2) = (TAGGED)C5(Arg,Xb(P3),Xb(P4),Pplus7)))\n",
	"     goto fail;\n",
	emit_dispatch_r("5+2*BPTP").
emit_ins_2(function_2q,w) -->
	"StoreH;\n",
	emit_goto_ins(function_2q,r).

ins_op_format(function_2, 225, "xxxCli"). % Descr function_x_x_x
emit_ins_2(function_2,r) -->
	"Numstack_End = NULL;\n",
	"if (ERRORTAG==(Xb(P1) = (TAGGED)C4(Arg,Xb(P2),Xb(P3),Pplus6)))\n",
	"     goto fail;\n",
	emit_dispatch_r("4+2*BPTP").
emit_ins_2(function_2,w) -->
	"StoreH;\n",
	emit_goto_ins(function_2,r).

ins_op_format(builtin_1q, 226, "QxC").
emit_ins_2(builtin_1q,r) -->
	"if (!C3(Arg,Xb(P2))) goto fail;\n",
	emit_dispatch_r("2+BPTP").
emit_ins_2(builtin_1q,w) -->
	"StoreH;\n",
	emit_goto_ins(builtin_1q,r).

ins_op_format(builtin_1, 227, "xC"). % Descr builtin_x
emit_ins_2(builtin_1,r) -->
	"if (!C2(Arg,Xb(P1))) goto fail;\n",
	emit_dispatch_r("1+BPTP").
emit_ins_2(builtin_1,w) -->
	"StoreH;\n",
	emit_goto_ins(builtin_1,r).

ins_op_format(builtin_2q, 228, "QxxC").
emit_ins_2(builtin_2q,r) -->
	"if (!C4(Arg,Xb(P2),Xb(P3))) goto fail;\n",
	emit_dispatch_r("3+BPTP").
emit_ins_2(builtin_2q,w) -->
	"StoreH;\n",
	emit_goto_ins(builtin_2q,r).

ins_op_format(builtin_2, 229, "xxC"). % Descr builtin_x_x
emit_ins_2(builtin_2,r) -->
	"if (!C3(Arg,Xb(P1),Xb(P2))) goto fail;\n",
	emit_dispatch_r("2+BPTP").
emit_ins_2(builtin_2,w) -->
	"StoreH;\n",
	emit_goto_ins(builtin_2,r).

ins_op_format(builtin_3q, 230, "QxxxC").
emit_ins_2(builtin_3q,r) -->
	"if (!C5(Arg,Xb(P2),Xb(P3),Xb(P4))) goto fail;\n",
	emit_dispatch_r("4+BPTP").
emit_ins_2(builtin_3q,w) -->
	"StoreH;\n",
	emit_goto_ins(builtin_3q,r).

ins_op_format(builtin_3, 231, "xxxC"). % Descr builtin_x_x_x
emit_ins_2(builtin_3,r) -->
	"if (!C4(Arg,Xb(P1),Xb(P2),Xb(P3))) goto fail;\n",
	emit_dispatch_r("3+BPTP").
emit_ins_2(builtin_3,w) -->
	"StoreH;\n",
	emit_goto_ins(builtin_3,r).

ins_op_format(retry_instance, 232, "").
emit_ins_2(retry_instance,r) -->
	"CODE_RETRY_INSTANCE;\n".
emit_ins_2(retry_instance,w) -->
	"/* backtracking into clause/2 */\n",
	"StoreH;\n",
	emit_goto_ins(retry_instance,r).

ins_op_format(get_constraint, 247, "x"). % Descr x
emit_ins_2(get_constraint,r) -->
	"LoadH;\n",
	emit_goto_ins(get_constraint,w).
emit_ins_2(get_constraint,w) -->
	"t1 = Xb(P1);\n",
	"LoadCVA(t2,H);\n",
	"SwitchOnVar(t1,t0,\n",
	"      {BindHVA(t1,t2); Xb(P1)=t2;},\n",
	"      {BindCVA(t2,t1); Wake;},\n",
	"      {BindSVA(t1,t2); Xb(P1)=t2;},\n",
	"      {BindCVA(t2,t1); Wake;});\n",
	emit_dispatch_w("1").

ins_op_format(unify_void, 114, "i"). % Descr voids
emit_ins_2(unify_void,r) -->
	"U1_VOID_R(P1);\n",
	emit_dispatch_r("1").
emit_ins_2(unify_void,w) -->
	"for (i=SP1, P++; i>4; --i) ConstrHVA(H);\n",
	emit_goto_ins(unify_void_4,w).

ins_op_format(unify_void_1, 115, "").
emit_ins_2(unify_void_1,r) -->
	"U1_VOID_R(1);\n",
	emit_dispatch_r("0").
emit_ins_2(unify_void_1,w) -->
	"ConstrHVA(H);\n",
	emit_dispatch_w("0").

ins_op_format(unify_void_2, 116, "").
emit_ins_2(unify_void_2,r) -->
	"U1_VOID_R(2);\n",
	emit_dispatch_r("0").
emit_ins_2(unify_void_2,w) -->
	"ConstrHVA(H);\n",
	emit_goto_ins(unify_void_1,w).

ins_op_format(unify_void_3, 117, "").
emit_ins_2(unify_void_3,r) -->
	"U1_VOID_R(3);\n",
	emit_dispatch_r("0").
emit_ins_2(unify_void_3,w) -->
	"ConstrHVA(H);\n",
	emit_goto_ins(unify_void_2,w).

ins_op_format(unify_void_4, 118, "").
emit_ins_2(unify_void_4,r) -->
	"U1_VOID_R(4);\n",
	emit_dispatch_r("0").
emit_ins_2(unify_void_4,w) -->
	"ConstrHVA(H);\n",
	emit_goto_ins(unify_void_3,w).

ins_op_format(unify_x_variable, 119, "x"). % Descr x
emit_ins_2(unify_x_variable,r) -->
	"RefHeapNext(Xb(P1),S);\n",
	emit_dispatch_r("1").
emit_ins_2(unify_x_variable,w) -->
	"U1_XVAR_W(P1);\n",
	emit_dispatch_w("1").

ins_op_format(unify_x_value, 120, "x"). % Descr x
emit_ins_2(unify_x_value,r) -->
	emit_goto_ins(unify_x_local_value,r).
emit_ins_2(unify_x_value,w) -->
	"U1_XVAL_W(P1);\n",
	emit_dispatch_w("1").

ins_op_format(unify_x_local_value, 121, "x"). % Descr x
emit_ins_2(unify_x_local_value,r) -->
	"U1_XVAL_R2(P1);\n".
emit_ins_2(unify_x_local_value,w) -->
	"U1_XLVAL_W(P1);\n",
	emit_dispatch_w("1").

ins_op_format(unify_y_first_variable, 122, "y"). % Descr y
emit_ins_2(unify_y_first_variable,r) -->
	"ComputeE;\n",
	emit_goto_ins(unify_y_variable,r).
emit_ins_2(unify_y_first_variable,w) -->
	"ComputeE;\n",
	emit_goto_ins(unify_y_variable,w).

ins_op_format(unify_y_variable, 123, "y"). % Descr y
emit_ins_2(unify_y_variable,r) -->
	"RefHeapNext(Yb(P1),S);\n",
	emit_dispatch_r("1").
emit_ins_2(unify_y_variable,w) -->
	"U1_YVAR_W(P1);\n",
	emit_dispatch_w("1").

ins_op_format(unify_y_first_value, 124, "y"). % Descr y
emit_ins_2(unify_y_first_value,r) -->
	"U1_YFVAL_R(P1);\n",
	emit_dispatch_r("1").
emit_ins_2(unify_y_first_value,w) -->
	"U1_YFVAL_W(P1);\n",
	emit_dispatch_w("1").

ins_op_format(unify_y_value, 125, "y"). % Descr y
emit_ins_2(unify_y_value,r) -->
	emit_goto_ins(unify_y_local_value,r).
emit_ins_2(unify_y_value,w) -->
	"U1_YVAL_W(P1);\n",
	emit_dispatch_w("1").

ins_op_format(unify_y_local_value, 126, "y"). % Descr y
emit_ins_2(unify_y_local_value,r) -->
	"U1_YVAL_R2(P1);\n".
emit_ins_2(unify_y_local_value,w) -->
	"U1_YLVAL_W(P1);\n",
	emit_dispatch_w("1").

ins_op_format(unify_constantq, 127, "Qt").
emit_ins_2(unify_constantq,r) -->
	emit_shift("Q"),
	emit_goto_ins(unify_constant,r).
emit_ins_2(unify_constantq,w) -->
	"HeapPush(H,T2);\n",
	emit_dispatch_w("1+BPTP").

ins_op_format(unify_constant, 128, "t"). % Descr tagged
emit_ins_2(unify_constant,r) -->
	"RefHeapNext(t1,S);\n",
	"Unify_heap_atom(T1,t1);\n",
	emit_dispatch_r("0+BPTP").
emit_ins_2(unify_constant,w) -->
	"HeapPush(H,T1);\n",
	emit_dispatch_w("0+BPTP").

ins_op_format(unify_largeq, 258, "QL").
emit_ins_2(unify_largeq,r) -->
	emit_shift("Q"),
	emit_goto_ins(unify_large,r).
emit_ins_2(unify_largeq,w) -->
	emit_shift("Q"),
	emit_goto_ins(unify_large,w).

ins_op_format(unify_large, 259, "L"). % Descr large
emit_ins_2(unify_large,r) -->
	"RefHeapNext(t1,S);\n",
	"Unify_heap_large(Arg,Pplus1, t1);\n",
	emit_dispatch_r("0+LargeInsns(T1)").
emit_ins_2(unify_large,w) -->
	"w->global_top = HeapOffset(H,1);\n",
	"*H = MakeLarge(Arg,Pplus1);\n",
	emit_dispatch_r("0+LargeInsns(T1)").

ins_op_format(unify_structureq, 129, "Qf").
emit_ins_2(unify_structureq,r) -->
	emit_shift("Q"),
	emit_goto_ins(unify_structure,r).
emit_ins_2(unify_structureq,w) -->
	"HeapPush(H,Tag(STR,HeapOffset(H,1)));\n",
	"HeapPush(H,T2);\n",
	emit_dispatch_w("1+BPTP").

ins_op_format(unify_structure, 130, "f"). % Descr functor
emit_ins_2(unify_structure,r) -->
	"RefHeapNext(t1,S);\n",
	"Unify_heap_structure(T1,t1,\n",
	"		{", emit_dispatch_r("0+BPTP"), "},\n",
	"		{", emit_dispatch_w("0+BPTP"), "})\n".
emit_ins_2(unify_structure,w) -->
	"HeapPush(H,Tag(STR,HeapOffset(H,1)));\n",
	"HeapPush(H,T1);\n",
	emit_dispatch_w("0+BPTP").

ins_op_format(unify_nil, 131, ""). % Descr none
emit_ins_2(unify_nil,r) -->
	"RefHeapNext(t1,S);\n",
	"Unify_heap_atom(atom_nil,t1);\n",
	emit_dispatch_r("0").
emit_ins_2(unify_nil,w) -->
	"HeapPush(H,atom_nil);\n",
	emit_dispatch_w("0").

ins_op_format(unify_list, 132, ""). % Descr none
emit_ins_2(unify_list,r) -->
	"RefHeapNext(t1,S);\n",
	"Unify_heap_list(t1,\n",
	"		{", emit_dispatch_r("0"), "},\n",
	"		{", emit_dispatch_w("0"), "})\n".
emit_ins_2(unify_list,w) -->
	"HeapPush(H,Tag(LST,HeapOffset(H,1)));\n",
	emit_dispatch_w("0").

ins_op_format(unify_constant_neck_proceedq, 133, "Qt").
emit_ins_2(unify_constant_neck_proceedq,r) -->
	emit_shift("Q"),
	emit_goto_ins(unify_constant_neck_proceed,r).
emit_ins_2(unify_constant_neck_proceedq,w) -->
	"HeapPush(H,T2);\n",
	emit_goto_ins(neck_proceed,w).

ins_op_format(unify_constant_neck_proceed, 134, "t"). % Descr tagged
emit_ins_2(unify_constant_neck_proceed,r) -->
	"RefHeapNext(t1,S);\n",
	"Unify_heap_atom(T1,t1);\n",
	"LoadH;\n",
	emit_goto_ins(neck_proceed,w).
emit_ins_2(unify_constant_neck_proceed,w) -->
	"HeapPush(H,T1);\n",
	emit_goto_ins(neck_proceed,w).

ins_op_format(unify_nil_neck_proceed, 135, ""). % Descr none
emit_ins_2(unify_nil_neck_proceed,r) -->
	"RefHeapNext(t1,S);\n",
	"Unify_heap_atom(atom_nil,t1);\n",
	"LoadH;\n",
	emit_goto_ins(neck_proceed,w).
emit_ins_2(unify_nil_neck_proceed,w) -->
	"HeapPush(H,atom_nil);\n",
	emit_goto_ins(neck_proceed,w).

ins_op_format(u2_void_xvar, 136, "ix"). % Descr i_x
emit_ins_2(u2_void_xvar,r) -->
	"U1_VOID_R(P1);\n",
	"U1_XVAR_R(P2);\n",
	emit_dispatch_r("2").
emit_ins_2(u2_void_xvar,w) -->
	"U1_VOID_W(SP1);\n",
	"U1_XVAR_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_void_yfvar, 139, "iy"). % Descr i_y
emit_ins_2(u2_void_yfvar,r) -->
	"ComputeE;\n",
	emit_goto_ins(u2_void_yvar,r).
emit_ins_2(u2_void_yfvar,w) -->
	"ComputeE;\n",
	emit_goto_ins(u2_void_yvar,w).

ins_op_format(u2_void_yvar, 140, "iy"). % Descr i_y
emit_ins_2(u2_void_yvar,r) -->
	"U1_VOID_R(P1);\n",
	"U1_YVAR_R(P2);\n",
	emit_dispatch_r("2").
emit_ins_2(u2_void_yvar,w) -->
	"U1_VOID_W(SP1);\n",
	"U1_YVAR_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_void_xval, 137, "ix"). % Descr i_x
emit_ins_2(u2_void_xval,r) -->
	emit_goto_ins(u2_void_xlval,r).
emit_ins_2(u2_void_xval,w) -->
	"U1_VOID_W(SP1);\n",
	"U1_XVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_void_xlval, 138, "ix"). % Descr i_x
emit_ins_2(u2_void_xlval,r) -->
	"U1_VOID_R(P1);\n",
	"U1_XVAL_R3(P2);\n".
emit_ins_2(u2_void_xlval,w) -->
	"U1_VOID_W(SP1);\n",
	"U1_XLVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_void_yfval, 141, "iy"). % Descr i_y
emit_ins_2(u2_void_yfval,r) -->
	"U1_VOID_R(P1);\n",
	"U1_YFVAL_R(P2);\n",
	emit_dispatch_r("2").
emit_ins_2(u2_void_yfval,w) -->
	"U1_VOID_W(SP1);\n",
	"U1_YFVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_void_yval, 142, "iy"). % Descr i_y
emit_ins_2(u2_void_yval,r) -->
	emit_goto_ins(u2_void_ylval,r).
emit_ins_2(u2_void_yval,w) -->
	"U1_VOID_W(SP1);\n",
	"U1_YVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_void_ylval, 143, "iy"). % Descr i_y
emit_ins_2(u2_void_ylval,r) -->
	"U1_VOID_R(P1);\n",
	"U1_YVAL_R3(P2);\n".
emit_ins_2(u2_void_ylval,w) -->
	"U1_VOID_W(SP1);\n",
	"U1_YLVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_xvar_void, 144, "xi"). % Descr x_i
emit_ins_2(u2_xvar_void,r) -->
	"U1_XVAR_R(P1);\n",
	"U1_VOID_R(P2);\n",
	emit_dispatch_r("2").
emit_ins_2(u2_xvar_void,w) -->
	"U1_XVAR_W(P1);\n",
	"U1_VOID_W(SP2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_xvar_xvar, 145, "xx"). % Descr x_x
emit_ins_2(u2_xvar_xvar,r) -->
	"U1_XVAR_R(P1);\n",
	"U1_XVAR_R(P2);\n",
	emit_dispatch_r("2").
emit_ins_2(u2_xvar_xvar,w) -->
	"U1_XVAR_W(P1);\n",
	"U1_XVAR_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_xvar_yfvar, 148, "xy"). % Descr x_y
emit_ins_2(u2_xvar_yfvar,r) -->
	"ComputeE;\n",
	emit_goto_ins(u2_xvar_yvar,r).
emit_ins_2(u2_xvar_yfvar,w) -->
	"ComputeE;\n",
	emit_goto_ins(u2_xvar_yvar,w).

ins_op_format(u2_xvar_yvar, 149, "xy"). % Descr x_y
emit_ins_2(u2_xvar_yvar,r) -->
	"U1_XVAR_R(P1);\n",
	"U1_YVAR_R(P2);\n",
	emit_dispatch_r("2").
emit_ins_2(u2_xvar_yvar,w) -->
	"U1_XVAR_W(P1);\n",
	"U1_YVAR_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_xvar_xval, 146, "xx"). % Descr x_x
emit_ins_2(u2_xvar_xval,r) -->
	emit_goto_ins(u2_xvar_xlval,r).
emit_ins_2(u2_xvar_xval,w) -->
	"U1_XVAR_W(P1);\n",
	"U1_XVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_xvar_xlval, 147, "xx"). % Descr x_x
emit_ins_2(u2_xvar_xlval,r) -->
	"U1_XVAR_R(P1);\n",
	"U1_XVAL_R3(P2);\n".
emit_ins_2(u2_xvar_xlval,w) -->
	"U1_XVAR_W(P1);\n",
	"U1_XLVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_xvar_yfval, 150, "xy"). % Descr x_y
emit_ins_2(u2_xvar_yfval,r) -->
	"U1_XVAR_R(P1);\n",
	"U1_YFVAL_R(P2);\n",
	emit_dispatch_r("2").
emit_ins_2(u2_xvar_yfval,w) -->
	"U1_XVAR_W(P1);\n",
	"U1_YFVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_xvar_yval, 151, "xy"). % Descr x_y
emit_ins_2(u2_xvar_yval,r) -->
	emit_goto_ins(u2_xvar_ylval,r).
emit_ins_2(u2_xvar_yval,w) -->
	"U1_XVAR_W(P1);\n",
	"U1_YVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_xvar_ylval, 152, "xy"). % Descr x_y
emit_ins_2(u2_xvar_ylval,r) -->
	"U1_XVAR_R(P1);\n",
	"U1_YVAL_R3(P2);\n".
emit_ins_2(u2_xvar_ylval,w) -->
	"U1_XVAR_W(P1);\n",
	"U1_YLVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_yfvar_void, 153, "yi"). % Descr y_i
emit_ins_2(u2_yfvar_void,r) -->
	"ComputeE;\n",
	emit_goto_ins(u2_yvar_void,r).
emit_ins_2(u2_yfvar_void,w) -->
	"ComputeE;\n",
	emit_goto_ins(u2_yvar_void,w).

ins_op_format(u2_yvar_void, 154, "yi"). % Descr y_i
emit_ins_2(u2_yvar_void,r) -->
	"U1_YVAR_R(P1);\n",
	"U1_VOID_R(P2);\n",
	emit_dispatch_r("2").
emit_ins_2(u2_yvar_void,w) -->
	"U1_YVAR_W(P1);\n",
	"U1_VOID_W(SP2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_yfvar_xvar, 155, "yx"). % Descr y_x
emit_ins_2(u2_yfvar_xvar,r) -->
	"ComputeE;\n",
	emit_goto_ins(u2_yvar_xvar,r).
emit_ins_2(u2_yfvar_xvar,w) -->
	"ComputeE;\n",
	emit_goto_ins(u2_yvar_xvar,w).

ins_op_format(u2_yvar_xvar, 156, "yx"). % Descr y_x
emit_ins_2(u2_yvar_xvar,r) -->
	"U1_YVAR_R(P1);\n",
	"U1_XVAR_R(P2);\n",
	emit_dispatch_r("2").
emit_ins_2(u2_yvar_xvar,w) -->
	"U1_YVAR_W(P1);\n",
	"U1_XVAR_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_yfvar_yvar, 157, "yy"). % Descr y_y
emit_ins_2(u2_yfvar_yvar,r) -->
	"ComputeE;\n",
	emit_goto_ins(u2_yvar_yvar,r).
emit_ins_2(u2_yfvar_yvar,w) -->
	"ComputeE;\n",
	emit_goto_ins(u2_yvar_yvar,w).

ins_op_format(u2_yvar_yvar, 158, "yy"). % Descr y_y
emit_ins_2(u2_yvar_yvar,r) -->
	"U1_YVAR_R(P1);\n",
	"U1_YVAR_R(P2);\n",
	emit_dispatch_r("2").
emit_ins_2(u2_yvar_yvar,w) -->
	"U1_YVAR_W(P1);\n",
	"U1_YVAR_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_yfvar_xval, 159, "yx"). % Descr y_x
emit_ins_2(u2_yfvar_xval,r) -->
	emit_goto_ins(u2_yfvar_xlval,r).
emit_ins_2(u2_yfvar_xval,w) -->
	"ComputeE;\n",
	emit_goto_ins(u2_yvar_xval,w).

ins_op_format(u2_yfvar_xlval, 161, "yx"). % Descr y_x
emit_ins_2(u2_yfvar_xlval,r) -->
	"ComputeE;\n",
	emit_goto_ins(u2_yvar_xval,r).
emit_ins_2(u2_yfvar_xlval,w) -->
	"ComputeE;\n",
	emit_goto_ins(u2_yvar_xlval,w).

ins_op_format(u2_yvar_xval, 160, "yx"). % Descr y_x
emit_ins_2(u2_yvar_xval,r) -->
	emit_goto_ins(u2_yvar_xlval,r).
emit_ins_2(u2_yvar_xval,w) -->
	"U1_YVAR_W(P1);\n",
	"U1_XVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_yvar_xlval, 162, "yx"). % Descr y_x
emit_ins_2(u2_yvar_xlval,r) -->
	"U1_YVAR_R(P1);\n",
	"U1_XVAL_R3(P2);\n".
emit_ins_2(u2_yvar_xlval,w) -->
	"U1_YVAR_W(P1);\n",
	"U1_XLVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_yfvar_yval, 163, "yy"). % Descr y_y
emit_ins_2(u2_yfvar_yval,r) -->
	emit_goto_ins(u2_yfvar_ylval,r).
emit_ins_2(u2_yfvar_yval,w) -->
	"ComputeE;\n",
	emit_goto_ins(u2_yvar_yval,w).

ins_op_format(u2_yfvar_ylval, 165, "yy"). % Descr y_y
emit_ins_2(u2_yfvar_ylval,r) -->
	"ComputeE;\n",
	emit_goto_ins(u2_yvar_yval,r).
emit_ins_2(u2_yfvar_ylval,w) -->
	"ComputeE;\n",
	emit_goto_ins(u2_yvar_ylval,w).

ins_op_format(u2_yvar_yval, 164, "yy"). % Descr y_y
emit_ins_2(u2_yvar_yval,r) -->
	emit_goto_ins(u2_yvar_ylval,r).
emit_ins_2(u2_yvar_yval,w) -->
	"U1_YVAR_W(P1);\n",
	"U1_YVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_yvar_ylval, 166, "yy"). % Descr y_y
emit_ins_2(u2_yvar_ylval,r) -->
	"U1_YVAR_R(P1);\n",
	"U1_YVAL_R3(P2);\n".
emit_ins_2(u2_yvar_ylval,w) -->
	"U1_YVAR_W(P1);\n",
	"U1_YLVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_yfval_void, 185, "yi"). % Descr y_i
emit_ins_2(u2_yfval_void,r) -->
	"U1_YFVAL_R(P1);\n",
	"U1_VOID_R(P2);\n",
	emit_dispatch_r("2").
emit_ins_2(u2_yfval_void,w) -->
	"U1_YFVAL_W(P1);\n",
	"U1_VOID_W(SP2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_yfval_xvar, 188, "yx"). % Descr y_x
emit_ins_2(u2_yfval_xvar,r) -->
	"U1_YFVAL_R(P1);\n",
	"U1_XVAR_R(P2);\n",
	emit_dispatch_r("2").
emit_ins_2(u2_yfval_xvar,w) -->
	"U1_YFVAL_W(P1);\n",
	"U1_XVAR_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_yfval_yfval, 199, "yy"). % Descr y_y
emit_ins_2(u2_yfval_yfval,r) -->
	"U1_YFVAL_R(P1);\n",
	"U1_YFVAL_R(P2);\n",
	emit_dispatch_r("2").
emit_ins_2(u2_yfval_yfval,w) -->
	"U1_YFVAL_W(P1);\n",
	"U1_YFVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_yfval_xval, 193, "yx"). % Descr y_x
emit_ins_2(u2_yfval_xval,r) -->
	emit_goto_ins(u2_yfval_xlval,r).
emit_ins_2(u2_yfval_xval,w) -->
	"U1_YFVAL_W(P1);\n",
	"U1_XVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_yfval_xlval, 196, "yx"). % Descr y_x
emit_ins_2(u2_yfval_xlval,r) -->
	"U1_YFVAL_R(P1);\n",
	"U1_XVAL_R3(P2);\n".
emit_ins_2(u2_yfval_xlval,w) -->
	"U1_YFVAL_W(P1);\n",
	"U1_XLVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_yfval_yval, 202, "yy"). % Descr y_y
emit_ins_2(u2_yfval_yval,r) -->
	emit_goto_ins(u2_yfval_ylval,r).
emit_ins_2(u2_yfval_yval,w) -->
	"U1_YFVAL_W(P1);\n",
	"U1_YVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_yfval_ylval, 205, "yy"). % Descr y_y
emit_ins_2(u2_yfval_ylval,r) -->
	"U1_YFVAL_R(P1);\n",
	"U1_YVAL_R3(P2);\n".
emit_ins_2(u2_yfval_ylval,w) -->
	"U1_YFVAL_W(P1);\n",
	"U1_YLVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_xval_void, 167, "xi"). % Descr x_i
emit_ins_2(u2_xval_void,r) -->
	emit_goto_ins(u2_xlval_void,r).
emit_ins_2(u2_xval_void,w) -->
	"U1_XVAL_W(P1);\n",
	"U1_VOID_W(SP2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_xlval_void, 168, "xi"). % Descr x_i
emit_ins_2(u2_xlval_void,r) -->
	"RefHeapNext(t1,S);\n",
	"U1_VOID_R(P2);\n",
	"EUNIFY(Xb(P1),t1,2);\n".
emit_ins_2(u2_xlval_void,w) -->
	"U1_XLVAL_W(P1);\n",
	"U1_VOID_W(SP2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_xval_xvar, 169, "xx"). % Descr x_x
emit_ins_2(u2_xval_xvar,r) -->
	emit_goto_ins(u2_xlval_xvar,r).
emit_ins_2(u2_xval_xvar,w) -->
	"U1_XVAL_W(P1);\n",
	"U1_XVAR_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_xlval_xvar, 170, "xx"). % Descr x_x
emit_ins_2(u2_xlval_xvar,r) -->
	"t0 = Xb(P1);\n",
	"RefHeapNext(t1,S);\n",
	"U1_XVAR_R(P2);\n",
	"EUNIFY(t0,t1,2);\n".
emit_ins_2(u2_xlval_xvar,w) -->
	"U1_XLVAL_W(P1);\n",
	"U1_XVAR_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_xval_yfvar, 171, "xy"). % Descr x_y
emit_ins_2(u2_xval_yfvar,r) -->
	emit_goto_ins(u2_xlval_yfvar,r).
emit_ins_2(u2_xval_yfvar,w) -->
	"ComputeE;\n",
	emit_goto_ins(u2_xval_yvar,w).

ins_op_format(u2_xlval_yfvar, 172, "xy"). % Descr x_y
emit_ins_2(u2_xlval_yfvar,r) -->
	"ComputeE;\n",
	emit_goto_ins(u2_xval_yvar,r).
emit_ins_2(u2_xlval_yfvar,w) -->
	"ComputeE;\n",
	emit_goto_ins(u2_xlval_yvar,w).

ins_op_format(u2_xval_yvar, 173, "xy"). % Descr x_y
emit_ins_2(u2_xval_yvar,r) -->
	emit_goto_ins(u2_xlval_yvar,r).
emit_ins_2(u2_xval_yvar,w) -->
	"U1_XVAL_W(P1);\n",
	"U1_YVAR_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_xlval_yvar, 174, "xy"). % Descr x_y
emit_ins_2(u2_xlval_yvar,r) -->
	"U1_XVAL_R(P1);\n",
	"U1_YVAR_R(P2);\n",
	emit_dispatch_r("2").
emit_ins_2(u2_xlval_yvar,w) -->
	"U1_XLVAL_W(P1);\n",
	"U1_YVAR_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_xval_xval, 175, "xx"). % Descr x_x
emit_ins_2(u2_xval_xval,r) -->
	emit_goto_ins(u2_xval_xlval,r).
emit_ins_2(u2_xval_xval,w) -->
	"U1_XVAL_W(P1);\n",
	"U1_XVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_xval_xlval, 177, "xx"). % Descr x_x
emit_ins_2(u2_xval_xlval,r) -->
	emit_goto_ins(u2_xlval_xval,r).
emit_ins_2(u2_xval_xlval,w) -->
	"U1_XVAL_W(P1);\n",
	"U1_XLVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_xlval_xval, 176, "xx"). % Descr x_x
emit_ins_2(u2_xlval_xval,r) -->
	emit_goto_ins(u2_xlval_xlval,r).
emit_ins_2(u2_xlval_xval,w) -->
	"U1_XLVAL_W(P1);\n",
	"U1_XVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_xlval_xlval, 178, "xx"). % Descr x_x
emit_ins_2(u2_xlval_xlval,r) -->
	"U1_XVAL_R(P1);\n",
	"U1_XVAL_R3(P2);\n".
emit_ins_2(u2_xlval_xlval,w) -->
	"U1_XLVAL_W(P1);\n",
	"U1_XLVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_xval_yfval, 179, "xy"). % Descr x_y
emit_ins_2(u2_xval_yfval,r) -->
	emit_goto_ins(u2_xlval_yfval,r).
emit_ins_2(u2_xval_yfval,w) -->
	"U1_XVAL_W(P1);\n",
	"U1_YFVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_xlval_yfval, 180, "xy"). % Descr x_y
emit_ins_2(u2_xlval_yfval,r) -->
	"U1_XVAL_R(P1);\n",
	"U1_YFVAL_R(P2);\n",
	emit_dispatch_r("2").
emit_ins_2(u2_xlval_yfval,w) -->
	"U1_XLVAL_W(P1);\n",
	"U1_YFVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_xval_yval, 181, "xy"). % Descr x_y
emit_ins_2(u2_xval_yval,r) -->
	emit_goto_ins(u2_xval_ylval,r).
emit_ins_2(u2_xval_yval,w) -->
	"U1_XVAL_W(P1);\n",
	"U1_YVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_xval_ylval, 183, "xy"). % Descr x_y
emit_ins_2(u2_xval_ylval,r) -->
	emit_goto_ins(u2_xlval_yval,r).
emit_ins_2(u2_xval_ylval,w) -->
	"U1_XVAL_W(P1);\n",
	"U1_YLVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_xlval_yval, 182, "xy"). % Descr x_y
emit_ins_2(u2_xlval_yval,r) -->
	emit_goto_ins(u2_xlval_ylval,r).
emit_ins_2(u2_xlval_yval,w) -->
	"U1_XLVAL_W(P1);\n",
	"U1_YVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_xlval_ylval, 184, "xy"). % Descr x_y
emit_ins_2(u2_xlval_ylval,r) -->
	"U1_XVAL_R(P1);\n",
	"U1_YVAL_R3(P2);\n".
emit_ins_2(u2_xlval_ylval,w) -->
	"U1_XLVAL_W(P1);\n",
	"U1_YLVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_yval_void, 186, "yi"). % Descr y_i
emit_ins_2(u2_yval_void,r) -->
	emit_goto_ins(u2_ylval_void,r).
emit_ins_2(u2_yval_void,w) -->
	"U1_YVAL_W(P1);\n",
	"U1_VOID_W(SP2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_ylval_void, 187, "yi"). % Descr y_i
emit_ins_2(u2_ylval_void,r) -->
	"RefHeapNext(t1,S);\n",
	"U1_VOID_R(P2);\n",
	"EUNIFY(Yb(P1),t1,2);\n".
emit_ins_2(u2_ylval_void,w) -->
	"U1_YLVAL_W(P1);\n",
	"U1_VOID_W(SP2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_yval_xvar, 189, "yx"). % Descr y_x
emit_ins_2(u2_yval_xvar,r) -->
	emit_goto_ins(u2_ylval_xvar,r).
emit_ins_2(u2_yval_xvar,w) -->
	"U1_YVAL_W(P1);\n",
	"U1_XVAR_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_ylval_xvar, 190, "yx"). % Descr y_x
emit_ins_2(u2_ylval_xvar,r) -->
	"RefHeapNext(t1,S);\n",
	"U1_XVAR_R(P2);\n",
	"EUNIFY(Yb(P1),t1,2);\n".
emit_ins_2(u2_ylval_xvar,w) -->
	"U1_YLVAL_W(P1);\n",
	"U1_XVAR_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_yval_yvar, 191, "yy"). % Descr y_y
emit_ins_2(u2_yval_yvar,r) -->
	emit_goto_ins(u2_ylval_yvar,r).
emit_ins_2(u2_yval_yvar,w) -->
	"U1_YVAL_W(P1);\n",
	"U1_YVAR_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_ylval_yvar, 192, "yy"). % Descr y_y
emit_ins_2(u2_ylval_yvar,r) -->
	"U1_YVAL_R(P1);\n",
	"U1_YVAR_R(P2);\n",
	emit_dispatch_r("2").
emit_ins_2(u2_ylval_yvar,w) -->
	"U1_YLVAL_W(P1);\n",
	"U1_YVAR_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_yval_yfval, 200, "yy"). % Descr y_y
emit_ins_2(u2_yval_yfval,r) -->
	emit_goto_ins(u2_ylval_yfval,r).
emit_ins_2(u2_yval_yfval,w) -->
	"U1_YVAL_W(P1);\n",
	"U1_YFVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_ylval_yfval, 201, "yy"). % Descr y_y
emit_ins_2(u2_ylval_yfval,r) -->
	"U1_YVAL_R(P1);\n",
	"U1_YFVAL_R(P2);\n",
	emit_dispatch_r("2").
emit_ins_2(u2_ylval_yfval,w) -->
	"U1_YLVAL_W(P1);\n",
	"U1_YFVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_yval_xval, 194, "yx"). % Descr y_x
emit_ins_2(u2_yval_xval,r) -->
	emit_goto_ins(u2_yval_xlval,r).
emit_ins_2(u2_yval_xval,w) -->
	"U1_YVAL_W(P1);\n",
	"U1_XVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_yval_xlval, 197, "yx"). % Descr y_x
emit_ins_2(u2_yval_xlval,r) -->
	emit_goto_ins(u2_ylval_xval,r).
emit_ins_2(u2_yval_xlval,w) -->
	"U1_YVAL_W(P1);\n",
	"U1_XLVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_ylval_xval, 195, "yx"). % Descr y_x
emit_ins_2(u2_ylval_xval,r) -->
	emit_goto_ins(u2_ylval_xlval,r).
emit_ins_2(u2_ylval_xval,w) -->
	"U1_YLVAL_W(P1);\n",
	"U1_XVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_ylval_xlval, 198, "yx"). % Descr y_x
emit_ins_2(u2_ylval_xlval,r) -->
	"U1_YVAL_R(P1);\n",
	"U1_XVAL_R3(P2);\n".
emit_ins_2(u2_ylval_xlval,w) -->
	"U1_YLVAL_W(P1);\n",
	"U1_XLVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_yval_yval, 203, "yy"). % Descr y_y
emit_ins_2(u2_yval_yval,r) -->
	emit_goto_ins(u2_yval_ylval,r).
emit_ins_2(u2_yval_yval,w) -->
	"U1_YVAL_W(P1);\n",
	"U1_YVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_yval_ylval, 206, "yy"). % Descr y_y
emit_ins_2(u2_yval_ylval,r) -->
	emit_goto_ins(u2_ylval_yval,r).
emit_ins_2(u2_yval_ylval,w) -->
	"U1_YVAL_W(P1);\n",
	"U1_YLVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_ylval_yval, 204, "yy"). % Descr y_y
emit_ins_2(u2_ylval_yval,r) -->
	emit_goto_ins(u2_ylval_ylval,r).
emit_ins_2(u2_ylval_yval,w) -->
	"U1_YLVAL_W(P1);\n",
	"U1_YVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(u2_ylval_ylval, 207, "yy"). % Descr y_y
emit_ins_2(u2_ylval_ylval,r) -->
	"U1_YVAL_R(P1);\n",
	"U1_YVAL_R3(P2);\n".
emit_ins_2(u2_ylval_ylval,w) -->
	"U1_YLVAL_W(P1);\n",
	"U1_YLVAL_W(P2);\n",
	emit_dispatch_w("2").

ins_op_format(bump_counterq, 248, "Ql").
emit_ins_2(bump_counterq,r) -->
	emit_shift("Q"),
	emit_goto_ins(bump_counter,r).
emit_ins_2(bump_counterq,w) -->
	emit_shift("Q"),
	emit_goto_ins(bump_counter,w).

ins_op_format(bump_counter, 249, "l"). % Descr counter
emit_ins_2(bump_counter,r) -->
	"#if GAUGE\n",
	"  INCR_COUNTER(*(ENG_INT **)P);\n",
	"#endif\n",
	emit_dispatch_r("BPL").
emit_ins_2(bump_counter,w) -->
	"#if GAUGE\n",
	"  INCR_COUNTER(*(ENG_INT **)P);\n",
	"#endif\n",
	emit_dispatch_w("BPL").

ins_op_format(counted_neckq, 250, "Qll").
emit_ins_2(counted_neckq,r) -->
	emit_shift("Q"),
	emit_goto_ins(counted_neck,r).
emit_ins_2(counted_neckq,w) -->
	emit_shift("Q"),
	emit_goto_ins(counted_neck,w).

ins_op_format(counted_neck, 251, "ll"). % Descr counter_counter
emit_ins_2(counted_neck,r) -->
	"CODE_COUNTED_NECK;\n",
	"P += 4;\n",
	emit_goto_ins(neck,r).
emit_ins_2(counted_neck,w) -->
	"CODE_COUNTED_NECK;\n",
	"P += 4;\n",
	emit_goto_ins(neck,w).

ins_op_format(fail, 67, ""). % Descr none
emit_ins_2(fail,r) -->
	"goto fail;\n".
emit_ins_2(fail,w) -->
	"goto fail;\n".

ins_op_format(heapmargin_callq, 245, "Qli").
emit_ins_2(heapmargin_callq,r) -->
	emit_shift("Q"),
	emit_goto_ins(heapmargin_call,r).
emit_ins_2(heapmargin_callq,w) -->
	emit_shift("Q"),
	emit_goto_ins(heapmargin_call,w).

ins_op_format(heapmargin_call, 246, "li"). % Descr l_i
emit_ins_2(heapmargin_call,r) -->
	"if (HeapDifference(w->global_top,Heap_End) < LP1) {\n",
        "  explicit_heap_overflow(Arg,LP1, SP3);\n",
        "  t0 = X(0);		/* if followed by get_*_x0 */\n",
	"}\n",
	emit_dispatch_r("1+BPL").
emit_ins_2(heapmargin_call,w) -->
	"if (HeapDifference(H,Heap_End) < LP1) {\n",
	"  StoreH;\n",
	"  explicit_heap_overflow(Arg,LP1, SP3);\n",
	"  LoadH;\n",
	"  t0 = X(0);		/* if followed by get_*_x0 */\n",
	"}\n",
	emit_dispatch_w("1+BPL").

ins_op_format(neck, 65, ""). % Descr none
emit_ins_2(neck,r) -->
	"CODE_NECK;\n",
	emit_dispatch_r("0").
emit_ins_2(neck,w) -->
	"CODE_NECK;\n",
	emit_dispatch_w("0").

ins_op_format(dynamic_neck_proceed, 236, "").
emit_ins_2(dynamic_neck_proceed,r) -->
	"LoadH;\n",
	emit_goto_ins(dynamic_neck_proceed,w).
emit_ins_2(dynamic_neck_proceed,w) -->
	"Unify_atom_internal(PointerToTerm(ins),X(3));\n",
	"if (!w->next_alt)\n",
	"  goto proceed_w;\n",
	"SetB(w->node);\n",
	"if (!B->next_alt && (def_clock = use_clock+1)==0xffff) {\n",
	"  StoreH;\n",
	"  clock_overflow(Arg);\n",
	"  LoadH;\n",
	"}\n",
	emit_goto_ins(neck_proceed,w).

ins_op_format(neck_proceed, 66, ""). % Descr none
emit_ins_2(neck_proceed,r) -->
	"LoadH;\n",
	emit_goto_ins(neck_proceed,w).
emit_ins_2(neck_proceed,w) -->
	"CODE_NECK_PROCEED;\n".

ins_op_format(proceed, 64, ""). % Descr none
emit_ins_2(proceed,r) -->
	"proceed_r:\n",
	"CODE_PROCEED;\n",
	emit_dispatch_r("0").
emit_ins_2(proceed,w) -->
	"proceed_w:\n",
	"CODE_PROCEED;\n",
	emit_dispatch_w("0").

% The order is not the same for R and W!!!
emit_file(r) -->
	"/* DO NOT EDIT!!! This file is autogenerated */\n",
	{ Mode = r },
	emit_ins_no_label(inittrue,Mode),
	emit_ins_no_label(firsttrue_n,Mode),
	emit_ins_no_label(initcallq,Mode),
	emit_ins(initcall,Mode),
	emit_ins_no_label(firstcall_nq,Mode),
	emit_ins(firstcall_n,Mode),
	emit_ins_no_label(firstcall_8q,Mode),
	emit_ins(firstcall_8,Mode),
	emit_ins_no_label(firstcall_7q,Mode),
	emit_ins(firstcall_7,Mode),
	emit_ins_no_label(firstcall_6q,Mode),
	emit_ins(firstcall_6,Mode),
	emit_ins_no_label(firstcall_5q,Mode),
	emit_ins(firstcall_5,Mode),
	emit_ins_no_label(firstcall_4q,Mode),
	emit_ins(firstcall_4,Mode),
	emit_ins_no_label(firstcall_3q,Mode),
	emit_ins(firstcall_3,Mode),
	emit_ins_no_label(firstcall_2q,Mode),
	emit_ins(firstcall_2,Mode),
	emit_ins_no_label(firstcall_1q,Mode),
	emit_ins(firstcall_1,Mode),
	emit_ins_no_label(firstcallq,Mode),
	emit_ins(firstcall,Mode),
	emit_ins_no_label(call_nq,Mode),
	emit_ins(call_n,Mode),
	emit_ins_no_label(call_8q,Mode),
	emit_ins(call_8,Mode),
	emit_ins_no_label(call_7q,Mode),
	emit_ins(call_7,Mode),
	emit_ins_no_label(call_6q,Mode),
	emit_ins(call_6,Mode),
	emit_ins_no_label(call_5q,Mode),
	emit_ins(call_5,Mode),
	emit_ins_no_label(call_4q,Mode),
	emit_ins(call_4,Mode),
	emit_ins_no_label(call_3q,Mode),
	emit_ins(call_3,Mode),
	emit_ins_no_label(call_2q,Mode),
	emit_ins(call_2,Mode),
	emit_ins_no_label(call_1q,Mode),
	emit_ins(call_1,Mode),
	emit_ins_no_label(callq,Mode),
	emit_ins(call,Mode),
	emit_ins_no_label(lastcall_nq,Mode),
	emit_ins(lastcall_n,Mode),
	emit_ins_no_label(lastcall_8q,Mode),
	emit_ins(lastcall_8,Mode),
	emit_ins_no_label(lastcall_7q,Mode),
	emit_ins(lastcall_7,Mode),
	emit_ins_no_label(lastcall_6q,Mode),
	emit_ins(lastcall_6,Mode),
	emit_ins_no_label(lastcall_5q,Mode),
	emit_ins(lastcall_5,Mode),
	emit_ins_no_label(lastcall_4q,Mode),
	emit_ins(lastcall_4,Mode),
	emit_ins_no_label(lastcall_3q,Mode),
	emit_ins(lastcall_3,Mode),
	emit_ins_no_label(lastcall_2q,Mode),
	emit_ins(lastcall_2,Mode),
	emit_ins_no_label(lastcall_1q,Mode),
	emit_ins(lastcall_1,Mode),
	emit_ins_no_label(lastcallq,Mode),
	emit_ins(lastcall,Mode),
	emit_ins_no_label(executeq,Mode),
	emit_ins_no_label(execute,Mode),
	emit_ins_no_label(put_x_void,Mode),
	emit_ins_no_label(put_x_variable,Mode),
	emit_ins_no_label(put_xval_xval,Mode),
	emit_ins_no_label(put_x_value,Mode),
	emit_ins_no_label(put_x_unsafe_value,Mode),
	emit_ins_no_label(put_y_first_variable,Mode),
	emit_ins_no_label(put_y_variable,Mode),
	emit_ins_no_label(put_yfvar_yvar,Mode),
	emit_ins_no_label(put_yvar_yvar,Mode),
	emit_ins_no_label(put_yval_yval,Mode),
	emit_ins_no_label(put_y_value,Mode),
	emit_ins_no_label(put_y_unsafe_value,Mode),
	emit_ins_no_label(put_constantq,Mode),
	emit_ins_no_label(put_constant,Mode),
	emit_ins_no_label(put_nil,Mode),
	emit_ins_no_label(put_largeq,Mode),
	emit_ins_no_label(put_large,Mode),
	emit_ins_no_label(put_structureq,Mode),
	emit_ins_no_label(put_structure,Mode),
	emit_ins_no_label(put_list,Mode),
	emit_ins_no_label(put_yval_yuval,Mode),
	emit_ins_no_label(put_yuval_yval,Mode),
	emit_ins_no_label(put_yuval_yuval,Mode),
	emit_ins(get_x_value,Mode),
	emit_ins(get_y_first_value,Mode),
	emit_ins(get_y_value,Mode),
	emit_ins_no_label(get_constantq,Mode),
	emit_ins(get_constant,Mode),
	emit_ins_no_label(get_largeq,Mode),
	emit_ins(get_large,Mode),
	emit_ins_no_label(get_structureq,Mode),
	emit_ins(get_structure,Mode),
	emit_ins(get_nil,Mode),
	emit_ins(get_list,Mode),
	emit_ins_no_label(get_constant_neck_proceedq,Mode),
	emit_ins(get_constant_neck_proceed,Mode),
	emit_ins(get_nil_neck_proceed,Mode),
	emit_ins(cutb_x,Mode),
	emit_ins(cutb_x_neck,Mode),
	emit_ins(cutb_neck,Mode),
	emit_ins(cutb_x_neck_proceed,Mode),
	emit_ins(cutb_neck_proceed,Mode),
	emit_ins(cute_x,Mode),
	emit_ins(cute_x_neck,Mode),
	emit_ins(cute_neck,Mode),
	emit_ins(cutf_x,Mode),
	emit_ins(cutf,Mode),
	emit_ins(cut_y,Mode),
	emit_ins_no_label(choice_x,Mode),
	emit_ins_no_label(choice_yf,Mode),
	emit_ins(choice_y,Mode),
	emit_ins_no_label(kontinue,Mode),
	emit_ins(leave,Mode),
	emit_ins(exit_toplevel,Mode),
	emit_ins(retry_cq,Mode),
	emit_ins(retry_c,Mode),
	emit_ins_no_label(get_structure_x0q,Mode),
	emit_ins_no_label(get_structure_x0,Mode),
	emit_ins_no_label(get_large_x0q,Mode),
	emit_ins_no_label(get_large_x0,Mode),
	emit_ins_no_label(get_constant_x0q,Mode),
	emit_ins_no_label(get_constant_x0,Mode),
	emit_ins_no_label(get_nil_x0,Mode),
	emit_ins_no_label(get_list_x0,Mode),
	emit_ins_no_label(get_xvar_xvar,Mode),
	emit_ins_no_label(get_x_variable,Mode),
	emit_ins_no_label(get_y_first_variable,Mode),
	emit_ins(get_y_variable,Mode),
	emit_ins_no_label(get_yfvar_yvar,Mode),
	emit_ins(get_yvar_yvar,Mode),
	emit_ins_no_label(branch,Mode),
	emit_ins(function_1q,Mode),
	emit_ins(function_1,Mode),
	emit_ins(function_2q,Mode),
	emit_ins(function_2,Mode),
	emit_ins(builtin_1q,Mode),
	emit_ins(builtin_1,Mode),
	emit_ins(builtin_2q,Mode),
	emit_ins(builtin_2,Mode),
	emit_ins(builtin_3q,Mode),
	emit_ins(builtin_3,Mode),
	emit_ins(retry_instance,Mode),
	emit_ins_no_label(get_constraint,Mode),
	emit_ins_no_label(unify_void,Mode),
	emit_ins_no_label(unify_void_1,Mode),
	emit_ins_no_label(unify_void_2,Mode),
	emit_ins_no_label(unify_void_3,Mode),
	emit_ins_no_label(unify_void_4,Mode),
	emit_ins_no_label(unify_x_variable,Mode),
	emit_ins_no_label(unify_x_value,Mode),
	emit_ins(unify_x_local_value,Mode),
	emit_ins_no_label(unify_y_first_variable,Mode),
	emit_ins(unify_y_variable,Mode),
	emit_ins_no_label(unify_y_first_value,Mode),
	emit_ins_no_label(unify_y_value,Mode),
	emit_ins(unify_y_local_value,Mode),
	emit_ins_no_label(unify_constantq,Mode),
	emit_ins(unify_constant,Mode),
	emit_ins_no_label(unify_largeq,Mode),
	emit_ins(unify_large,Mode),
	emit_ins_no_label(unify_structureq,Mode),
	emit_ins(unify_structure,Mode),
	emit_ins_no_label(unify_nil,Mode),
	emit_ins_no_label(unify_list,Mode),
	emit_ins_no_label(unify_constant_neck_proceedq,Mode),
	emit_ins(unify_constant_neck_proceed,Mode),
	emit_ins_no_label(unify_nil_neck_proceed,Mode),
	emit_ins_no_label(u2_void_xvar,Mode),
	emit_ins_no_label(u2_void_yfvar,Mode),
	emit_ins(u2_void_yvar,Mode),
	emit_ins_no_label(u2_void_xval,Mode),
	emit_ins(u2_void_xlval,Mode),
	emit_ins_no_label(u2_void_yfval,Mode),
	emit_ins_no_label(u2_void_yval,Mode),
	emit_ins(u2_void_ylval,Mode),
	emit_ins_no_label(u2_xvar_void,Mode),
	emit_ins_no_label(u2_xvar_xvar,Mode),
	emit_ins_no_label(u2_xvar_yfvar,Mode),
	emit_ins(u2_xvar_yvar,Mode),
	emit_ins_no_label(u2_xvar_xval,Mode),
	emit_ins(u2_xvar_xlval,Mode),
	emit_ins_no_label(u2_xvar_yfval,Mode),
	emit_ins_no_label(u2_xvar_yval,Mode),
	emit_ins(u2_xvar_ylval,Mode),
	emit_ins_no_label(u2_yfvar_void,Mode),
	emit_ins(u2_yvar_void,Mode),
	emit_ins_no_label(u2_yfvar_xvar,Mode),
	emit_ins(u2_yvar_xvar,Mode),
	emit_ins_no_label(u2_yfvar_yvar,Mode),
	emit_ins(u2_yvar_yvar,Mode),
	emit_ins_no_label(u2_yfvar_xval,Mode),
	emit_ins(u2_yfvar_xlval,Mode),
	emit_ins(u2_yvar_xval,Mode),
	emit_ins(u2_yvar_xlval,Mode),
	emit_ins_no_label(u2_yfvar_yval,Mode),
	emit_ins(u2_yfvar_ylval,Mode),
	emit_ins(u2_yvar_yval,Mode),
	emit_ins(u2_yvar_ylval,Mode),
	emit_ins_no_label(u2_yfval_void,Mode),
	emit_ins_no_label(u2_yfval_xvar,Mode),
	emit_ins_no_label(u2_yfval_yfval,Mode),
	emit_ins_no_label(u2_yfval_xval,Mode),
	emit_ins(u2_yfval_xlval,Mode),
	emit_ins_no_label(u2_yfval_yval,Mode),
	emit_ins(u2_yfval_ylval,Mode),
	emit_ins_no_label(u2_xval_void,Mode),
	emit_ins(u2_xlval_void,Mode),
	emit_ins_no_label(u2_xval_xvar,Mode),
	emit_ins(u2_xlval_xvar,Mode),
	emit_ins_no_label(u2_xval_yfvar,Mode),
	emit_ins(u2_xlval_yfvar,Mode),
	emit_ins(u2_xval_yvar,Mode),
	emit_ins(u2_xlval_yvar,Mode),
	emit_ins_no_label(u2_xval_xval,Mode),
	emit_ins(u2_xval_xlval,Mode),
	emit_ins(u2_xlval_xval,Mode),
	emit_ins(u2_xlval_xlval,Mode),
	emit_ins_no_label(u2_xval_yfval,Mode),
	emit_ins(u2_xlval_yfval,Mode),
	emit_ins_no_label(u2_xval_yval,Mode),
	emit_ins(u2_xval_ylval,Mode),
	emit_ins(u2_xlval_yval,Mode),
	emit_ins(u2_xlval_ylval,Mode),
	emit_ins_no_label(u2_yval_void,Mode),
	emit_ins(u2_ylval_void,Mode),
	emit_ins_no_label(u2_yval_xvar,Mode),
	emit_ins(u2_ylval_xvar,Mode),
	emit_ins_no_label(u2_yval_yvar,Mode),
	emit_ins(u2_ylval_yvar,Mode),
	emit_ins_no_label(u2_yval_yfval,Mode),
	emit_ins(u2_ylval_yfval,Mode),
	emit_ins_no_label(u2_yval_xval,Mode),
	emit_ins(u2_yval_xlval,Mode),
	emit_ins(u2_ylval_xval,Mode),
	emit_ins(u2_ylval_xlval,Mode),
	emit_ins_no_label(u2_yval_yval,Mode),
	emit_ins(u2_yval_ylval,Mode),
	emit_ins(u2_ylval_yval,Mode),
	emit_ins(u2_ylval_ylval,Mode),
	emit_ins_no_label(bump_counterq,Mode),
	emit_ins(bump_counter,Mode),
	emit_ins_no_label(counted_neckq,Mode),
	emit_ins(counted_neck,Mode),
	emit_ins_no_label(fail,Mode),
	emit_ins_no_label(heapmargin_callq,Mode),
	emit_ins(heapmargin_call,Mode),
	emit_ins(neck,Mode),
	emit_ins_no_label(dynamic_neck_proceed,Mode),
	emit_ins_no_label(neck_proceed,Mode),
	emit_ins_no_label(proceed,Mode).

emit_file(w) -->
	"/* DO NOT EDIT!!! This file is autogenerated */\n",
	{ Mode = w },
	emit_ins(inittrue,Mode),
	emit_ins(firsttrue_n,Mode),
	emit_ins_no_label(initcallq,Mode),
	emit_ins(initcall,Mode),
	emit_ins_no_label(firstcall_nq,Mode),
	emit_ins_no_label(firstcall_8q,Mode),
	emit_ins_no_label(firstcall_7q,Mode),
	emit_ins_no_label(firstcall_6q,Mode),
	emit_ins_no_label(firstcall_5q,Mode),
	emit_ins_no_label(firstcall_4q,Mode),
	emit_ins_no_label(firstcall_3q,Mode),
	emit_ins_no_label(firstcall_2q,Mode),
	emit_ins_no_label(firstcall_1q,Mode),
	emit_ins_no_label(firstcallq,Mode),
	emit_ins_no_label(call_nq,Mode),
	emit_ins_no_label(call_8q,Mode),
	emit_ins_no_label(call_7q,Mode),
	emit_ins_no_label(call_6q,Mode),
	emit_ins_no_label(call_5q,Mode),
	emit_ins_no_label(call_4q,Mode),
	emit_ins_no_label(call_3q,Mode),
	emit_ins_no_label(call_2q,Mode),
	emit_ins_no_label(call_1q,Mode),
	emit_ins_no_label(callq,Mode),
	emit_ins_no_label(lastcall_nq,Mode),
	emit_ins_no_label(lastcall_8q,Mode),
	emit_ins_no_label(lastcall_7q,Mode),
	emit_ins_no_label(lastcall_6q,Mode),
	emit_ins_no_label(lastcall_5q,Mode),
	emit_ins_no_label(lastcall_4q,Mode),
	emit_ins_no_label(lastcall_3q,Mode),
	emit_ins_no_label(lastcall_2q,Mode),
	emit_ins_no_label(lastcall_1q,Mode),
	emit_ins_no_label(lastcallq,Mode),
	emit_ins(firstcall_n,Mode),
	emit_ins(firstcall_8,Mode),
	emit_ins(firstcall_7,Mode),
	emit_ins(firstcall_6,Mode),
	emit_ins(firstcall_5,Mode),
	emit_ins(firstcall_4,Mode),
	emit_ins(firstcall_3,Mode),
	emit_ins(firstcall_2,Mode),
	emit_ins(firstcall_1,Mode),
	emit_ins(firstcall,Mode),
	emit_ins(call_n,Mode),
	emit_ins(call_8,Mode),
	emit_ins(call_7,Mode),
	emit_ins(call_6,Mode),
	emit_ins(call_5,Mode),
	emit_ins(call_4,Mode),
	emit_ins(call_3,Mode),
	emit_ins(call_2,Mode),
	emit_ins(call_1,Mode),
	emit_ins(call,Mode),
	emit_ins(lastcall_n,Mode),
	emit_ins(lastcall_8,Mode),
	emit_ins(lastcall_7,Mode),
	emit_ins(lastcall_6,Mode),
	emit_ins(lastcall_5,Mode),
	emit_ins(lastcall_4,Mode),
	emit_ins(lastcall_3,Mode),
	emit_ins(lastcall_2,Mode),
	emit_ins(lastcall_1,Mode),
	emit_ins(lastcall,Mode),
	emit_ins(execute,Mode),
	emit_ins_no_label(executeq,Mode),
	emit_ins(put_x_void,Mode),
	emit_ins(put_x_variable,Mode),
	emit_ins_no_label(put_xval_xval,Mode),
	emit_ins_no_label(put_x_value,Mode),
	emit_ins(put_x_unsafe_value,Mode),
	emit_ins(put_y_first_variable,Mode),
	emit_ins(put_y_variable,Mode),
	emit_ins(put_yfvar_yvar,Mode),
	emit_ins(put_yvar_yvar,Mode),
	emit_ins_no_label(put_constantq,Mode),
	emit_ins_no_label(put_constant,Mode),
	emit_ins_no_label(put_nil,Mode),
	emit_ins(put_largeq,Mode),
	emit_ins(put_large,Mode),
	emit_ins(put_structureq,Mode),
	emit_ins(put_structure,Mode),
	emit_ins(put_list,Mode),
	emit_ins_no_label(put_y_value,Mode),
	emit_ins(put_y_unsafe_value,Mode),
	emit_ins_no_label(put_yval_yval,Mode),
	emit_ins(put_yval_yuval,Mode),
	emit_ins(put_yuval_yval,Mode),
	emit_ins(put_yuval_yuval,Mode),
	emit_ins_no_label(get_x_value,Mode),
	emit_ins_no_label(get_y_first_value,Mode),
	emit_ins_no_label(get_y_value,Mode),
	emit_ins_no_label(get_constantq,Mode),
	emit_ins(get_constant,Mode),
	emit_ins_no_label(get_largeq,Mode),
	emit_ins(get_large,Mode),
	emit_ins_no_label(get_structureq,Mode),
	emit_ins(get_structure,Mode),
	emit_ins_no_label(get_nil,Mode),
	emit_ins_no_label(get_list,Mode),
	emit_ins_no_label(get_constant_neck_proceedq,Mode),
	emit_ins(get_constant_neck_proceed,Mode),
	emit_ins_no_label(get_nil_neck_proceed,Mode),
	emit_ins_no_label(cutb_x,Mode),
	emit_ins_no_label(cutb_x_neck,Mode),
	emit_ins_no_label(cutb_neck,Mode),
	emit_ins_no_label(cutb_x_neck_proceed,Mode),
	emit_ins_no_label(cutb_neck_proceed,Mode),
	emit_ins_no_label(cute_x,Mode),
	emit_ins_no_label(cute_x_neck,Mode),
	emit_ins_no_label(cute_neck,Mode),
	emit_ins_no_label(cutf,Mode),
	emit_ins_no_label(cutf_x,Mode),
	emit_ins_no_label(cut_y,Mode),
	emit_ins_no_label(choice_x,Mode),
	emit_ins_no_label(choice_yf,Mode),
	emit_ins(choice_y,Mode),
	emit_ins(kontinue,Mode),
	emit_ins_no_label(leave,Mode),
	emit_ins_no_label(exit_toplevel,Mode),
	emit_ins_no_label(retry_cq,Mode),
	emit_ins_no_label(retry_c,Mode),
	emit_ins_no_label(get_structure_x0q,Mode),
	emit_ins(get_structure_x0,Mode),
	emit_ins_no_label(get_large_x0q,Mode),
	emit_ins(get_large_x0,Mode),
	emit_ins_no_label(get_constant_x0q,Mode),
	emit_ins(get_constant_x0,Mode),
	emit_ins_no_label(get_nil_x0,Mode),
	emit_ins_no_label(get_list_x0,Mode),
	emit_ins_no_label(get_xvar_xvar,Mode),
	emit_ins_no_label(get_x_variable,Mode),
	emit_ins_no_label(get_y_first_variable,Mode),
	emit_ins(get_y_variable,Mode),
	emit_ins_no_label(get_yfvar_yvar,Mode),
	emit_ins(get_yvar_yvar,Mode),
	emit_ins_no_label(branch,Mode),
	emit_ins_no_label(function_1q,Mode),
	emit_ins_no_label(function_1,Mode),
	emit_ins_no_label(function_2q,Mode),
	emit_ins_no_label(function_2,Mode),
	emit_ins_no_label(builtin_1q,Mode),
	emit_ins_no_label(builtin_1,Mode),
	emit_ins_no_label(builtin_2q,Mode),
	emit_ins_no_label(builtin_2,Mode),
	emit_ins_no_label(builtin_3q,Mode),
	emit_ins_no_label(builtin_3,Mode),
	emit_ins_no_label(retry_instance,Mode),
	emit_ins(get_constraint,Mode),
	emit_ins_no_label(unify_void,Mode),
	emit_ins(unify_void_4,Mode),
	emit_ins(unify_void_3,Mode),
	emit_ins(unify_void_2,Mode),
	emit_ins(unify_void_1,Mode),
	emit_ins_no_label(unify_x_variable,Mode),
	emit_ins_no_label(unify_x_value,Mode),
	emit_ins_no_label(unify_x_local_value,Mode),
	emit_ins_no_label(unify_y_first_variable,Mode),
	emit_ins(unify_y_variable,Mode),
	emit_ins_no_label(unify_y_first_value,Mode),
	emit_ins_no_label(unify_y_value,Mode),
	emit_ins_no_label(unify_y_local_value,Mode),
	emit_ins_no_label(unify_constantq,Mode),
	emit_ins_no_label(unify_constant,Mode),
	emit_ins_no_label(unify_largeq,Mode),
	emit_ins(unify_large,Mode),
	emit_ins_no_label(unify_structureq,Mode),
	emit_ins_no_label(unify_structure,Mode),
	emit_ins_no_label(unify_nil,Mode),
	emit_ins_no_label(unify_list,Mode),
	emit_ins_no_label(unify_constant_neck_proceedq,Mode),
	emit_ins_no_label(unify_constant_neck_proceed,Mode),
	emit_ins_no_label(unify_nil_neck_proceed,Mode),
	emit_ins_no_label(u2_void_xvar,Mode),
	emit_ins_no_label(u2_void_yfvar,Mode),
	emit_ins(u2_void_yvar,Mode),
	emit_ins_no_label(u2_void_xval,Mode),
	emit_ins_no_label(u2_void_xlval,Mode),
	emit_ins_no_label(u2_void_yfval,Mode),
	emit_ins_no_label(u2_void_yval,Mode),
	emit_ins_no_label(u2_void_ylval,Mode),
	emit_ins_no_label(u2_xvar_void,Mode),
	emit_ins_no_label(u2_xvar_xvar,Mode),
	emit_ins_no_label(u2_xvar_yfvar,Mode),
	emit_ins(u2_xvar_yvar,Mode),
	emit_ins_no_label(u2_xvar_xval,Mode),
	emit_ins_no_label(u2_xvar_xlval,Mode),
	emit_ins_no_label(u2_xvar_yfval,Mode),
	emit_ins_no_label(u2_xvar_yval,Mode),
	emit_ins_no_label(u2_xvar_ylval,Mode),
	emit_ins_no_label(u2_yfvar_void,Mode),
	emit_ins(u2_yvar_void,Mode),
	emit_ins_no_label(u2_yfvar_xvar,Mode),
	emit_ins(u2_yvar_xvar,Mode),
	emit_ins_no_label(u2_yfvar_yvar,Mode),
	emit_ins(u2_yvar_yvar,Mode),
	emit_ins_no_label(u2_yfvar_xval,Mode),
	emit_ins(u2_yvar_xval,Mode),
	emit_ins_no_label(u2_yfvar_xlval,Mode),
	emit_ins(u2_yvar_xlval,Mode),
	emit_ins_no_label(u2_yfvar_yval,Mode),
	emit_ins(u2_yvar_yval,Mode),
	emit_ins_no_label(u2_yfvar_ylval,Mode),
	emit_ins(u2_yvar_ylval,Mode),
	emit_ins_no_label(u2_yfval_void,Mode),
	emit_ins_no_label(u2_yfval_xvar,Mode),
	emit_ins_no_label(u2_yfval_yfval,Mode),
	emit_ins_no_label(u2_yfval_xval,Mode),
	emit_ins_no_label(u2_yfval_xlval,Mode),
	emit_ins_no_label(u2_yfval_yval,Mode),
	emit_ins_no_label(u2_yfval_ylval,Mode),
	emit_ins_no_label(u2_xval_void,Mode),
	emit_ins_no_label(u2_xlval_void,Mode),
	emit_ins_no_label(u2_xval_xvar,Mode),
	emit_ins_no_label(u2_xlval_xvar,Mode),
	emit_ins_no_label(u2_xval_yfvar,Mode),
	emit_ins(u2_xval_yvar,Mode),
	emit_ins_no_label(u2_xlval_yfvar,Mode),
	emit_ins(u2_xlval_yvar,Mode),
	emit_ins_no_label(u2_xval_xval,Mode),
	emit_ins_no_label(u2_xval_xlval,Mode),
	emit_ins_no_label(u2_xlval_xval,Mode),
	emit_ins_no_label(u2_xlval_xlval,Mode),
	emit_ins_no_label(u2_xval_yfval,Mode),
	emit_ins_no_label(u2_xlval_yfval,Mode),
	emit_ins_no_label(u2_xval_yval,Mode),
	emit_ins_no_label(u2_xval_ylval,Mode),
	emit_ins_no_label(u2_xlval_yval,Mode),
	emit_ins_no_label(u2_xlval_ylval,Mode),
	emit_ins_no_label(u2_yval_void,Mode),
	emit_ins_no_label(u2_ylval_void,Mode),
	emit_ins_no_label(u2_yval_xvar,Mode),
	emit_ins_no_label(u2_ylval_xvar,Mode),
	emit_ins_no_label(u2_yval_yvar,Mode),
	emit_ins_no_label(u2_ylval_yvar,Mode),
	emit_ins_no_label(u2_yval_yfval,Mode),
	emit_ins_no_label(u2_ylval_yfval,Mode),
	emit_ins_no_label(u2_yval_xval,Mode),
	emit_ins_no_label(u2_yval_xlval,Mode),
	emit_ins_no_label(u2_ylval_xval,Mode),
	emit_ins_no_label(u2_ylval_xlval,Mode),
	emit_ins_no_label(u2_yval_yval,Mode),
	emit_ins_no_label(u2_yval_ylval,Mode),
	emit_ins_no_label(u2_ylval_yval,Mode),
	emit_ins_no_label(u2_ylval_ylval,Mode),
	emit_ins_no_label(bump_counterq,Mode),
	emit_ins(bump_counter,Mode),
	emit_ins_no_label(counted_neckq,Mode),
	emit_ins(counted_neck,Mode),
	emit_ins_no_label(fail,Mode),
	emit_ins_no_label(heapmargin_callq,Mode),
	emit_ins(heapmargin_call,Mode),
	emit_ins(neck,Mode),
	emit_ins(dynamic_neck_proceed,Mode),
	emit_ins(neck_proceed,Mode),
	emit_ins_no_label(proceed,Mode).

/* INSTRUCTION CODES */

/* <insn> ::= <opcode>{<e>|<f>|<i>|<l>|<t>|<x>|<y>|<z>|<C>|<E>|<Q>|<Y>|<Z>|<L>}
   <e> frame_size
   <v> var_count in SRI model only
   <f> functor
   <i> count
   <l> long
   <t> term
   <x> x operand
   <y> y operand
   <z> y operand, low bit -> unsafe
   <C> C/native code pointer
   <E> predicate pointer
   <Q> pad byte
   <Y> ::= <i>{<y>}
   <Z> ::= <i>{<z>}
   <L> large number, (spec functor and data object)
*/

%ins_op_format(ci_call, 241, "ii"). % Descr i_i
%ins_op_format(ci_inarg, 242, "ii"). % Descr i_i
%ins_op_format(ci_outarg, 243, "ii"). % Descr i_i
%ins_op_format(ci_retval, 244, "ii"). % Descr i_i

emit_ins_op(Ins) -->
	{ ins_op_format(Ins, Opcode, _) },
	"#define ", emit_uppercase(Ins), " ", emit_number(Opcode), "\n".

emit_all_ins_op -->
	"/* DO NOT EDIT!!! This file is autogenerated */\n",
	emit_ins_op(initcallq),
	emit_ins_op(initcall),
	emit_ins_op(firstcallq),
	emit_ins_op(firstcall),
	emit_ins_op(firstcall_1q),
	emit_ins_op(firstcall_1),
	emit_ins_op(firstcall_2q),
	emit_ins_op(firstcall_2),
	emit_ins_op(firstcall_3q),
	emit_ins_op(firstcall_3),
	emit_ins_op(firstcall_4q),
	emit_ins_op(firstcall_4),
	emit_ins_op(firstcall_5q),
	emit_ins_op(firstcall_5),
	emit_ins_op(firstcall_6q),
	emit_ins_op(firstcall_6),
	emit_ins_op(firstcall_7q),
	emit_ins_op(firstcall_7),
	emit_ins_op(firstcall_8q),
	emit_ins_op(firstcall_8),
	emit_ins_op(firstcall_nq),
	emit_ins_op(firstcall_n),
	emit_ins_op(callq),
	emit_ins_op(call),
	emit_ins_op(call_1q),
	emit_ins_op(call_1),
	emit_ins_op(call_2q),
	emit_ins_op(call_2),
	emit_ins_op(call_3q),
	emit_ins_op(call_3),
	emit_ins_op(call_4q),
	emit_ins_op(call_4),
	emit_ins_op(call_5q),
	emit_ins_op(call_5),
	emit_ins_op(call_6q),
	emit_ins_op(call_6),
	emit_ins_op(call_7q),
	emit_ins_op(call_7),
	emit_ins_op(call_8q),
	emit_ins_op(call_8),
	emit_ins_op(call_nq),
	emit_ins_op(call_n),
	emit_ins_op(lastcallq),
	emit_ins_op(lastcall),
	emit_ins_op(lastcall_1q),
	emit_ins_op(lastcall_1),
	emit_ins_op(lastcall_2q),
	emit_ins_op(lastcall_2),
	emit_ins_op(lastcall_3q),
	emit_ins_op(lastcall_3),
	emit_ins_op(lastcall_4q),
	emit_ins_op(lastcall_4),
	emit_ins_op(lastcall_5q),
	emit_ins_op(lastcall_5),
	emit_ins_op(lastcall_6q),
	emit_ins_op(lastcall_6),
	emit_ins_op(lastcall_7q),
	emit_ins_op(lastcall_7),
	emit_ins_op(lastcall_8q),
	emit_ins_op(lastcall_8),
	emit_ins_op(lastcall_nq),
	emit_ins_op(lastcall_n),
	emit_ins_op(executeq),
	emit_ins_op(execute),
	emit_ins_op(proceed),
	emit_ins_op(neck),
	emit_ins_op(neck_proceed),
	emit_ins_op(fail),
	emit_ins_op(branch),
	emit_ins_op(put_x_void),
	emit_ins_op(put_x_variable),
	emit_ins_op(put_x_value),
	emit_ins_op(put_x_unsafe_value),
	emit_ins_op(put_y_first_variable),
	emit_ins_op(put_y_variable),
	emit_ins_op(put_y_value),
	emit_ins_op(put_y_unsafe_value),
	emit_ins_op(put_constantq),
	emit_ins_op(put_constant),
	emit_ins_op(put_structureq),
	emit_ins_op(put_structure),
	emit_ins_op(put_nil),
	emit_ins_op(put_list),
	emit_ins_op(put_yfvar_yvar),
	emit_ins_op(put_yvar_yvar),
	emit_ins_op(put_xval_xval),
	emit_ins_op(put_yval_yval),
	emit_ins_op(put_yval_yuval),
	emit_ins_op(put_yuval_yval),
	emit_ins_op(put_yuval_yuval),
	emit_ins_op(get_x_variable),
	emit_ins_op(get_x_value),
	emit_ins_op(get_y_first_variable),
	emit_ins_op(get_y_variable),
	emit_ins_op(get_y_first_value),
	emit_ins_op(get_y_value),
	emit_ins_op(get_constantq),
	emit_ins_op(get_constant),
	emit_ins_op(get_structureq),
	emit_ins_op(get_structure),
	emit_ins_op(get_nil),
	emit_ins_op(get_list),
	emit_ins_op(get_constant_x0q),
	emit_ins_op(get_constant_x0),
	emit_ins_op(get_structure_x0q),
	emit_ins_op(get_structure_x0),
	emit_ins_op(get_nil_x0),
	emit_ins_op(get_list_x0),
	emit_ins_op(get_xvar_xvar),
	emit_ins_op(get_yfvar_yvar),
	emit_ins_op(get_yvar_yvar),
	emit_ins_op(get_constant_neck_proceedq),
	emit_ins_op(get_constant_neck_proceed),
	emit_ins_op(get_nil_neck_proceed),
	emit_ins_op(unify_void),
	emit_ins_op(unify_void_1),
	emit_ins_op(unify_void_2),
	emit_ins_op(unify_void_3),
	emit_ins_op(unify_void_4),
	emit_ins_op(unify_x_variable),
	emit_ins_op(unify_x_value),
	emit_ins_op(unify_x_local_value),
	emit_ins_op(unify_y_first_variable),
	emit_ins_op(unify_y_variable),
	emit_ins_op(unify_y_first_value),
	emit_ins_op(unify_y_value),
	emit_ins_op(unify_y_local_value),
	emit_ins_op(unify_constantq),
	emit_ins_op(unify_constant),
	emit_ins_op(unify_structureq),
	emit_ins_op(unify_structure),
	emit_ins_op(unify_nil),
	emit_ins_op(unify_list),
	emit_ins_op(unify_constant_neck_proceedq),
	emit_ins_op(unify_constant_neck_proceed),
	emit_ins_op(unify_nil_neck_proceed),
	emit_ins_op(u2_void_xvar),
	emit_ins_op(u2_void_xval),
	emit_ins_op(u2_void_xlval),
	emit_ins_op(u2_void_yfvar),
	emit_ins_op(u2_void_yvar),
	emit_ins_op(u2_void_yfval),
	emit_ins_op(u2_void_yval),
	emit_ins_op(u2_void_ylval),
	emit_ins_op(u2_xvar_void),
	emit_ins_op(u2_xvar_xvar),
	emit_ins_op(u2_xvar_xval),
	emit_ins_op(u2_xvar_xlval),
	emit_ins_op(u2_xvar_yfvar),
	emit_ins_op(u2_xvar_yvar),
	emit_ins_op(u2_xvar_yfval),
	emit_ins_op(u2_xvar_yval),
	emit_ins_op(u2_xvar_ylval),
	emit_ins_op(u2_yfvar_void),
	emit_ins_op(u2_yvar_void),
	emit_ins_op(u2_yfvar_xvar),
	emit_ins_op(u2_yvar_xvar),
	emit_ins_op(u2_yfvar_yvar),
	emit_ins_op(u2_yvar_yvar),
	emit_ins_op(u2_yfvar_xval),
	emit_ins_op(u2_yvar_xval),
	emit_ins_op(u2_yfvar_xlval),
	emit_ins_op(u2_yvar_xlval),
	emit_ins_op(u2_yfvar_yval),
	emit_ins_op(u2_yvar_yval),
	emit_ins_op(u2_yfvar_ylval),
	emit_ins_op(u2_yvar_ylval),
	emit_ins_op(u2_xval_void),
	emit_ins_op(u2_xlval_void),
	emit_ins_op(u2_xval_xvar),
	emit_ins_op(u2_xlval_xvar),
	emit_ins_op(u2_xval_yfvar),
	emit_ins_op(u2_xlval_yfvar),
	emit_ins_op(u2_xval_yvar),
	emit_ins_op(u2_xlval_yvar),
	emit_ins_op(u2_xval_xval),
	emit_ins_op(u2_xlval_xval),
	emit_ins_op(u2_xval_xlval),
	emit_ins_op(u2_xlval_xlval),
	emit_ins_op(u2_xval_yfval),
	emit_ins_op(u2_xlval_yfval),
	emit_ins_op(u2_xval_yval),
	emit_ins_op(u2_xlval_yval),
	emit_ins_op(u2_xval_ylval),
	emit_ins_op(u2_xlval_ylval),
	emit_ins_op(u2_yfval_void),
	emit_ins_op(u2_yval_void),
	emit_ins_op(u2_ylval_void),
	emit_ins_op(u2_yfval_xvar),
	emit_ins_op(u2_yval_xvar),
	emit_ins_op(u2_ylval_xvar),
	emit_ins_op(u2_yval_yvar),
	emit_ins_op(u2_ylval_yvar),
	emit_ins_op(u2_yfval_xval),
	emit_ins_op(u2_yval_xval),
	emit_ins_op(u2_ylval_xval),
	emit_ins_op(u2_yfval_xlval),
	emit_ins_op(u2_yval_xlval),
	emit_ins_op(u2_ylval_xlval),
	emit_ins_op(u2_yfval_yfval),
	emit_ins_op(u2_yval_yfval),
	emit_ins_op(u2_ylval_yfval),
	emit_ins_op(u2_yfval_yval),
	emit_ins_op(u2_yval_yval),
	emit_ins_op(u2_ylval_yval),
	emit_ins_op(u2_yfval_ylval),
	emit_ins_op(u2_yval_ylval),
	emit_ins_op(u2_ylval_ylval),
	emit_ins_op(cutb_x),
	emit_ins_op(cutf),
	emit_ins_op(cutb_x_neck),
	emit_ins_op(cutb_neck),
	emit_ins_op(cutb_x_neck_proceed),
	emit_ins_op(cutb_neck_proceed),
	emit_ins_op(cute_x),
	emit_ins_op(cutf_x),
	emit_ins_op(cute_x_neck),
	emit_ins_op(cute_neck),
	emit_ins_op(cut_y),
	emit_ins_op(choice_x),
	emit_ins_op(choice_yf),
	emit_ins_op(choice_y),
	emit_ins_op(function_1q),
	emit_ins_op(function_1),
	emit_ins_op(function_2q),
	emit_ins_op(function_2),
	emit_ins_op(builtin_1q),
	emit_ins_op(builtin_1),
	emit_ins_op(builtin_2q),
	emit_ins_op(builtin_2),
	emit_ins_op(builtin_3q),
	emit_ins_op(builtin_3),
	emit_ins_op(retry_instance),
	emit_ins_op(kontinue),
	emit_ins_op(leave),
	emit_ins_op(exit_toplevel),
	emit_ins_op(dynamic_neck_proceed),
	emit_ins_op(retry_cq),
	emit_ins_op(retry_c),
	emit_ins_op(heapmargin_callq),
	emit_ins_op(heapmargin_call),
	emit_ins_op(get_constraint),
	emit_ins_op(bump_counterq),
	emit_ins_op(bump_counter),
	emit_ins_op(counted_neckq),
	emit_ins_op(counted_neck),
	emit_ins_op(put_largeq),
	emit_ins_op(put_large),
	emit_ins_op(get_largeq),
	emit_ins_op(get_large),
	emit_ins_op(get_large_x0q),
	emit_ins_op(get_large_x0),
	emit_ins_op(unify_largeq),
	emit_ins_op(unify_large),
	emit_ins_op(inittrue),
	emit_ins_op(firsttrue_n).
