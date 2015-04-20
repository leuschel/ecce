/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

extern TAGGED fu1_minus PROTO((Argdecl, TAGGED x0, INSN *op));
extern TAGGED fu1_plus PROTO((Argdecl, TAGGED x0, INSN *op));
extern TAGGED fu1_integer PROTO((Argdecl, TAGGED x0, INSN *op));
extern TAGGED fu1_float PROTO((Argdecl, TAGGED x0, INSN *op));
extern TAGGED fu1_not PROTO((Argdecl, TAGGED x0, INSN *op));
extern TAGGED fu1_add1 PROTO((Argdecl, TAGGED x0, INSN *op));
extern TAGGED fu1_sub1 PROTO((Argdecl, TAGGED x0, INSN *op));
extern TAGGED fu2_plus PROTO((Argdecl, TAGGED x0, TAGGED x1, INSN *op));
extern TAGGED fu2_minus PROTO((Argdecl, TAGGED x0, TAGGED x1, INSN *op));
extern TAGGED fu2_times PROTO((Argdecl, TAGGED x0, TAGGED x1, INSN *op));
extern TAGGED fu2_fdivide PROTO((Argdecl, TAGGED x0, TAGGED x1, INSN *op));
extern TAGGED fu2_idivide PROTO((Argdecl, TAGGED x0, TAGGED x1, INSN *op));
extern TAGGED fu2_rem PROTO((Argdecl, TAGGED x0, TAGGED x1, INSN *op));
extern TAGGED fu2_mod PROTO((Argdecl, TAGGED x0, TAGGED x1, INSN *op));
extern TAGGED fu2_xor PROTO((Argdecl, TAGGED x0, TAGGED x1, INSN *op));
extern TAGGED fu2_and PROTO((Argdecl, TAGGED x0, TAGGED x1, INSN *op));
extern TAGGED fu2_or PROTO((Argdecl, TAGGED x0, TAGGED x1, INSN *op));
extern TAGGED fu2_lsh PROTO((Argdecl, TAGGED x0, TAGGED x1, INSN *op));
extern TAGGED fu2_rsh PROTO((Argdecl, TAGGED x0, TAGGED x1, INSN *op));
extern TAGGED fu2_compare PROTO((Argdecl, TAGGED x0, TAGGED x1, INSN *op));
extern TAGGED fu2_arg PROTO((Argdecl, TAGGED x0, TAGGED x1, INSN *op));
extern TAGGED fu2_gcd PROTO((Argdecl, TAGGED x0, TAGGED x1, INSN *op));
extern TAGGED fu1_abs PROTO((Argdecl, TAGGED x0, INSN *op));
extern TAGGED fu1_sign PROTO((Argdecl, TAGGED x0, INSN *op));

extern TAGGED fu1_intpart PROTO((Argdecl, TAGGED x0, INSN *op));
extern TAGGED fu1_fractpart PROTO((Argdecl, TAGGED x0, INSN *op));
extern TAGGED fu1_floor PROTO((Argdecl, TAGGED x0, INSN *op));
extern TAGGED fu1_round PROTO((Argdecl, TAGGED x0, INSN *op));
extern TAGGED fu1_ceil PROTO((Argdecl, TAGGED x0, INSN *op));
extern TAGGED fu2_pow PROTO((Argdecl, TAGGED x0, TAGGED x1, INSN *op));
extern TAGGED fu1_exp PROTO((Argdecl, TAGGED x0, INSN *op));
extern TAGGED fu1_log PROTO((Argdecl, TAGGED x0, INSN *op));
extern TAGGED fu1_sqrt PROTO((Argdecl, TAGGED x0, INSN *op));
extern TAGGED fu1_sin PROTO((Argdecl, TAGGED x0, INSN *op));
extern TAGGED fu1_cos PROTO((Argdecl, TAGGED x0, INSN *op));
extern TAGGED fu1_atan PROTO((Argdecl, TAGGED x0, INSN *op));


