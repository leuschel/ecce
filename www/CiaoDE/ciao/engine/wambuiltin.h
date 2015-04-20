/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

extern BOOL bu1_atom PROTO((Argdecl,TAGGED x0));
extern BOOL bu1_atomic PROTO((Argdecl,TAGGED x0));
extern BOOL bu1_float PROTO((Argdecl,TAGGED x0));
extern BOOL bu1_integer PROTO((Argdecl,TAGGED x0));
extern BOOL bu1_number PROTO((Argdecl,TAGGED x0));
extern BOOL bu1_var PROTO((Argdecl,TAGGED x0));
extern BOOL bu1_nonvar PROTO((Argdecl,TAGGED x0));
extern BOOL bu2_lexeq PROTO((Argdecl,TAGGED x0, TAGGED x1));
extern BOOL bu2_lexne PROTO((Argdecl,TAGGED x0, TAGGED x1));
extern BOOL bu2_lexlt PROTO((Argdecl,TAGGED x0, TAGGED x1));
extern BOOL bu2_lexle PROTO((Argdecl,TAGGED x0, TAGGED x1));
extern BOOL bu2_lexgt PROTO((Argdecl,TAGGED x0, TAGGED x1));
extern BOOL bu2_lexge PROTO((Argdecl,TAGGED x0, TAGGED x1));
extern BOOL bu2_numeq PROTO((Argdecl,TAGGED x0, TAGGED x1));
extern BOOL bu2_numne PROTO((Argdecl,TAGGED x0, TAGGED x1));
extern BOOL bu2_numlt PROTO((Argdecl,TAGGED x0, TAGGED x1));
extern BOOL bu2_numle PROTO((Argdecl,TAGGED x0, TAGGED x1));
extern BOOL bu2_numgt PROTO((Argdecl,TAGGED x0, TAGGED x1));
extern BOOL bu2_numge PROTO((Argdecl,TAGGED x0, TAGGED x1));
extern BOOL bu1_if PROTO((Argdecl,TAGGED x0));
extern BOOL bu2_univ PROTO((Argdecl,TAGGED x0, TAGGED x1));
extern BOOL bu3_functor PROTO((Argdecl,TAGGED x0, TAGGED x1, TAGGED x2));
