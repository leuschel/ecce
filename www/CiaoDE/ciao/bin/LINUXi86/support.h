/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

#ifndef _SUPPORT_H
#define _SUPPORT_H

#include "debug.h"
#include "initial.h"

/*  Macros for BUILTIN C-PREDICATE support  */

extern INSN *bootcode;
#if defined(INTERNAL_CALLING)
extern INSN *internal_calling;
#endif
extern INSN *startgoalcode;
extern INSN *startgoalcode_cont;
extern INSN *contcode;
extern INSN *failcode;
extern INSN *exitcode;
extern struct try_node *termcode;
extern struct try_node *fail_alt;
extern BOOL predtrace;
extern BOOL prof_include_time;
extern ENG_INT mem_prog_count;
extern CLOCK def_clock, use_clock;
extern struct sw_on_key *switch_on_function;
extern struct definition *int_address;
/*extern char *emulator_path;*/ /* Unused now.  DCG. */
extern char *emulator_version;
extern char incremental_symbol_table_path[];
/*extern TAGGED *numstack_end;*/
/* extern char *mem_start; */ /* Unused now.  MCL. */

/* These are for error handling (DCG) */
/* Changed to be private for every thread */
/*
extern int ErrArgNo;
extern TAGGED Culprit;
*/

extern BOOL cunify PROTO((struct worker *w, TAGGED u, TAGGED v));
extern TAGGED ptr_to_stream PROTO((struct worker *w, struct stream_node *n));
extern TAGGED make_integer_check PROTO((Argdecl, ENG_INT i, INSN *op));
extern TAGGED make_float_check PROTO((Argdecl, ENG_FLT i, INSN *op));
extern TAGGED make_structure PROTO((Argdecl, TAGGED functor));
extern TAGGED init_atom_check PROTO((char *str));
extern TAGGED evaluate PROTO((Argdecl, TAGGED v));
extern TAGGED *checkalloc PROTO((int size));
extern TAGGED *checkrealloc PROTO((TAGGED *ptr, int decr, int size));

extern void add_definition PROTO((struct sw_on_key **swp, struct sw_on_key_node *node, TAGGED key, struct definition *def));
extern struct definition *insert_definition PROTO((struct sw_on_key **swp, TAGGED tagpname, int arity, BOOL insertp));
extern struct definition *find_definition PROTO((struct sw_on_key **swp, TAGGED term, TAGGED **argl, BOOL insertp));
extern struct definition *parse_definition PROTO((TAGGED complex));

extern struct stream_node *new_stream PROTO((TAGGED name, char *mode, FILE *file));
extern struct stream_node *stream_to_ptr PROTO((TAGGED t, int mode));
extern struct stream_node *stream_to_ptr_check PROTO((TAGGED t, int mode, int *errcode));
extern struct sw_on_key *new_switch_on_key PROTO((int size, struct try_node *otherwise));
extern struct try_node *def_retry_c PROTO((BOOL (*proc)(), int arity));
extern struct sw_on_key_node *incore_gethash PROTO((struct sw_on_key *sw, TAGGED k));
extern struct instance *current_instance PROTO((struct worker *w));
extern BOOL next_instance PROTO((struct worker *w, struct instance **ipp));
extern BOOL next_instance_conc PROTO((struct worker *w, struct instance **ipp));
extern struct sw_on_key_node *dyn_puthash PROTO((struct sw_on_key **swp, TAGGED k));
extern void leave_to_gc PROTO((int type, char *ptr));
extern void updateChoicepoints PROTO((int decrement));
extern void compressTrail PROTO((Argdecl, BOOL from_gc));
extern void print_string PROTO((struct stream_node *stream, char *p));
extern void print_variable PROTO((struct worker *w, struct stream_node *stream, TAGGED term));
extern void print_number PROTO((Argdecl, struct stream_node *stream, TAGGED term));
/*extern void print_atom PROTO((struct stream_node *stream, TAGGED term));*/
extern int compile_large PROTO((TAGGED term, INSN *p));
extern struct instance *compile_term_aux PROTO((struct worker *w, TAGGED head, TAGGED body, struct worker **w_n));

extern ENG_INT get_integer PROTO((TAGGED t));
extern ENG_FLT get_float PROTO((TAGGED t));
extern TAGGED make_large PROTO((Argdecl,TAGGED *p));
extern TAGGED make_integer PROTO((Argdecl,ENG_INT i));
extern TAGGED make_float PROTO((Argdecl,ENG_FLT f));

extern struct worker *create_wam_storage PROTO((void));

#define DerefHeap(Xderef,Ptr) \
{ \
  REGISTER TAGGED m_i, m_j; \
 \
  RefHeap(m_i,Ptr); \
  DerefHeapSwitch(m_i,m_j,{break;}) \
  Xderef = m_i; \
}

#define DerefCar(Xderef,Ptr) \
{ \
  REGISTER TAGGED m_i, m_j; \
 \
  RefCar(m_i,Ptr); \
  DerefHeapSwitch(m_i,m_j,{break;}) \
  Xderef = m_i; \
}

#define DerefCdr(Xderef,Ptr) \
{ \
  REGISTER TAGGED m_i, m_j; \
 \
  RefCdr(m_i,Ptr); \
  DerefHeapSwitch(m_i,m_j,{break;}) \
  Xderef = m_i; \
}

#define DerefArg(Xderef,Ptr,I) \
{ \
  REGISTER TAGGED m_i, m_j; \
 \
  RefArg(m_i,Ptr,I); \
  DerefHeapSwitch(m_i,m_j,{break;}) \
  Xderef = m_i; \
}

#define DerefHeapNext(Xderef,Ptr) \
{ \
  REGISTER TAGGED m_i, m_j; \
 \
  RefHeapNext(m_i,Ptr); \
  DerefHeapSwitch(m_i,m_j,{break;}) \
  Xderef = m_i; \
}


#define DEREF(Xderef,X) \
{ \
  REGISTER TAGGED m_i, m_j; \
 \
  m_i = X; \
  DerefSwitch(m_i,m_j,;) \
  Xderef = m_i; \
}

#define SwitchOnVar(Reg,Aux,HVACode,CVACode,SVACode,NVACode) \
{ \
    for (;;) \
      { \
	  if (!IsVar(Reg)) \
	    NVACode \
	  else if (Reg & TagBitSVA) \
	    { RefSVA(Aux,Reg); \
	      if (Reg!=Aux) { Reg=Aux; continue; } \
	      else SVACode \
	    } \
	  else if (!(Reg & TagBitCVA)) \
	    { RefHVA(Aux,Reg); \
	      if (Reg!=Aux) { Reg=Aux; continue; } \
	      else HVACode \
	    } \
	  else \
	    { RefCVA(Aux,Reg); \
	      if (Reg!=Aux) { Reg=Aux; continue; } \
	      else CVACode \
	    } \
	  break; \
	} \
}

#define SwitchOnHeapVar(Reg,Aux,HVACode,CVACode,NVACode) \
{ \
    for (;;) \
      { \
	  if (!IsVar(Reg)) \
	    NVACode \
	  else if (!(Reg & TagBitCVA)) \
	    { RefHVA(Aux,Reg); \
	      if (Reg!=Aux) { Reg=Aux; continue; } \
	      else HVACode \
	    } \
	  else \
	    { RefCVA(Aux,Reg); \
	      if (Reg!=Aux) { Reg=Aux; continue; } \
	      else CVACode \
	    } \
	  break; \
	} \
}



#define DerefSwitch(Reg,Aux,VarCode) \
{ \
  if (IsVar(Reg)) \
    do \
      if (Reg == (Aux = CTagToPointer(Reg))) \
	{VarCode;break;} \
    while (IsVar(Reg=Aux)); \
}

#define DerefHeapSwitch(Reg,Aux,VarCode) DerefSwitch(Reg,Aux,VarCode)


#define YoungerHeapVar(Q,R)	HeapYounger(Q,R)
#define YoungerStackVar(Q,R)	StackYounger(Q,R)

#define CondHVA(X)		(!OffHeaptop(X,w->global_uncond))
#define CondCVA(X)		(!OffHeaptop(TagHVA(TagToCVA(X)),w->global_uncond))
#define CondSVA(X)		(!OffStacktop(X,w->local_uncond))
#define CondStackvar(X)		CondSVA(X)

#define BindingOfHVA(X)		CTagToHVA(X)
#define BindingOfCVA(X)		CTagToCVA(X)
#define BindingOfSVA(X)		CTagToSVA(X)
#define BindingOfStackvar(X)	X

/* segfault patch -- jf */
void trail_push_check(Argdecl, TAGGED x);

/* segfault patch -- jf */
#define BindSVA(U,V) \
{ \
  if (CondSVA(U)) \
    TrailPushCheck(w->trail_top,U); \
  CTagToSVA(U) = V; \
}

/* segfault patch -- jf */
#define BindCVA(U,V) \
{ \
  TrailPushCheck(w->trail_top,U); \
  CTagToCVA(U) = V; \
}

/* segfault patch -- jf */
#define BindHVA(U,V) \
{ \
  if (CondHVA(U)) \
    TrailPushCheck(w->trail_top,U); \
  CTagToHVA(U) = V; \
}

#define Wake \
{ \
  SetEvent, Heap_Warn_Soft = HeapCharOffset(Heap_Warn_Soft,-1); \
}

#define WakeCount (TestEvent ? HeapCharDifference(Heap_Warn_Soft,Heap_Start) : 0)


#define PlainUntrail(TR,Ref,CONT) \
{ \
  Ref = TrailPop(TR); \
  if (!IsVar(Ref)) \
    CONT \
  else \
    CTagToPointer(Ref) = Ref; \
}
  
/* SERIOUS_FAULT - a fault that should not occur- indicating a corruption
                  such as following the STR tag not coming to a FNT tag
		  this kind of fault may not need to be testing in final
		  version but must in testing cause a total abort
   USAGE_FAULT   - a fault in the usage(incorrect parameters) of a 
                  builtin predicate - an error message is written.
   MINOR_FAULT   - a fault that should result in a error message being
                  written somewhere, but the builtin predicate just
		  fails and is not aborted
*/


/* Exit code from wam() when aborting */
#define WAM_ABORT -32768 /* see exceptions.pl */
#define WAM_INTERRUPTED -32767

#include <setjmp.h>
#include "compat.h"

extern JMP_BUF abort_env;

extern void failc(char *mesg);

#define EXIT(Y)           {ENG_PRINTF1(stream_user_error,"{ERROR: %s}\n", Y); \
                           fflush(NULL); \
                           at_exit(-1);}

#define SERIOUS_FAULT(Y)       {failc(Y); \
                                LONGJMP(abort_env, WAM_ABORT); }
                          
#define MAJOR_FAULT(Y)         {failc(Y); return FALSE;}

#define USAGE_FAULT(Y)         {failc(Y); return FALSE;}

#define MINOR_FAULT(Y)         {return FALSE;}

/* Error codes, xref errhandle.pl, internals.pl //) */

/* OGRAMA: error classification ISO PROLOG */

/* Errors identifiers cannot be zero (as 0 = -0) */
#define INSTANTIATION_ERROR     1
#define TYPE_ERROR(D)           (RANGE_PER_ERROR*START_TYPE+D)
#define DOMAIN_ERROR(D)         (RANGE_PER_ERROR*START_DOM+D)
#define EXISTENCE_ERROR(D)      (RANGE_PER_ERROR*START_EXIST+D)
#define PERMISSION_ERROR(D,F)   (RANGE_PER_ERROR*START_PERM+D*10+F)
#define REPRESENTATION_ERROR(D) (RANGE_PER_ERROR*START_REPRES+D)
#define EVALUATION_ERROR(D)     (RANGE_PER_ERROR*START_EVAL+D)
#define RESOURCE_ERROR          (RANGE_PER_ERROR*START_RES)
#define SYNTAX_ERROR            (RANGE_PER_ERROR*START_SYNTAX)
#define SYSTEM_ERROR            (RANGE_PER_ERROR*START_SYSTEM)
#define USER_EXCEPTION          (RANGE_PER_ERROR*START_USER)

#define RANGE_PER_ERROR 100                    /* Enough number of errors */

#define START_INST    0
#define START_TYPE    1
#define START_DOM     2
#define START_EXIST   3
#define START_PERM    4
#define START_REPRES  5
#define START_EVAL    6
#define START_RES     7
#define START_SYNTAX  8
#define START_SYSTEM  9
#define START_USER    10

/* TYPE_ERRORS */
#define STRICT_ATOM          0
#define ATOMIC               1
#define BYTE                 2
#define CHARACTER            3
#define COMPOUND             4
#define EVALUABLE            5
#define IN_BYTE              6
#define INTEGER              7
#define LIST                 8
#define NUMBER               9
#define PREDICATE_INDICATOR 10
#define VARIABLE            11
#define CALLABLE            12

/* DOMAIN_ERRORS */
#define CHARACTER_CODE_LIST     0
#define SOURCE_SINK             1
#define STREAM                  2
#define IO_MODE                 3
#define NOT_EMPTY_LIST          4
#define NOT_LESS_THAN_ZERO      5
#define OPERATOR_PRIORITY       6
#define PROLOG_FLAG             7
#define READ_OPTION             8
#define FLAG_VALUE              9
#define CLOSE_OPTION           10
#define STREAM_OPTION          11
#define STREAM_OR_ALIAS        12
#define STREAM_POSITION        13
#define STREAM_PROPERTY        14
#define WRITE_OPTION           15
#define OPERATOR_SPECIFIER     16


/* EXISTENCE_ERRORS */
#define PROCEDURE 0
/* SOURCE_SINK and STREAM already defined */
/*
#define SOURCE_SINK             1
#define STREAM                  2
*/

/* PERMISION_ERRORS: composed of type of action + object on which the action
   is defined */

/* PERMISSION_TYPE */
#define ACCESS      0
#define CREATE      1
#define INPUT       2
#define MODIFY      3
#define OPEN        4
#define OUTPUT      5
#define REPOSITION  6

/* OBJECTS */
#define BINARY_STREAM        0
/*
#define SOURCE_SINK             1
#define STREAM                  2
*/
#define TEXT_STREAM          3
#define FLAG                 4
#define OPERATOR             5
#define PAST_END_OF_STREAM   6
#define PRIVATE_PROCEDURE    7
#define STATIC_PROCEDURE     8



/* REPRESENTATION_ERROR */

/* CHARACTER_CODE_LIST already defined */
/* #define CHARACTER_CODE_LIST     0 */
#define IN_CHARACTER_CODE   1
#define MAX_ARITY           2
/*#define CHARACTER            3*/
#define MAX_INTEGER         4
#define MIN_INTEGER         5
#define CHARACTER_CODE      6

/* EVALUATION_ERROR */
#define FLOAT_OVERFLOW 0
#define INT_OVERFLOW   1
#define E_UNDEFINED    2
#define E_UNDERFLOW    3
#define ZERO_DIVISOR   4


/* OGRAMA: OLD VERSION ---------------------------------------------------- */ 
/* #define TYPE_ERROR(Type) (32+Type) */ /* includes also domain errors */ 

/* #define INSTANTIATION_ERROR 1
#define READ_PAST_EOS_ERROR 2
#define NO_READ_PERMISSION 3
#define NO_WRITE_PERMISSION 4
#define NO_SUCH_FILE 5
#define NO_OPEN_PERMISSION 6
#define NO_ACCESS_PERMISSION 7
#define SYSTEM_ERROR 8 OGRAMA: Error de sistema */

/* Type codes for TYPE_ERROR  //) */
/* #define STRICT_ATOM 0
#define ATOMIC 1
#define BYTE 2
#define CALLABLE 3
#define COMPOUND 4
#define EVALUABLE 5
#define IN_BYTE 6
#define INTEGER 7
#define LIST 8
#define NUMBER 9
#define PREDICATE_INDICATOR 10
#define VARIABLE 11 
*/

/*
#define CHARACTER_CODE_LIST 32 First domain code 
#define STREAM_OR_ALIAS 33
#define SOURCE_SINK 34  OGRAMA */
/* END OLD VERSION ----------------------------------------------------------- */

#define BUILTIN_ERROR(Code,Culpr,ArgNo) \
   { ErrArgNo = ArgNo; Culprit = Culpr; return -Code; }

#define ERROR_IN_ARG(Arg,ArgNo,ReqType) \
{ ErrArgNo = ArgNo; Culprit = Arg; \
  return (IsVar(Arg) ? -INSTANTIATION_ERROR : -TYPE_ERROR(ReqType)); \
}


#define NDEREF(Wam, Reg,Aux,Exit) \
{ \
  DerefSwitch(Reg,Aux,{failc(illexp); Exit;}) \
  if (!IsNumber(Reg)) \
    { \
      Reg = evaluate(Wam, Reg); \
      if(!IsNumber(Reg)) \
        {failc(illexp); Exit;} \
    } \
}

/* MakeLST(To,Car,Cdr):
   
   Set 'To' to a term TAGGED LST
   whose car and cdr are 'Car' and Cdr'.

   'To' may be identical to 'Car' or 'Cdr'.
*/
#define MakeLST(To,Car,Cdr) \
{ TAGGED makelst_car = (Car); \
  HeapPush(w->global_top,makelst_car); \
  HeapPush(w->global_top,Cdr); \
  To = Tag(LST,HeapOffset(w->global_top,-2)); \
}

/* MakeSTR(To,Functor):
   
   Set 'To' to a term TAGGED STR
   whose principal functor is 'Functor'.  
   Space is allocated for the arguments, but they are not filled in.
*/
#define MakeSTR(To,Functor) \
{ \
  HeapPush(w->global_top,Functor); \
  To = Tag(STR,HeapOffset(w->global_top,-1)); \
  w->global_top = HeapOffset(w->global_top,Arity(Functor)); \
}

#define Unify_constant(U,V) \
{ REGISTER TAGGED m_t0, m_u=U, m_t1=V; \
  SwitchOnVar(m_t1,m_t0,{BindHVA(m_t1,m_u);}, \
	            {BindCVA(m_t1,m_u);Wake;}, \
	            {BindSVA(m_t1,m_u);}, \
		    {if (m_t1!=m_u) return FALSE;}) \
}


#define ENG_PRINTF1(S,FMT,A1) \
{ char m_buf[2048]; sprintf(m_buf,FMT,A1); print_string(S, m_buf); }

#define ENG_PRINTF2(S,FMT,A1,A2) \
{ char m_buf[2048]; sprintf(m_buf,FMT,A1,A2); print_string(S, m_buf); }

#define ENG_PRINTF3(S,FMT,A1,A2,A3) \
{ char m_buf[2048]; sprintf(m_buf,FMT,A1,A2,A3); print_string(S, m_buf); }

#define ENG_PRINTF4(S,FMT,A1,A2,A3,A4) \
{ char m_buf[2048]; sprintf(m_buf,FMT,A1,A2,A3,A4); print_string(S, m_buf); }

#define ENG_TTYPRINTF0(FMT) print_string(Error_Stream_Ptr,FMT) 

#define ENG_TTYPRINTF1(FMT,A1) ENG_PRINTF1(Error_Stream_Ptr,FMT,A1) 

#define ENG_TTYPRINTF2(FMT,A1,A2) ENG_PRINTF2(Error_Stream_Ptr,FMT,A1,A2) 

#define ENG_TTYPRINTF3(FMT,A1,A2,A3) ENG_PRINTF3(Error_Stream_Ptr,FMT,A1,A2,A3) 

#define ENG_TTYPRINTF4(FMT,A1,A2,A3,A4) ENG_PRINTF4(Error_Stream_Ptr,FMT,A1,A2,A3,A4) 


#define EXPAND_ATOM_BUFFER(new_max_atom_length) \
{ \
     Atom_Buffer = \
        (char *)checkrealloc((TAGGED *)Atom_Buffer, \
                             Atom_Buffer_Length, \
                             new_max_atom_length); \
    Atom_Buffer_Length = new_max_atom_length; \
}


#endif /* _SUPPORT_H */
