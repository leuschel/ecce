/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

#include "datadefs.h"
#include "support.h"
#include "instrdefs.h"

/* declarations for global functions accessed here */

#include "wamsupport_defs.h"
#include "alloc_defs.h"

/* local declarations */

static struct try_node *get_null_alt(int arity);


INSN *startgoalcode;		       /* WAM code to start a goal -- Shared */
INSN *bootcode;		       /* WAM bootstrap to run bootgoal -- Shared */
#if defined(INTERNAL_CALLING)
INSN *internal_calling;		       /* WAM bootstrap to run bootgoal -- Shared */
#endif
INSN *startgoalcode_cont;    /* Specify cont. on success and failure -- Shared */
INSN *contcode;/* continuations of FrameSize N after exceptions -- Shared */
INSN *failcode;	      /* continuation of FrameSize 0 that fails -- Shared */
INSN *exitcode;	  /* continuation of FrameSize 0 that exits wam -- Shared */

static INSN insn_fail[1] = {FAIL};                              /* Shared */
struct try_node *null_alt = NULL;/* linked list of null alternatives - Shared*/

struct try_node *termcode;/* "clause" of arity 1 that exits wam -- Shared */
struct try_node *fail_alt;         /* null alternative, arity=0 -- Shared */

/* Find a null alt. of 'arity', or push a new one. */
static struct try_node *get_null_alt(arity)
     int arity;
{
  REGISTER struct try_node *a;
  ENG_INT current_mem = total_mem_count;

  for (a = null_alt; a; a = (a+1)->next)
    if (a->node_offset == ArityToOffset(arity)) return a;

  a = (struct try_node *)checkalloc(sizeof(struct try_node)
				    + sizeof(struct try_node *)
#if GAUGE
				    + 2*sizeof(ENG_INT)
#endif
				    );

  INC_MEM_PROG((total_mem_count - current_mem));

  a->node_offset = ArityToOffset(arity);
  a->number = 0;
  a->emul_p = insn_fail;
  a->emul_p2 = insn_fail;
  (a+1)->next = null_alt;	/* tail of list */
#if GAUGE
  a->entry_counter = (ENG_INT *)(a+1)+1;
  a->entry_counter[0] = 0;
  a->entry_counter[1] = 0;
#endif
  a->next = NULL;		/* no more alternatives */
  null_alt = a;
  return a;
}


/* return no. of bytes to skip if first alt. in read mode */
int p2_offset(insn)
     int insn;
{
  if
    ((insn==GET_NIL_X0) ||
     (insn==GET_LIST_X0))
    return 1;
  else if
    ((insn==GET_CONSTANT_X0) ||
     (insn==GET_STRUCTURE_X0))
      return 1+BPT;
  else if
    ((insn==GET_CONSTANT_X0Q) ||
     (insn==GET_STRUCTURE_X0Q))
      return 2+BPT;
  else return 0;
}

struct try_node *def_retry_c(proc,arity)
     BOOL (*proc)();
     int arity;
{
  struct try_node *item;
  INSN *bp;

  item = (struct try_node *) checkalloc(sizeof(struct try_node)
                                        +sizeof(struct emul_info)
                                        +(2+BPTP)*sizeof(INSN)
#if GAUGE
                                        +2*sizeof(ENG_INT)
#endif
                                        );
  item->node_offset = ArityToOffset(arity);
  item->number = 0;
  bp = item->emul_p = (INSN *)(((char *)item)+sizeof(struct try_node));
  item->emul_p2 = bp;
  item->next = item;
  *bp++ = RETRY_CQ;
  *bp++ = 0;			/* alignment */
  *(short **)bp = (short *)proc;
  bp += BPTP;
#if GAUGE
  item->entry_counter = (ENG_INT *)bp;
  item->entry_counter[0] = 0;
  item->entry_counter[1] = 0;
#endif
  return item;
}

/*
   |CALLQ|0|address_call/1|...padding...|Initial Frame Size|EXIT_TOPLEVEL|
   ^                                                       ^
   bootcode                                                termcode?
*/

void init_bootcode(b)
     INSN *b;
{
  bootcode = b;
  *b++ = CALLQ;
  *b++ = 0;			                         /* for alignment */
  *(struct definition **)b = address_call;
  b += BPTP;                                                   /* padding */
  *b++ = EToY0*sizeof(TAGGED);	                     /* initial FrameSize */
  *b++ = EXIT_TOPLEVEL;

  exitcode = b-2;
  termcode = def_retry_c(NULL,1);                 /* size of initial cpt. */
  b = termcode->emul_p;
  *b++ = EXIT_TOPLEVEL;

  address_nd_current_instance = def_retry_c(NULL,DynamicPreserved);
  b = address_nd_current_instance->emul_p;
  *b++ = RETRY_INSTANCE;
}

#if defined(INTERNAL_CALLING)
void init_internal_calling(b)
     INSN *b;
{
  internal_calling = b;
  *b++ = CALLQ;
  *b++ = 0;			                         /* for alignment */
  *(struct definition **)b = address_internal_call;
  b += BPTP;                                                   /* padding */
  *b++ = EToY0*sizeof(TAGGED);	                     /* initial FrameSize */
  *b++ = EXIT_TOPLEVEL;
}
#endif


/*
|CALLQ|0|address_call/1|...padding...|Init. Frame Size|EXIT_TOPLEVEL|
   ^                                                            ^
   bootcode                                                     termcode?
*/


void init_startgoalcode(b)
     INSN *b;
{
  startgoalcode = b;
  *b++ = CALLQ;
  *b++ = 0;			                         /* for alignment */
  *(struct definition **)b = address_call;
  b += BPTP;                                                   /* padding */
  *b++ = EToY0*sizeof(TAGGED);	                     /* initial FrameSize */
  *b++ = EXIT_TOPLEVEL;

  /*
    exitcode = b-2;
    termcode = def_retry_c(NULL,1);
    b = termcode->emul_p;
    *b++ = EXIT_TOPLEVEL;
    
    address_nd_current_instance = def_retry_c(NULL,DynamicPreserved);
    b = address_nd_current_instance->emul_p;
    *b++ = RETRY_INSTANCE;
  */
}

/*
|CALLQ|0|address_call_with_cont/2|...padding...|Init. Frame Size|EXIT_TOPLEVEL|
   ^                                                            ^
   bootcode                                                     termcode?
*/

void init_startgoalcode_cont(b)
     INSN *b;
{
  startgoalcode_cont = b;
  *b++ = CALLQ;
  *b++ = 0;			                         /* for alignment */
  *(struct definition **)b = address_call_with_cont;
  b += BPTP;                                                   /* padding */
  *b++ = EToY0*sizeof(TAGGED);	                     /* initial FrameSize */
  *b++ = EXIT_TOPLEVEL;
}


/* '&contcode[(1+LOffset)*(i+1)]'
    is a good continuation when interrupting 'foo/i'. */
void init_contcode(b,f)
     INSN *b, *f;
{
  int i;

  fail_alt = get_null_alt(0);

  failcode = f+LOffset;
  *f++ = EToY0*sizeof(TAGGED);	/* FrameSize */
  *f++ = EXECUTE;
  *(struct definition **)f = address_fail;

  contcode = b+LOffset;
  for (i=0; i<ARITYLIMIT; i++)
    {
      *b++ = (EToY0+i)*sizeof(TAGGED); /* FrameSize */
      *b++ = KONTINUE;
    }
}


TAGGED evaluate(Arg, v)
     Argdecl;                        /* To have a Wam ready for (*Proc)() */
     REGISTER TAGGED v;
{
  REGISTER TAGGED t, u;
  REGISTER TInfo Proc;

 restart:
  switch (TagOf(v))
    {
    case NUM:
      return v;

    case LST:
      DerefCdr(t,v);
      DerefCar(v,v);
      if (t==atom_nil)
	goto restart;
      else
	return ERRORTAG;

    case STR:
      if (STRIsLarge(v))
	return v;
      t = TagToHeadfunctor(v);
      Proc = incore_gethash(switch_on_function,t)->value.tinfo;
      if (Proc!=NULL)
	switch (Arity(t))
	  {
	  case 1:
	    RefArg(t,v,1);
	    return (*Proc)(Arg,t,NULL);
	  case 2:
	    RefArg(t,v,1);
	    RefArg(u,v,2);
	    return (*Proc)(Arg,t,u,NULL);
	  }

    default:
      return ERRORTAG;
    }
}
