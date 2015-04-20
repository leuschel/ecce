/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002 UPM-CLIP */


#if defined(Solaris) || defined(LINUX) || defined(DARWIN)
#include <string.h>
#else
#include <memory.h>
#endif


#include <unistd.h>

#include "compat.h"
#include "datadefs.h"
#include "support.h"

/* declarations for global functions accessed here */

#include "own_malloc_defs.h"
#include "start_defs.h"
#include "alloc_defs.h"
#include "support_defs.h"
#include "streams_defs.h"
#include "wamsupport_defs.h"
#include "timing_defs.h"
#include "initial_defs.h"

/* local declarations */

static TAGGED *get_tiny_blocks(void);
/*static char *mem_start; */  /* beginning of our virtual memory -- Shared */
extern int end;                             /* Does it exist in NeXT? (MCL) */



 /* total # bytes grabbed by malloc/realloc/free . Not accurately measured
    -- it depends on the mem. mang. own data structures (MCL) */
ENG_INT total_mem_count = 0;                                 /* Shared */

 /* # bytes used by the Prolog program & database code.  Probably not
    accurately measured (patched here and there) (MCL).  */
ENG_INT mem_prog_count = 0;                                     /* Shared */

 /* Number of predicates asserted */
ENG_INT num_of_predicates = 0;                                  /* Shared */

 /* locks for accessing the shared memory.  Although malloc() is MT-safe in
    Solaris (how 'bout Linux?), the engine uses several internal data
    structures for its own memory management which should be correctly
    updated.  So we use our own lock for providing an atomic access to
    checkalloc/checkrealloc/checkdealloc (MCL).
 */


extern SLOCK    mem_mng_l;

           /* From an execution profile, 24 seems a good threshhold (MCL) */
#define THRESHHOLD 24
#define NTINY 682

static TAGGED *tiny_blocks = NULL;                     /* Shared & locked */

static TAGGED *get_tiny_blocks()
{
  REGISTER int i;
  TAGGED *ptr;
  REGISTER TAGGED *p;
  TAGGED tail = 0;

 /* prt was a call to checkalloc, but I made a direct call to Malloc()
    because of recursive locks stopping the engine.  Thus, the total amount
    of memory is also increased here. */

  ptr = Malloc(NTINY*THRESHHOLD);
  p = ptr+(NTINY*THRESHHOLD>>2);

  if (!ptr) {
    ENG_perror("% Malloc");
    Release_slock(mem_mng_l);
    SERIOUS_FAULT("Memory allocation failed");
  }

  total_mem_count += NTINY*THRESHHOLD;

  for (i=NTINY; i>0; i--)
    p -= THRESHHOLD>>2, p[0] = tail, tail = (TAGGED)p;
  tiny_blocks = ptr+(THRESHHOLD>>2);
  return ptr;
}


/* segfault patch -- jf */
#if defined(MallocBase)     
#define ENSURE_ADDRESSABLE(P, SIZE) \
  (((TAGGED)(P) >= (TAGGED)MallocBase) && \
  (((TAGGED)(P) - (TAGGED)MallocBase) < POINTERMASK) && \
   (((TAGGED)(P) - (TAGGED)MallocBase + (SIZE)) < POINTERMASK))
#else
#define ENSURE_ADDRESSABLE(P, SIZE) \
  ((TAGGED)(P) < POINTERMASK)
#endif

TAGGED *checkalloc(size)
     int size;
{
  TAGGED *p;
  Wait_Acquire_slock(mem_mng_l);

  if (size<=THRESHHOLD) {
    if ((p=tiny_blocks))
      tiny_blocks = (TAGGED *)p[0];
    else
      p=get_tiny_blocks();
  } else {                                           /* size > THRESHHOLD */
    p = (TAGGED *)Malloc(size);

    if (!p) {
      ENG_perror("% Malloc");
      Release_slock(mem_mng_l);
      SERIOUS_FAULT("Memory allocation failed");
    }
    /* segfault patch -- jf */
    if (!ENSURE_ADDRESSABLE(p, size)) {
      Release_slock(mem_mng_l);
      SERIOUS_FAULT("Alloc: Memory out of addressable bounds!");
    }

    /* mem_prog_count += size+sizeof(TAGGED); */
    total_mem_count += size;
  }
#if defined(DEBUG)
  if (debug_mem)
    printf("checkalloc returned %x, %d chars\n", (unsigned int)p, size);
#endif
  Release_slock(mem_mng_l);
  return p;
}

void checkdealloc(ptr,decr)
     TAGGED *ptr;
     int decr;
{
  Wait_Acquire_slock(mem_mng_l);

  if (decr<=THRESHHOLD) {
    ptr[0] = (TAGGED)tiny_blocks;
    tiny_blocks = ptr;
  } else {
    total_mem_count -= decr;
    Free(ptr);
  }
#if defined(DEBUG)
  if (debug_mem)
    printf("checkdealloc freed %x, %d chars\n", (unsigned int)ptr, decr);
#endif
  Release_slock(mem_mng_l);
}


TAGGED *checkrealloc(ptr,decr,size)
     TAGGED *ptr;
     int decr, size;
{
  TAGGED *p;

  Wait_Acquire_slock(mem_mng_l);

  if (decr<=THRESHHOLD) {
    if (size<=THRESHHOLD)
      p = ptr;
    else {
      p = Malloc(size);                       /* Was a call to checkalloc */
      total_mem_count += size;
      if (!p) {
        ENG_perror("% Malloc");
        Release_slock(mem_mng_l);
        SERIOUS_FAULT("Memory allocation failed");
      }
      /* segfault patch -- jf */
      if (!ENSURE_ADDRESSABLE(p, size)) {
	Release_slock(mem_mng_l);
	SERIOUS_FAULT("Realloc: memory out of addressable bounds!");
      }
      memcpy(p, ptr, ((decr-1) & -4)+4);
      ptr[0] = (TAGGED)tiny_blocks;
      tiny_blocks = ptr;
    }
  } else                                             /* decr > THRESHHOLD */
    if (size<=THRESHHOLD) {
      if (!(p=tiny_blocks))
	p=get_tiny_blocks();
      memcpy(p, ptr, ((size-1) & -4)+4);
    } else {
      p = (TAGGED *)Realloc(ptr,size);
      if (!p) {
        ENG_perror("% realloc");
        Release_slock(mem_mng_l);
        SERIOUS_FAULT("Memory allocation failed");
      }
      /* segfault patch -- jf */
      if (!ENSURE_ADDRESSABLE(p, size)) {
	Release_slock(mem_mng_l);
	SERIOUS_FAULT("Realloc: memory out of addressable bounds!");
      }
      /* mem_prog_count  += (size-decr); */
      total_mem_count += (size-decr);
    }
#if defined(DEBUG)
  if (debug_mem)
  printf("checkrealloc returned %x, %d chars\n",
         (unsigned int)p, size);
#endif
  Release_slock(mem_mng_l);
  return p;
}

 /* Creates the wam structure, allocates its areas and initializes them.
    This returns an empty, fresh wam.  We do not add it here to the task
    state list; it needs its own thread, which we have after startwam() */


extern ENG_INT mem_prog_count;

struct worker *create_and_init_wam()
{
  Argdecl;
  /*ENG_INT saved_program_count = mem_prog_count;  */

  Arg = create_wam_storage();                         /* Just create *Arg */
  create_wam_areas(Arg);                      /* Make room for the stacks */
  numstack_init(Arg);                                     /* bignum areas */
  local_init_each_time(Arg);                               /* Local areas */
  return Arg;
}

/* Available workers are enqued here */

struct worker *wam_list = NULL;
SLOCK    wam_list_l;

struct worker *free_wam()
{
  struct worker *free_wam;

  Wait_Acquire_slock(wam_list_l);
  if (wam_list) {
    free_wam = wam_list;
    wam_list = Next_Worker(free_wam);
    Release_slock(wam_list_l);
    Next_Worker(free_wam) = NULL;
  } else {
    Release_slock(wam_list_l);
    free_wam = create_and_init_wam();
  }
  return free_wam;
}

void release_wam(wam)
     struct worker *wam;
{
  local_init_each_time(wam);
  Wait_Acquire_slock(wam_list_l);
  Next_Worker(wam) = wam_list;
  wam_list = wam;
  Release_slock(wam_list_l);
}


#define CREATE_TYPED_STORAGE(T) (struct T *)checkalloc(sizeof(struct T))

struct worker *create_wam_storage()
{
  struct worker *w;

  w = (struct worker *)checkalloc(SIZEOFWORKER(reg_bank_size));
  w->misc = CREATE_TYPED_STORAGE(misc_info);
  w->streams = CREATE_TYPED_STORAGE(io_streams);
  w->debugger_info = CREATE_TYPED_STORAGE(debugger_state);

  return w;
}


void create_wam_areas(Arg)
     Argdecl;
{
  int i, j;
  char *cp;

  Atom_Buffer = (char *)checkalloc(Atom_Buffer_Length=STATICMAXATOM);

  /* heap pointer is first free cell, grows ++ */
  GETENV(i,cp,"GLOBALSTKSIZE",GLOBALSTKSIZE);
  Heap_Start = checkalloc(i*sizeof(TAGGED));
  Heap_End =  HeapOffset(Heap_Start,i);
  Heap_Warn_Soft =  Heap_Warn =  HeapOffset(Heap_End,-CALLPAD);

  /* stack pointer is first free cell, grows ++ */
  GETENV(i,cp,"LOCALSTKSIZE",LOCALSTKSIZE);
  Stack_Start  = checkalloc(i*sizeof(TAGGED));
  Stack_End =  StackOffset(Stack_Start,i);
  Stack_Warn = StackOffset(Stack_End,-STACKPAD);

  /* trail pointer is first free cell, grows ++ */
  /* choice pointer is last busy cell, grows -- */
  GETENV(i,cp,"CHOICESTKSIZE",CHOICESTKSIZE);
  GETENV(j,cp,"TRAILSTKSIZE",TRAILSTKSIZE);
  i += j;
  Choice_End = Trail_Start = checkalloc(i*sizeof(TAGGED));
  Choice_Start =  Trail_End = TrailOffset(Trail_Start, i);


 /*  Do not touch the (TAGGED) type casting! Or the emulator will break! */

#if defined(USE_TAGGED_CHOICE_START)
  Tagged_Choice_Start = (TAGGED *)((TAGGED)Choice_Start + TaggedZero);
#endif
}


                             /* Global code */

void init_alloc()
{
  init_bootcode((INSN *)checkalloc(sizeof(INSN)*(6+BPTP)));
#if defined(INTERNAL_CALLING)
  init_internal_calling((INSN *)checkalloc(sizeof(INSN)*(6+BPTP)));
#endif
  init_startgoalcode((INSN *)checkalloc(sizeof(INSN)*(6+BPTP)));
  init_startgoalcode_cont((INSN *)checkalloc(sizeof(INSN)*(6+BPTP)));
  init_contcode((INSN *)checkalloc(sizeof(INSN)*(3*ARITYLIMIT)),
		(INSN *)checkalloc(sizeof(INSN)*(4+BPTP)));
}

/*  mem_start wrongly calculated, and mem_prog_count set to zero only once */
/*
void mem_prog_reset()
{
    mem_start = (char *)(&end);
#if MallocBase
  if (mem_start < (char *)MallocBase)
    mem_start = (char *)MallocBase;
#endif

  mem_prog_count = 0;
}
*/

/* program_usage: [sizeof_used_space, 0] */
BOOL program_usage(Arg)
     Argdecl;
{
  TAGGED x;

  MakeLST(x,TaggedZero,atom_nil);
  MakeLST(x,MakeInteger(Arg,mem_prog_count),x);
  return cunify(Arg,X(0),x);
}

/* internal_symbol_usage: [number_atoms_funcs_preds, number_pred_defs] */
BOOL internal_symbol_usage(Arg)
     Argdecl;
{
  TAGGED x;

  MakeLST(x,MakeInteger(Arg,num_of_predicates),atom_nil);
  MakeLST(x,MakeInteger(Arg,prolog_atoms->count),x);
  return cunify(Arg,X(0),x);
}


/* total_usage: [total_space, 0].  Changed to use total_mem_count (MCL) */
BOOL total_usage(Arg)
     Argdecl;
{
  TAGGED x;
  ENG_INT n;

/* n = (char *)sbrk(0) - mem_start; */
  n = total_mem_count;
  MakeLST(x,TaggedZero,atom_nil);
  MakeLST(x,MakeInteger(Arg,n),x);
  return cunify(Arg,X(0),x);
}


BOOL statistics(Arg)
     Argdecl;
{
  struct stream_node *s = Output_Stream_Ptr;
  ENG_INT used, free;
  ENG_LINT userclick0 = userclick();
  ENG_LINT systemclick0 = systemclick();
  ENG_LINT wallclick0 = wallclick();
  struct frame *newa;

  /*
  ENG_PRINTF1(s,
             "memory (total)    %10ld bytes\n",
             (long int)((char *)sbrk(0)-mem_start));
  */
  ENG_PRINTF1(s,
             "memory used (total)    %10ld bytes\n",
             (long int)total_mem_count);
  ENG_PRINTF1(s, 
              "   program space (including reserved for atoms): %ld bytes\n", 
              mem_prog_count);

  ENG_PRINTF1(s,
              "   number of atoms and functor/predicate names: %ld\n", 
              prolog_atoms->count);
  ENG_PRINTF1(s,
              "   number of predicate definitions: %ld\n", 
              num_of_predicates);

  used = HeapCharDifference(Heap_Start,w->global_top);
  free = HeapCharDifference(w->global_top,Heap_End);
  ENG_PRINTF3(s, 
              "   global stack   %10ld bytes:%10ld in use,%10ld free\n",
              used+free, used, free);

  ComputeA(newa,w->node);
  used = StackCharDifference(Stack_Start,newa);
  free = StackCharDifference(newa,Stack_End);
  ENG_PRINTF3(s,
              "   local stack    %10ld bytes:%10ld in use,%10ld free\n",
              used+free, used, free);

  used = TrailCharDifference(Trail_Start,w->trail_top);
  free = TrailCharDifference(w->trail_top,w->node)/2;
  ENG_PRINTF3(s,
              "   trail stack    %10ld bytes:%10ld in use,%10ld free\n",
              used+free, used, free);

  used = ChoiceCharDifference(Choice_Start,w->node);
  free = ChoiceCharDifference(w->node,w->trail_top)/2;
  ENG_PRINTF3(s,
              "   control stack  %10ld bytes:%10ld in use,%10ld free\n\n",
              used+free, used, free);

  ENG_PRINTF4(s,
              " %10.6f sec. for %ld global, %ld local, and %ld control space overflows\n",
              ((ENG_FLT)stats.ss_click)/stats.userclockfreq, stats.ss_global, stats.ss_local, stats.ss_control);
  ENG_PRINTF3(s,
              " %10.6f sec. for %ld garbage collections which collected %ld bytes\n\n",
              ((ENG_FLT)stats.gc_click)/stats.userclockfreq, stats.gc_count, stats.gc_acc*sizeof(TAGGED));

  ENG_PRINTF3(s,
              " runtime:    %10.6f sec. %12lld clicks at %12lld Hz\n",
	      (ENG_FLT)(userclick0-stats.startclick)/stats.userclockfreq,
              userclick0-stats.startclick,
	      stats.userclockfreq);
  ENG_PRINTF3(s,
              " usertime:   %10.6f sec. %12lld clicks at %12lld Hz\n",
	      (ENG_FLT)(userclick0-stats.startuserclick)/stats.userclockfreq,
	      userclick0-stats.startuserclick,
	      stats.userclockfreq);
  ENG_PRINTF3(s,
              " systemtime: %10.6f sec. %12lld clicks at %12lld Hz\n",
	      (ENG_FLT)(systemclick0-stats.startsystemclick)/stats.systemclockfreq,
              systemclick0-stats.startsystemclick,
	      stats.systemclockfreq);

  ENG_PRINTF3(s,
              " walltime:   %10.6f sec. %12lld clicks at %12lld Hz\n\n",
	      (ENG_FLT)(wallclick0-stats.startwallclick)/stats.wallclockfreq,
	      wallclick0-stats.startwallclick,
	      stats.wallclockfreq);

  return TRUE;
}
