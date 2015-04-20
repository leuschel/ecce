/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

/*         BASIC DATA DEFINITIONS  & SIMPLE MACROS
  the macros here involve casting,tagging detagging and the like
  term to pointer conversion must know where object are in virtual memory */


#ifndef _TERMDEFS_H
#define _TERMDEFS_H

/* SIMPLE TYPES  & various CONSTANTS    -------------------------------   */

#define ANY 1                /* ?? or 0  - for dynamic arrays in bytecode */
#define SAME 0

typedef unsigned short int INSN;                                /* 16 bit */
typedef unsigned long int TAGGED;                               /* 32 bit */
typedef unsigned long int UNTAGGED;                             /* 32 bit */

#if !defined(_WINDOWS_H)

typedef int BOOL;		/* Win32 includes this definition */
# define FALSE 0
# define TRUE 1
#endif

typedef long ENG_INT;
typedef long long ENG_LINT;
typedef double ENG_FLT;

/* #if defined(ppc) || defined (armv4l) */
#if defined(USE_LONG_DOUBLE)
typedef long double ENG_LFLT;
#else
typedef double ENG_LFLT;
#endif

/*** TAGGED DATATYPES --------------------------------***/


#define WORDSIZE 32
#define SmallShift 2		/* no. of GC bits, concides with 32bit align */
#define TAGSIZE 3
#define ARITYSIZE 8
#define TAGOFFSET (WORDSIZE-TAGSIZE) /* 29 */
#define ARITYOFFSET (WORDSIZE-TAGSIZE-1-ARITYSIZE) /* 20 */
#define ARITYLIMIT (1<<ARITYSIZE) /* 256 */

#define TAGMASK		((((TAGGED)1<<TAGSIZE)-1)<<TAGOFFSET) /* E000 0000 */
#define QMask		((TAGGED)1<<TAGOFFSET>>1) /* 1000 0000 */
#define ZMask		((TAGGED)1<<TAGOFFSET>>2) /* 0800 0000 */
#define POINTERMASK	(QMask-(1<<SmallShift)) /* 0FFF FFFC */
#define INDEXMASK	(((TAGGED)1<<ARITYOFFSET)-1) /* 000F FFFF */
 
                     /* Tag creates TAGGED from tag and pointer */  
#if MallocBase
#define Tag(T,P)	(((T)<<TAGOFFSET)+((TAGGED)(P) & POINTERMASK))
#else
#define Tag(T,P)	(((T)<<TAGOFFSET)+((TAGGED)(P)))
#endif

#define TagIndex(T,P)	(((T)<<TAGOFFSET)+((TAGGED)((P)<<SmallShift)))

#define PointerPart(T)	((ENG_INT)((T)&POINTERMASK))  

#if MallocBase
#define TagToPointer(T)	((TAGGED *)(((TAGGED)(T)&POINTERMASK)+MallocBase))
#else
#define TagToPointer(T)	((TAGGED *)((TAGGED)(T)&POINTERMASK))
#endif

#define IndexPart(T)	(((T)&INDEXMASK)>>SmallShift)

#define HasTag(X,T)	(((X) & TAGMASK) == ((T)<<TAGOFFSET))
#define TagOf(P)	((P)>>TAGOFFSET)  /* collects tag */
#define CT(T1,T2)	((T1)<<TAGSIZE|(T2)) /* for concatenating tags     */

#define IsVar(A)	((int)(A)>=0)        /* variable tags begin with 0 */

#define TagIsHVA(X)	((X) < CVA<<TAGOFFSET)
#define TagIsCVA(X)	HasTag(X,CVA)
#define TagIsSVA(X)	((int)(X) >= (int)(SVA<<TAGOFFSET))
#define TagIsSmall(X)	((int)(X) < (int)TaggedHigh)
#define TagIsLarge(X)   (TagIsSTR(X) && STRIsLarge(X))
#define TagIsNUM(X)	((int)(X) < (int)(ATM<<TAGOFFSET)) 
#define TagIsATM(X)	HasTag(X,ATM)
#define TagIsLST(X)	HasTag(X,LST)
#define TagIsSTR(X)	((X) >= (STR<<TAGOFFSET))
#define TagIsStructure(X) (TagIsSTR(X) && !STRIsLarge(X))
#define STRIsLarge(X)   (TagToHeadfunctor(X) & QMask)

#if MallocBase
#define TermToPointer(X)	((TAGGED *)((X) ^ (TaggedZero^MallocBase)))
#define TermToPointerOrNull(X)	((TAGGED *)((X)==TaggedZero ? 0 : \
					    (X) ^ (TaggedZero^MallocBase)))
#define PointerToTerm(X)	((TAGGED)(X) ^ (TaggedZero^MallocBase))
#define PointerToTermOrZero(X)	(!(X) ? TaggedZero : \
				 (TAGGED)(X) ^ (TaggedZero^MallocBase))
#else
#define TermToPointer(X)	((TAGGED *)((X) ^ TaggedZero))
#define TermToPointerOrNull(X)	((TAGGED *)((X) ^ TaggedZero))
#define PointerToTerm(X)	((TAGGED)(X) ^ TaggedZero)
#define PointerToTermOrZero(X)	((TAGGED)(X) ^ TaggedZero)
#endif

/* Assuming IsVar(X): */
#define VarIsCVA(X)	((int)(X<<1) >= (int)(CVA<<1<<TAGOFFSET))

/* Assuming !IsVar(X): */
#define TermIsATM(X)	((int)(X<<1) >= (int)(ATM<<1<<TAGOFFSET))
#define TermIsLST(X)	((int)(X<<1) < (int)(STR<<1<<TAGOFFSET))

/* Test for HVA, CVA, LST, STR i.e. 0, 1, 6, 7 (and LNUM)*/
/* This works for some machines, but not for others...
   #define IsHeapTerm(A)	((int)(A)+(SVA<<TAGOFFSET)>=0)
*/
#define IsHeapTerm(A)	((TAGGED)(A)+(SVA<<TAGOFFSET) < (NUM<<TAGOFFSET))

#define IsHeapVar(X)	((X) < (SVA<<TAGOFFSET))
#define IsStackVar(X)	((int)(X) >= (int)(SVA<<TAGOFFSET))
#define IsAtomic(X)	((int)(X) < (int)(LST<<TAGOFFSET))
#define IsComplex(X)	((X) >= (LST<<TAGOFFSET))

#define TermIsAtomic(X) (IsAtomic(X) || TagIsLarge(X))
#define TermIsComplex(X) (IsComplex(X) && !TagIsLarge(X))

#define TagBitFunctor  (1<<TAGOFFSET)       /* ATM or STR or large NUM */
#define TagBitComplex  (2<<TAGOFFSET)       /* LST or STR or large NUM */

#define TagBitCVA (1<<TAGOFFSET) /* CVA (or UBV) */
#define TagBitSVA (2<<TAGOFFSET) /* SVA (or UBV) */

/* If this ordering ever changes, must update TagToHVA etc. too! */

#define HVA ((TAGGED)0)		/* heap variable */
#define CVA ((TAGGED)1)		/* constrained variable */
#define SVA ((TAGGED)2)		/* stack variable */
#define UBV ((TAGGED)3)		/* Unbound -- low bits are array index */

#define NUM ((TAGGED)4)		/* number: small integer */
#define ATM ((TAGGED)5)		/* atom: low part is atmtab index */
#define LST ((TAGGED)6)		/* list */
#define STR ((TAGGED)7)		/* structure */

#define MAXTAG 7
#define NOTAG 8

#define ERRORTAG   ((TAGGED)0)	/* ERRORTAG is a TAGGED pointer guaranteed 
				   to be different from all TAGGED objects */

/* Tags + one more bit: 
   Funny objects are represented as small ints.

   Floats and integers > 26 bits are represented as structures with
   special functors. The functors have the subtag bit = 1.
   Float=NUM, integer=ATM.

   ATM = atom as index in atmtab.
*/

#define TaggedLow	Tag(NUM,0)
#define TaggedZero	(TaggedLow+ZMask)
#define TaggedHigh	(TaggedLow+QMask)
#define HighInt		(1<<25)

/* These should be used with caution. */
#define MakeSmall(X)	(((TAGGED)((ENG_INT)(X)<<SmallShift))+TaggedZero)
#define GetSmall(X)	((ENG_INT)(((X)>>SmallShift)-(TaggedZero>>SmallShift)))
#define GetString(X)	(TagToAtom(X)->name)

#define USE_ATOM_LEN

#if defined(USE_ATOM_LEN)
#define GetAtomLen(X)   (TagToAtom(X)->atom_len)
#endif

#define MakeFunctorFix   (TagIndex(ATM,2) + QMask)
#define MakeFunctorFloat (TagIndex(NUM,3) + QMask)
#define LargeIsFloat(X)  (!(TagToHeadfunctor(X)&TagBitFunctor))

#define MakeLarge(ARG,Ptr) make_large(ARG,(TAGGED *)(Ptr))
#define MakeInteger(ARG, X) (IntIsSmall(X) ? MakeSmall(X) : make_integer(ARG,X))
#define MakeFloat(ARG,X)	make_float(ARG,X)
#define MakeAtom(X)	TagIndex(ATM,X)
#define MakeString(X)	init_atom_check(X)

#define GetInteger(X)	(TagIsSmall(X) ? GetSmall(X) : get_integer(X))
#define GetFloat(X)     (TagIsSmall(X) ? (ENG_FLT)GetSmall(X) : get_float(X))

#define IntIsSmall(X)	((X) >= -HighInt && (X) < HighInt)
#define IsInteger(X)	(TagIsSmall(X) || (TagIsLarge(X) && !LargeIsFloat(X)))
#define IsFloat(X)	(TagIsLarge(X) && LargeIsFloat(X))
#define IsNumber(X)	(TagIsSmall(X) || TagIsLarge(X))
#define IsAtom(X)	TagIsATM(X)
#define IsString(X)	TagIsATM(X)

/* TAGGED TO POINTER and related -----------------------------------*/
/* manipulating TAGGED objects removing tag and getting correct type of
   pointer  */

#if !defined(MallocBase)
# define CTagToHVA(x) (*(TAGGED *)(x))
# define CTagToCVA(x) (*(TAGGED *)(x))
# define CTagToSVA(x) (*(TAGGED *)(x))
# define CTagToLST(x) (*(TAGGED *)(x))
# define CTagToSTR(x) (*(TAGGED *)(x))
# define CTagToGoal(x) (*HeapOffset((TAGGED *)(x),1))
# define CTagToDef(x) (*HeapOffset((TAGGED *)(x),2))
# define CTagToCar(x) (*(TAGGED *)(x))
# define CTagToCdr(x) (*HeapOffset((TAGGED *)(x),1))
# define CTagToArg(x,I) (*HeapOffset((TAGGED *)(x),I))
# define CTagToPointer(x) (*TagToPointer(x)) /* must mask */
#else
# define CTagToHVA(x) (*TagToHVA(x))
# define CTagToCVA(x) (*TagToCVA(x))
# define CTagToSVA(x) (*TagToSVA(x))
# define CTagToLST(x) (*TagToLST(x))
# define CTagToSTR(x) (*TagToSTR(x))
# define CTagToGoal(x) (*TagToGoal(x))
# define CTagToDef(x) (*TagToDef(x))
# define CTagToCar(x) (*TagToCar(x))
# define CTagToCdr(x) (*TagToCdr(x))
# define CTagToArg(x,I) (*TagToArg(x,I))
# define CTagToPointer(x) (*TagToPointer(x))
#endif


#if sun && mc68010
/* This gets around a compiler error! */
# define TagToHVA(x)	((TAGGED *)(x)) /* tag = 0 */
# define TagToCVA(x)	TagToPointer(x)
# define TagToSVA(x)	TagToPointer(x)
# define TagToUBV(x)	TagToPointer(x)
# define TagToLST(x)	TagToPointer(x)
# define TagToSTR(x)	TagToPointer(x)

# define TagHVA(x)	((TAGGED)(x)) /* tag = 0 */
# define TagSVA(x)	Tag(SVA,x)
# define TagCVA(x)	Tag(CVA,x)
#else
#if MallocBase
# define TagToHVA(x)	((TAGGED*)((x)+MallocBase)) /* tag = 0 */
# define TagToCVA(x)	((TAGGED*)((x)-(CVA<<TAGOFFSET)+MallocBase))
# define TagToSVA(x)	((TAGGED*)((x)-(SVA<<TAGOFFSET)+MallocBase))
# define TagToUBV(x)	((TAGGED*)((x)-(UBV<<TAGOFFSET)+MallocBase))
# define TagToLST(x)	((TAGGED*)((x)-(LST<<TAGOFFSET)+MallocBase))
# define TagToSTR(x)	((TAGGED*)((x)-(STR<<TAGOFFSET)+MallocBase))

# define TagHVA(x)	Tag(HVA,x)
# define TagSVA(x)	Tag(SVA,x)
# define TagCVA(x)	Tag(CVA,x)
#else
# define TagToHVA(x)	((TAGGED*)(x)) /* tag = 0 */
# define TagToCVA(x)	((TAGGED*)((x)-(CVA<<TAGOFFSET)))
# define TagToSVA(x)	((TAGGED*)((x)-(SVA<<TAGOFFSET)))
# define TagToUBV(x)	((TAGGED*)((x)-(UBV<<TAGOFFSET)))
# define TagToLST(x)	((TAGGED*)((x)-(LST<<TAGOFFSET)))
# define TagToSTR(x)	((TAGGED*)((x)-(STR<<TAGOFFSET)))

# define TagHVA(x)	((TAGGED)(x)) /* tag = 0 */
# define TagSVA(x)	Tag(SVA,x)
# define TagCVA(x)	Tag(CVA,x)
#endif
#endif


/* Functor hackery. --MC */
/*-----------------------*/

/* 1 + no. untyped words */
#define LargeArity(X)	(PointerPart(X)>>2)

#define LargeInsns(X)	(PointerPart(X)>>1)

#define Arity(X)	(PointerPart(X)>>ARITYOFFSET)
#define SetArity(X,A)	((TAGGED)(((X) & (TAGMASK | INDEXMASK)) | (A<<ARITYOFFSET)))

#define TagToAtom(X)	(atmtab[IndexPart(X)]->value.atomp)

    /* finding the principal functor of a structure */
    /* finding the arguments of a structure, first argument is 1 */
    /* finding the car & cdr of a list. */
    /* finding the constraints of a CVA. */
#define TagToHeadfunctor(X) CTagToSTR(X)
#define TagToArg(X,N)	HeapOffset(TagToSTR(X),N)
#define TagToCar(X)	TagToLST(X)
#define TagToCdr(X)	HeapOffset(TagToLST(X),1)
#define TagToGoal(X)	HeapOffset(TagToCVA(X),1)
#define TagToDef(X)	HeapOffset(TagToCVA(X),2)

#define TagToInstance(X)	((struct instance *)TermToPointerOrNull(X))
#define TagToInstHandle(X)	((InstanceHandle *) TermToPointerOrNull(X))
#define TagToInstancePtr(X)	((struct instance **)TermToPointerOrNull(X))
#define TagToStream(X)	((struct stream_node *)TermToPointer(X))
#define TagToRoot(X)	((struct int_info *)TermToPointer(X))
#define TagToEmul(X)	((struct emul_info *)TermToPointer(X))
#define TagToFunctor(X)	((struct definition *)TermToPointer(X))

struct numstack {
  TAGGED *end;
  struct numstack *next;
};


/* WAM REGISTERS */

/* There are some registers which whould be private to each worker.  Those
   which are not critical for speed are subindirected in their own blocks.
   Others I think can affect performance appear just at the worker level.
   The reason for this difference is that changing the size of the struct
   worker causes a real mess in the whole compiler, and a bootstrapping
   procedure is needed.  Thus, if new no critical registers are needed, they
   can be allocated inside of one of these blocks, and this does not change
   the size of the struct worker itself. */


struct io_streams {                    /* Speed not critical: I/O, anyway */
  struct stream_node *input_stream_ptr;
  struct stream_node *output_stream_ptr;
  struct stream_node *error_stream_ptr;
};

struct debugger_state {                      /* Debugging is made by hand */
  TAGGED current_debugger_state;
  TAGGED current_debugger_mode;
};

#if defined(GLOBVARS)
#define MAXGLOBALVARS 16
#endif

#define USE_GLOBAL_VARS 1
#if defined(USE_GLOBAL_VARS)
#define GLOBAL_VARS_ROOT (w->misc->global_vars_root)
#endif

struct misc_info{

/* Incore compilation of a clause sometimes requires expanding the number of
   X registers of the machine, which amounts to expanding the worker itself.
   In that case, new_worker (which is otherwise NULL) will point to the
   newly allocated worker.  This worker will be the active one upon normal
   returning to wam(). */

  struct worker *expanded_worker;

  /* From heapgc: need to have per-worker garbage collection */

  long gc_total_grey/* = 0 */; /* accumulated upper bound of garbage left */
                                         /* Must be explicitly inited !!! */
  long gcgrey;          /* upper bound(?) of garbage left in old segments */
  long total_found;        /* the number of non-garbage words in the heap */
  TAGGED cvas_found;                 /* the last CVA found while shunting */
  struct node *gc_aux_node;     /* aux. choicepoint for the WAM registers */
  struct node *gc_choice_start;
  TAGGED *gc_trail_start;
  TAGGED *gc_heap_start;
  struct frame *gc_stack_start;
  struct node *top_conc_chpt;  /* Topmost chicepoint for concurrent facts */
#if defined(GLOBVARS)
  TAGGED globalvar[MAXGLOBALVARS];                     /* Global registers */
#endif
#if defined(USE_GLOBAL_VARS)
  TAGGED global_vars_root;
#endif
   /* For error handling through exceptions */
  int errargno;
  TAGGED culprit;
			/* Access the goal descriptor for this thread */
  struct goal_str *goal_desc_ptr;

/* Available workers are enqueued in a list. */
  struct worker *next_worker;

/* This goal should stp right now! */
  BOOL stop_this_goal;
};


struct worker {

/* Space for miscelaneous stuff (or temporary hacks) */

  struct misc_info *misc;


/* Input and output streams */

  struct io_streams *streams;


/* Current debugger state.  Realloced into local heap. */

  struct debugger_state *debugger_info;


/* Save info for incore compilation */

  INSN *last_insn;


/* Temporary allocation for various operations regarding atoms and strings. */

  char *atom_buffer;
  int atom_buffer_length;


  
/* Private stack for operations with big numbers.  Better have fast maths. */

  TAGGED *numstack_top;
  TAGGED *numstack_end;
  struct numstack *numstack_first;
  struct numstack *numstack_last;

  /* Boundaries of different areas */

  TAGGED *heap_start;
  TAGGED *heap_end;
  TAGGED *heap_warn_soft;
  TAGGED *heap_warn;
  TAGGED *int_heap_warn;      /* Heap_Start if ^C was hit, else Heap_Warn */
    
  TAGGED *stack_start;
  TAGGED *stack_end;
  TAGGED *stack_warn;
    
  TAGGED *choice_end;
  TAGGED *choice_start;


#if defined(USE_TAGGED_CHOICE_START)
  TAGGED *tagged_choice_start;   /* Not used any more, but I can't just
                                    remove it: the size of the WRB is
                                    critical for the compiler and changing
                                    it is a real hassle */
#else
  TAGGED *dummy;                           /* Use up the space, therefore */
#endif    

  TAGGED *trail_start;
  TAGGED *trail_end;
  
  struct node *node;		/* choice pointer */
  struct node *next_node;	/* -""- at predicate entry */
  struct node *segment_node;	/* gc's segment choice point */
  INSN *insn;			/* program counter */
  TAGGED *structure;		/* structure pointer */
  TAGGED global_uncond;		/* first uncond. global variable */
  TAGGED local_uncond;		/* first uncond. local variable no. */
  int value_trail;		/* size of value_trail extension of w->node */

  /* incidentally, the rest is similar to a struct node */
  TAGGED *trail_top;		/* trail pointer */
  TAGGED *global_top;		/* heap pointer */
  struct try_node *next_alt;	/* next clause at predicate entry */
  struct frame *frame;		/* environment pointer */
  INSN *next_insn;		/* continuation */
  struct frame *local_top;	/* local stack pointer, or NULL if invalid */
  TAGGED term[ANY];		/* temporary variables */
};


struct frame {			/* a.k.a. environment */
  struct frame *frame;		/* continuation frame pointer */
  INSN *next_insn;		/* continuation program pointer */
  TAGGED term[ANY];		/* permanent variables */
};

typedef enum {CHOICE,MARKER} node_type;

#if defined(MARKERS)
struct node {			/* a.k.a. marker. Collapsed with a Chpt? */
  /*  node_type type_of_node;*/
  TAGGED *trail_top;
  TAGGED *global_top;
  struct try_node *next_alt;
  struct frame *frame;
  INSN *next_insn;
  struct frame *local_top;
  TAGGED term[ANY];            /* Offset between nodes in try_node struct */
};

struct extension_regs {
  struct marker *topmost_marker;                    /* Last pushed marker */
};

struct marker {
  node_type type_of_node;
  struct marker* previous_marker;
  struct marker* next_marker;                         /* NULL if topmost */
  struct worker saved_state;              /* Huge, but should be reduced */
};

#else                                             /* Normal choicepoints */

struct node {			                  /* a.k.a. choice_point */
  TAGGED *trail_top;
  TAGGED *global_top;
  struct  try_node *next_alt;
  struct  frame *frame;
  INSN   *next_insn;
  struct  frame *local_top;
  TAGGED  term[ANY];
};

#endif

typedef unsigned long Bignum;

#define ChoiceptMarkPure(b) (*(TAGGED *)(&(b)->trail_top) |= 1)
#define ChoiceptTestPure(b) ((TAGGED)(b)->trail_top & 1)

#define ChoiceptMarkStatic(b) (*(TAGGED *)(&(b)->trail_top) |= 2)
#define ChoiceptTestStatic(b) ((TAGGED)(b)->trail_top & 2)

#if (MallocBase>>28 & 0x8)
#define ChoiceptMarkNoCVA(b) (*(TAGGED *)(&(b)->trail_top) &= ~0x80000000)
#define ChoiceptTestNoCVA(b) (!((TAGGED)(b)->trail_top & 0x80000000))
#else
#define ChoiceptMarkNoCVA(b) (*(TAGGED *)(&(b)->trail_top) |= 0x80000000)
#define ChoiceptTestNoCVA(b) ((TAGGED)(b)->trail_top & 0x80000000)
#endif

/*  Misc   ---------------------------------------------  */

#define BPL		(sizeof(long)/sizeof(INSN))
#define BPT		(sizeof(TAGGED)/sizeof(INSN))
#define BPTP		(sizeof(TAGGED *)/sizeof(INSN))
#define BPF		(sizeof(ENG_FLT)/sizeof(INSN))

/* Deposit Source into Mask:ed portion of Dest */
#define Deposit(Source,Mask,Dest) (((Source)&(Mask))|((Dest)& ~(Mask)))

#endif /* _TERMDEFS_H */
