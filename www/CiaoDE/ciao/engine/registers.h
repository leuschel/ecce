/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */

/* Access macros for the principal WAM registers, bytecode offsets, etc. */

#define SIZEOFWORKER(X) (sizeof(struct worker) + ((X)-ANY)*sizeof(TAGGED))

/* In registers.c */
extern int reg_bank_size;
extern char **prolog_argv;
extern int prolog_argc;
/*extern struct worker *self;*/

#define REGISTER register

/* #define REGISTER */

#define Arg w
#define Argdecl \
	REGISTER struct worker *w
#define WAMENV \
	Argdecl = self

#define WToX0		(sizeof(struct worker)/sizeof(TAGGED)-ANY)
#define Xb(I)		(*CharOffset(w,I)) /* I as bytecode operand */
#define X(I)		(w->term[I]) /* I as zero-based */

/* # X regs used for control in choicepoints for dynamic code */
/*
   X(0) -
   X(1) -
   X(2) - x2_next clause pointer / handle.
   X(3) -
   X(4) - clock (used even in conc. predicates, although ignored).
   X(5) - x5_next clause pointer / handle.
   ------ The next ones, only meaningful for concurrent predicates.
   X(6) - predicate root (needed in case there are no clause pointers - MCL).
   X(7) - blocking/non-blocking and exited/non exited (MCL).
   X(8) - pointer to previous dynamic concurrent choicepoint.
*/

#define X2_CHN 2
#define ClockSlot 4
#define X5_CHN 5
#define RootArg 6
#define InvocationAttr 7
#define PrevDynChpt 8
#define DynamicPreserved 9

#define BLOCKIDX (1<<0)
#define EXECIDX  (1<<1)

#define SET_BLOCKING(arg) (arg) = ((arg) | BLOCKIDX)
#define SET_NONBLOCKING(arg) (arg) = ((arg) & ~BLOCKIDX)
#define IS_BLOCKING(arg) ((arg) & BLOCKIDX)
#define IS_NONBLOCKING(arg) !((arg) & BLOCKIDX)

#define SET_EXECUTING(arg) (arg) = ((arg) | EXECIDX)
#define SET_NONEXECUTING(arg) (arg) = ((arg) & ~EXECIDX)
#define EXECUTING(arg) ((arg) & EXECIDX)
#define NONEXECUTING(arg) !((arg) & EXECIDX)


/* initial choicepoint */
#define InitialNode ChoiceCharOffset(Choice_Start,ArityToOffset(1))

/* initial value_trail size: leave room for an extra choicept */
#define InitialValueTrail (-(sizeof(struct node)/sizeof(TAGGED)-ANY))

#define EToY0		(sizeof(struct frame)/sizeof(TAGGED)-ANY)
#define Yb(I)		(*CharOffset(E,I)) /* I as bytecode operand */
#define Y(I)		(E->term[I]) /* I as zero-based */

/* for tracing: */
#define X_OffsetToIndex(O)	((O)/sizeof(TAGGED)-WToX0)
#define Y_OffsetToIndex(O)	((O)/sizeof(TAGGED)-EToY0)
#define FrameSizeToCount(O)	((O)/sizeof(TAGGED)-EToY0)



/* Private areas for a thread, related to the overall wam status */

#define Heap_Start          w->heap_start
#define Heap_End            w->heap_end
#define Heap_Warn_Soft      w->heap_warn_soft
#define Heap_Warn           w->heap_warn
#define Int_Heap_Warn       w->int_heap_warn
#define Stack_Start         w->stack_start
#define Stack_End           w->stack_end
#define Stack_Warn          w->stack_warn
#define Choice_End          w->choice_end
#define Choice_Start        w->choice_start


#define USE_TAGGED_CHOICE_START

#if defined(USE_TAGGED_CHOICE_START)
#define Tagged_Choice_Start w->tagged_choice_start
#endif

#define Trail_Start         w->trail_start
#define Trail_End           w->trail_end


/* These are related to the I/O pointers */

#define Input_Stream_Ptr    w->streams->input_stream_ptr
#define Output_Stream_Ptr   w->streams->output_stream_ptr
#define Error_Stream_Ptr    w->streams->error_stream_ptr


/* These access the private stack for bignum operations */

#define Numstack_Top        w->numstack_top
#define Numstack_End        w->numstack_end
#define Numstack_First      w->numstack_first
#define Numstack_Last       w->numstack_last


/* These keep the current state of the debugger. */

#define Current_Debugger_State w->debugger_info->current_debugger_state
#define Current_Debugger_Mode  w->debugger_info->current_debugger_mode


/* Local space to generate atoms and other general string operations */

#define Atom_Buffer          w->atom_buffer
#define Atom_Buffer_Length   w->atom_buffer_length


/* Incore compiling */

#define Last_Insn            (w->last_insn)

/* Expanded worker (not always active) */

#define Expanded_Worker (w->misc->expanded_worker)

#define Next_Worker(w)  (w->misc->next_worker)

#define Stop_This_Goal(w) (w->misc->stop_this_goal)

/* Global variables */

#define GLOBVAR(i) w->misc->globalvar[i]

/* The local (per-thread) definitions for garbage collection */

#define Gc_Total_Grey    (w->misc->gc_total_grey)
#define Gcgrey           (w->misc->gcgrey)
#define Total_Found      (w->misc->total_found)
#define Cvas_Found       (w->misc->cvas_found)
#define Gc_Aux_Node      (w->misc->gc_aux_node)
#define Gc_Choice_Start  (w->misc->gc_choice_start)
#define Gc_Trail_Start   (w->misc->gc_trail_start)
#define Gc_Heap_Start    (w->misc->gc_heap_start)
#define Gc_Stack_Start   (w->misc->gc_stack_start)


/* Topmost choicepoint for calls to concurrent facts. */

#define TopConcChpt     w->misc->top_conc_chpt

/* Global registers */

#define GlobReg(Which) w->misc->globalreg[Which]

/* Throwing exceptions */

#define ErrArgNo w->misc->errargno
#define Culprit w->misc->culprit


/* If you edit this defn you must update ComputeE as well. */
#define ComputeA(A,B) \
{ \
  if (w->local_top) \
    (A) = w->local_top; \
  else if (!StackYounger((A) = (B)->local_top,w->frame)) \
    (A) = StackCharOffset(w->frame,FrameSize(w->next_insn)); \
}

#define NewShadowregs(Gtop) \
{ \
  w->global_uncond = TagHVA(Gtop); \
  w->local_uncond = TagSVA(w->local_top); \
}

#define SetShadowregs(Chpt) \
{ \
  w->global_uncond = TagHVA((Chpt)->global_top); \
  w->local_uncond = TagSVA((Chpt)->local_top); \
}

#define SaveGtop(Chpt,Gtop) \
  (Chpt)->global_top = (Gtop);

#define RestoreGtop(Chpt) \
  w->global_top = (Chpt)->global_top;

#define SaveLtop(Chpt) \
  (Chpt)->local_top = w->local_top;

#define RestoreLtop(Chpt) \
  w->local_top = (Chpt)->local_top;

#define FrameSize(L)	(*((L)-1))

/* Number of bytes accessed off of L: */
#define LOffset		1


/* EVENTS    ------------------------------------- */

#define SetEvent	(TestEvent ? 0 : (Heap_Warn_Soft = Heap_Start))
#define TestEvent	(Heap_Warn!=Heap_Warn_Soft)

