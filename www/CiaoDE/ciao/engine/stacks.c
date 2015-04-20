/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

#if defined(DEBUG) || defined(THREADS)
#include "threads.h"
#endif

#include "datadefs.h"
#include "support.h"


/* declarations for global functions accessed here */

#include "stacks_defs.h"
#include "heapgc_defs.h"
#include "start_defs.h"
#include "timing_defs.h"
#if defined(DEBUG)
#include "locks_defs.h"
#endif

/* local declarations */

static void calculate_segment_node(Argdecl);


/*extern void choice_overflow();*/

/* stack_shift_usage: [global shifts,local+control/trail shifts,time spent] */


BOOL stack_shift_usage(Arg)
     Argdecl;
{
  TAGGED x;
  ENG_LINT time = (stats.ss_click*1000)/stats.userclockfreq;
  
  MakeLST(x,MakeInteger(Arg,time),atom_nil);
  time = stats.ss_local+stats.ss_control;
  MakeLST(x,MakeInteger(Arg,time),x);
  time = stats.ss_global;
  MakeLST(x,MakeInteger(Arg,time),x);
  return cunify(Arg,X(0),x);
}

/* termheap_usage: [sizeof_used_space, sizeof_free_space] */
BOOL termheap_usage(Arg)
     Argdecl;
{
  ENG_INT used, free;
  TAGGED x;
  
  used = HeapCharDifference(Heap_Start,w->global_top);
  free = HeapCharDifference(w->global_top,Heap_End);
  MakeLST(x,MakeInteger(Arg,free),atom_nil);
  MakeLST(x,MakeInteger(Arg,used),x);
  return cunify(Arg,X(0),x);
}

/* envstack_usage: [sizeof_used_space, sizeof_free_space] */
BOOL envstack_usage(Arg)
     Argdecl;
{
  ENG_INT used, free;
  TAGGED x;
  struct frame *newa;

  ComputeA(newa,w->node);
  used = StackCharDifference(Stack_Start,newa);
  free = StackCharDifference(newa,Stack_End);
  MakeLST(x,MakeInteger(Arg,free),atom_nil);
  MakeLST(x,MakeInteger(Arg,used),x);
  return cunify(Arg,X(0),x);
}

/* choice_usage: [sizeof_used_space, sizeof_free_space] */
BOOL choice_usage(Arg)
     Argdecl;
{
  ENG_INT used, free;
  TAGGED x;
  
  used = ChoiceCharDifference(Choice_Start,w->node);
  free = ChoiceCharDifference(w->node,w->trail_top)/2;
  MakeLST(x,MakeInteger(Arg,free),atom_nil);
  MakeLST(x,MakeInteger(Arg,used),x);
  return cunify(Arg,X(0),x);
}

/* trail_usage: [sizeof_used_space, sizeof_free_space] */
BOOL trail_usage(Arg)
     Argdecl;
{
  ENG_INT used, free;
  TAGGED x;
  
  used = TrailCharDifference(Trail_Start,w->trail_top);
  free = TrailCharDifference(w->trail_top,w->node)/2;
  MakeLST(x,MakeInteger(Arg,free),atom_nil);
  MakeLST(x,MakeInteger(Arg,used),x);
  return cunify(Arg,X(0),x);
}


/* Service routine for HEAPMARGIN* instructions.
 * pad - required amount of heap space.
 * arity - number of live X regs at this point.
 */
void explicit_heap_overflow(Arg,pad,arity)
     Argdecl;
     int pad, arity;
{
  REGISTER struct node *b = w->node;
  REGISTER int i;
  REGISTER struct frame *a;

#if defined(DEBUG)
  if (debug_gc)
    printf("Thread %d calling explicit_heap_overflow\n", (int)Thread_Id);
#endif

  
  /* ensure that w->node is fleshed out fully i.e. do a "neck" */
  /* arity of choicept could be greater than arity of clause */
  /* DO NOT clear w->next_alt -- we are still in "shallow mode" */
  if (!b->next_alt) {			/* try */
    b->next_alt = w->next_alt; /* 4 contiguous moves */
    b->frame = w->frame;
    b->next_insn = w->next_insn;
    SaveLtop(b);
    i=OffsetToArity(b->next_alt->node_offset);
    if (i>0){
      REGISTER TAGGED *t = (TAGGED *)w->next_node;
      
      do
        ChoicePush(t,X(--i));
      while (i>0);
    }
    if (ChoiceYounger(ChoiceOffset(b,CHOICEPAD),w->trail_top))
      choice_overflow(Arg,CHOICEPAD),
	b = w->node;
  }
  
  /* ensure that X regs are seen by heap_overflow(): make a frame */
  ComputeA(a,b);
  a->term[0] = TaggedZero;
  for (i=0; i<arity; i++)
    a->term[i+1] = X(i);
  a->frame = w->frame;
  a->next_insn = w->next_insn;
  w->frame = a;
  w->next_insn = &contcode[(1+LOffset)*(i+1)];
  w->local_top = (struct frame *)Offset(a,EToY0+i+1);
  heap_overflow(Arg,pad);
  for (i=0; i<arity; i++)
    X(i) = a->term[i+1];
  w->local_top = a;
  w->frame = a->frame;
  w->next_insn = a->next_insn;
}


/* Set w->segment_node to most recent choicept which is marked as pure. */
static void calculate_segment_node(Arg)
     Argdecl;
{
  REGISTER struct node *n;

  w->segment_node = NULL;
  for (n=w->node;
       w->segment_node==NULL;
       n=ChoiceCharOffset(n,-n->next_alt->node_offset))
    if (ChoiceptTestPure(n))
      w->segment_node = n;
}

/* Here when w->node and w->trail_top are within CHOICEPAD from each other. */
void choice_overflow(Arg,pad)
     Argdecl;
     int pad;
{
  ENG_LINT click0;
  TAGGED *choice_top;
  struct try_node *next_alt;

#if defined(DEBUG)
  if (debug_gc)
    printf("Thread %d calling choice overflow\n", (int)Thread_Id);
#endif

  click0 = userclick();

  if (!(next_alt = w->node->next_alt)) /* ensure A', P' exist */
    w->node->next_alt = w->next_alt,
    SaveLtop(w->node);

  if (pad<0)
    pad = -pad;			/* in compile_term: disable trail_gc */
  else {
    calculate_segment_node(Arg);
    trail_gc(Arg);
    compressTrail(Arg,FALSE);
  }
				/* ASSUMED: --CHOICE, TRAIL++ */

  choice_top = (TAGGED *)w->node+w->value_trail;
  if (ChoiceYounger(ChoiceOffset(choice_top,2*pad),w->trail_top)) {
    REGISTER struct node *b;
    TAGGED *newtr;
    int mincount, newcount, oldcount, reloc_factor;
    
    {
      mincount = 2*pad - ChoiceDifference(choice_top,w->trail_top);
      oldcount = ChoiceDifference(Choice_Start,Choice_End);
      newcount = oldcount + (oldcount<mincount ? mincount : oldcount);
      newtr = checkrealloc(Trail_Start,
                           oldcount*sizeof(TAGGED),
                           newcount*sizeof(TAGGED));
#if defined(DEBUG)
      if (debug_gc)
        printf("Thread %d is reallocing TRAIL from %lx to %lx\n", 
               (int)Thread_Id, (long int)Trail_Start, (long int)newtr);
#endif
    }
    reloc_factor = (char *)newtr - (char *)Trail_Start;
    {
      REGISTER TAGGED *tr;
      TAGGED *trb;
      
      tr = (TAGGED *)((char *)Choice_Start+reloc_factor);
      trb = (TAGGED *)((char *)choice_top+reloc_factor);
      Trail_Start = Choice_End = newtr;                /* new low bound */
      Choice_Start = Trail_End = newtr+newcount;      /* new high bound */
      /* Do not take out (TAGGED) casting, or the engine will break!! */

#if defined(USE_TAGGED_CHOICE_START)
      Tagged_Choice_Start = (TAGGED *)((TAGGED)Choice_Start + TaggedZero);
#endif
      {
        REGISTER TAGGED *x;
	/* We have to relocate the concurrent topmost choicepoint */
#if defined(THREADS)
        REGISTER struct node *concchpt;
#endif
        
        x = Choice_Start;                  /* Copy the new choicepoint stack */
        while (OffChoicetop(trb,tr))
          ChoicePush(x,ChoiceNext(tr));
        w->node = b = (struct node *)(x-w->value_trail);

#if defined(THREADS)
        /* The chain of concurrent dynamic choicepoints has to be
           relocated as well.  The initial TopConcChpt was set to be
           the initial choice node.  MCL. */
        concchpt = TopConcChpt = 
          (struct node *)((char *)TopConcChpt + reloc_factor +
                          (newcount-oldcount)*sizeof(TAGGED));

        while(concchpt != InitialNode) {
#if defined(DEBUG)
          if (debug_concchoicepoints || debug_gc)
            printf("*** %d(%d) Changing dynamic chpt@%x\n",
                   (int)Thread_Id, (int)GET_INC_COUNTER, 
                   (unsigned int)concchpt);
#endif
          concchpt->term[PrevDynChpt] =
            PointerToTermOrZero(
                (struct node *)((char *)TermToPointerOrNull(concchpt->term[PrevDynChpt])
                                + reloc_factor 
                                + (newcount-oldcount)*sizeof(TAGGED))
                );
          concchpt = (struct node *)TermToPointerOrNull(concchpt->term[PrevDynChpt]);
        }
#endif
      }
    }
    w->next_node =
      (struct node *)((char *)w->next_node + reloc_factor +
                      (newcount-oldcount)*sizeof(TAGGED));
    w->trail_top = (TAGGED *)((char *)w->trail_top+reloc_factor);
    
    while (OffChoicetop(b,Choice_Start)){
      b->trail_top = (TAGGED *)((char *)b->trail_top+reloc_factor);
      b = ChoiceCharOffset(b,-b->next_alt->node_offset);
    }
  }
  
  w->node->next_alt = next_alt;

  stats.ss_control++;
  click0 = userclick()-click0;
  stats.startclick += click0;
  stats.lastclick += click0;
  stats.ss_click += click0;
}


/* Here when w->local_top and Stack_End are within STACKAD from each other. */
void stack_overflow(Arg)
     Argdecl;
{
  int count, reloc_factor;
  TAGGED *newh;
  REGISTER TAGGED t1 /* , t2 */ ; /* unused */
  REGISTER TAGGED *pt1;
  REGISTER struct node *n, *n2;
  ENG_FLT click0 = userclick();
  
#if defined(DEBUG)
  if (debug_gc) printf("Thread %d calling stack overflow\n", (int)Thread_Id);
#endif

  ComputeA(w->local_top,w->node);
  
  count = 2*StackDifference(Stack_Start,Stack_End);
  newh = checkrealloc(Stack_Start,
		      count*sizeof(TAGGED)/2,
		      count*sizeof(TAGGED));
#if defined(DEBUG)
  if (debug_gc)
    printf("Thread %d is reallocing STACK from %lx to %lx\n", 
           (int)Thread_Id, (long int)Stack_Start, (long int)newh);
#endif

  reloc_factor = (char *)newh - (char *)Stack_Start;

  /* HH, AA and TR are free pointers;  BB is last used word. */

  if (reloc_factor!=0) {
    struct node *aux_node;
    REGISTER struct frame *frame;
    int i;
    
    aux_node = ChoiceCharOffset(w->node,ArityToOffset(0));
    aux_node->next_alt = fail_alt;
    aux_node->frame = (struct frame *)((char *)w->frame+reloc_factor);
    aux_node->next_insn = w->next_insn;
    aux_node->local_top = (struct frame *)((char *)w->local_top+
                                           reloc_factor);
    
				/* relocate pointers in trail */
    pt1 = Trail_Start;
    while (TrailYounger(w->trail_top,pt1)) {
      t1 = TrailNext(pt1);
      if (TagIsSVA(t1))
        *(pt1-1) += reloc_factor;
    }
    
				/* relocate pointers in choice&env stks */
    for (n=aux_node; n!=InitialNode; n=n2){
      n2=ChoiceCharOffset(n,-n->next_alt->node_offset);
      *(TAGGED *)(&n2->local_top) += reloc_factor;
      *(TAGGED *)(&n2->frame) += reloc_factor;
      for (pt1=n->term; pt1!=(TAGGED *)n2;) {
        t1 = ChoicePrev(pt1);
        if (TagIsSVA(t1))
          *(pt1-1) += reloc_factor;
      }
      
      i = FrameSize(n->next_insn);
      frame = n->frame;
      while (frame >= n2->local_top) {
        pt1 = (TAGGED *)StackCharOffset(frame,i);
        while (pt1!=frame->term){
          t1 = *(--pt1);
          if (TagIsSVA(t1))
            *pt1 += reloc_factor;
        }
        if (frame->frame)
          *(TAGGED *)(&frame->frame) += reloc_factor,
            i = FrameSize(frame->next_insn),
            frame = frame->frame;
        else
          frame = NULL;
      } 
    }
    
    w->frame = aux_node->frame;
    w->local_top = aux_node->local_top;
    SetShadowregs(w->node);
  }
  
  Stack_Start = newh;		/* new low bound */
  Stack_End = newh+count;	/* new high bound */
  Stack_Warn = StackOffset(Stack_End,-STACKPAD);
  stats.ss_local++;
  click0 = userclick()-click0;
  stats.startclick += click0;
  stats.lastclick += click0;
  stats.ss_click += click0;
}


static BOOL gcexplicit = FALSE;       /* Shared, no locked --- global flag */


BOOL gc_start(Arg)
     Argdecl;
{
    gcexplicit = TRUE;
    heap_overflow(Arg,CALLPAD);

    return TRUE;
}

/* Here when w->global_top and Heap_End are within CALLPAD from each other. */
void heap_overflow(Arg,pad)
     Argdecl;
     int pad;
{
  TAGGED *oldh = w->global_top;
  TAGGED *newh = w->global_top;
  TAGGED *lowboundh;
  BOOL gc = gcexplicit;
  /*extern long gc_total_grey;*//*Now in a register*/

#if defined(DEBUG)
  if (debug_gc) printf("Thread %d calling heap_overflow\n", (int)Thread_Id);
#endif

  gcexplicit = FALSE;
  calculate_segment_node(Arg);
  if (gc ||
      (current_gcmode != atom_off &&
      HeapCharDifference(Heap_Start,oldh) >= GetSmall(current_gcmargin)*kB)) {
    GarbageCollect(Arg);
    newh = w->global_top;
    lowboundh = newh-Gc_Total_Grey;
    if (!gc &&
        (HeapCharDifference(newh,oldh) < GetSmall(current_gcmargin)*kB ||
         HeapYounger(HeapOffset(newh,2*pad),Heap_End)) &&
        !(HeapCharDifference(lowboundh,oldh) < GetSmall(current_gcmargin)*kB ||
          HeapYounger(HeapOffset(lowboundh,2*pad),Heap_End))) {
				       /* garbage collect the entire heap */
      w->segment_node = InitialNode;
      GarbageCollect(Arg);
      newh = w->global_top;
    }
  }
  if ((!gc &&
       HeapCharDifference(newh,oldh) < GetSmall(current_gcmargin)*kB) ||
      HeapYounger(HeapOffset(newh,2*pad),Heap_End)) {
    /* increase heapsize */
    int mincount, newcount, oldcount, reloc_factor;
    REGISTER TAGGED t1 /*, t2 */ ;  /* unused */
    REGISTER TAGGED *pt1;
    REGISTER struct node *n, *n2;
    TAGGED *newh;
    ENG_FLT click0 = userclick();
    
    int wake_count = HeapCharDifference(Heap_Warn_Soft,Heap_Start);
    
    ComputeA(w->local_top,w->node);
    
    mincount = 2*pad - HeapDifference(w->global_top,Heap_End);
    oldcount = HeapDifference(Heap_Start,Heap_End);
    newcount = oldcount + (oldcount<mincount ? mincount : oldcount);
    
    newh = checkrealloc(Heap_Start,
                        oldcount*sizeof(TAGGED),
                        newcount*sizeof(TAGGED));
#if defined(DEBUG)
    if (debug_gc)
      printf("Thread %d is reallocing HEAP from %lx to %lx\n", 
             (int)Thread_Id, (long int)Heap_Start, (long int)newh);
#endif


    reloc_factor = (char *)newh - (char *)Heap_Start;
      
    /* AA, HH and TR are free pointers;  BB is last used word. */
      
    if (reloc_factor!=0) {
      struct node *aux_node;
      REGISTER struct frame *frame;
      int i;
      
      aux_node = ChoiceCharOffset(w->node,ArityToOffset(0));
      aux_node->next_alt = fail_alt;
      aux_node->frame = w->frame;
      aux_node->next_insn = w->next_insn;
      aux_node->global_top = w->global_top;
      aux_node->local_top = w->local_top; /* segfault patch -- jf */
      
      /* relocate pointers in global stk */
      pt1 = newh;
      w->global_top = (TAGGED *)((char *)w->global_top + reloc_factor);
      while (HeapYounger(w->global_top,pt1)) {
	t1 = HeapNext(pt1);
	if (t1&QMask) pt1 += LargeArity(t1);
	else if (IsHeapTerm(t1))
	  *(pt1-1) += reloc_factor;
      }

#if defined(USE_GLOBAL_VARS)
      /* relocate pointers in global vars root */
      if (IsHeapTerm(GLOBAL_VARS_ROOT)) {
        GLOBAL_VARS_ROOT += reloc_factor;
      }
#endif

      /* relocate pointers in trail stk */
      pt1 = Trail_Start;
      TrailPush(w->trail_top,Current_Debugger_State);
      while (TrailYounger(w->trail_top,pt1)) {
	t1 = TrailNext(pt1);
	if (IsHeapTerm(t1))
	  *(pt1-1) += reloc_factor;
      }
      Current_Debugger_State = TrailPop(w->trail_top);
      
      
      /* relocate pointers in choice&env stks */
      for (n=aux_node; n!=InitialNode; n=n2)
	{
	  n2=ChoiceCharOffset(n,-n->next_alt->node_offset);
	  for (pt1=n->term; pt1!=(TAGGED *)n2;)
	    {
	      t1 = ChoicePrev(pt1);
	      if (IsHeapTerm(t1))
		*(pt1-1) += reloc_factor;
	    }
	  
	  i = FrameSize(n->next_insn);
	  frame = n->frame;
	  while (frame >= n2->local_top)
	    {
	      pt1 = (TAGGED *)StackCharOffset(frame,i);
	      while (pt1!=frame->term)
		{
		  t1 = *(--pt1);
		  if (IsHeapTerm(t1))
		    *pt1 += reloc_factor;
		}
	      i = FrameSize(frame->next_insn);
	      frame = frame->frame;
	    } 
	  *(TAGGED *)(&n->global_top) += reloc_factor;
	}
      *(TAGGED *)(&n->global_top) += reloc_factor;
      
      SetShadowregs(w->node);
    }
    
    Heap_Start = newh; /* new low bound */
    Heap_End = newh+newcount; /* new high bound */
    Int_Heap_Warn = (Int_Heap_Warn==Heap_Warn
		     ? HeapOffset(Heap_End,-CALLPAD)
		     : Heap_Start);
    Heap_Warn = HeapOffset(Heap_End,-CALLPAD);
    if (wake_count>=0)
      Heap_Warn_Soft = HeapCharOffset(Heap_Start,-wake_count);
      else
	Heap_Warn_Soft = Int_Heap_Warn;
    stats.ss_global++;
    click0 = userclick()-click0;
    stats.startclick += click0;
    stats.lastclick += click0;
    stats.ss_click += click0;
  }
}



/* Collect all constraints that have been woken "recently" by
   scanning the newest trail segment.  Also, excise such entries
   belonging to the newest heap segment. */
void collect_goals_from_trail(Arg,wake_count)
     Argdecl;
     int wake_count;
{
  int sofar=0;
  REGISTER TAGGED *tr = w->trail_top;
  REGISTER TAGGED *h = w->global_top;
  TAGGED *tr0 = NULL;
  TAGGED *limit = TagToPointer(w->node->trail_top);
  
  while (sofar<wake_count && TrailYounger(tr,limit))
    {
      TAGGED ref, value;

      ref = TrailPop(tr);
      if (!TagIsCVA(ref))
	continue;
      RefCVA(value,ref);
      if (value==ref)
	SERIOUS_FAULT("wake - unable to find all goals");
      if (sofar++ > 1)
	{
	  HeapPush(h,X(0));
	  HeapPush(h,X(1));
	  X(1) = Tag(LST,HeapOffset(h,-2));
	}
      else if (sofar > 1)
	X(1) = X(0);

      X(0) = Tag(LST,TagToGoal(ref));
      if (!CondCVA(ref))
	{
	  tr0=tr, *tr=0;
	}
    }
  w->global_top = h;
  Heap_Warn_Soft = Heap_Start;	/* make WakeCount==0 */

  if (sofar<wake_count)
    SERIOUS_FAULT("wake - unable to find all goals")
  else if (sofar==1)
    X(1) = CTagToCdr(X(0)),
    X(0) = CTagToCar(X(0));

  /* now compress the trail */

  if (tr0)
    {
      h = tr = tr0;
      while (TrailYounger(w->trail_top,tr))
	{
	  TAGGED ref;
	  
	  if ((ref = TrailNext(tr)))
	    TrailPush(h,ref);
	}
      w->trail_top = h;
    }
}


/* Tidy new half of trail exhaustively. */
void trail_gc(Arg)
     Argdecl;
{
  REGISTER TAGGED *tr = w->trail_top;
  REGISTER struct node *b = w->node;
  int wake_count = WakeCount;
  TAGGED heap_last = TagHVA(HeapOffset(Heap_End,-1));
  /*extern struct node *gc_aux_node;*/ /* Now in a register */
  /*extern struct node *gc_choice_start;*/ /* No in a register */
  /*extern TAGGED *gc_trail_start;*/ /* now in a register */

  Gc_Aux_Node = ChoiceCharOffset(b,ArityToOffset(0));
  Gc_Aux_Node->next_alt = fail_alt;
  Gc_Aux_Node->trail_top = tr;
  Gc_Choice_Start = w->segment_node;
  Gc_Trail_Start = TagToPointer(w->segment_node->trail_top);
  
  if (current_gctrace == atom_verbose) {
        ENG_TTYPRINTF0("{GC}  Trail GC started\n");
  }

  while (!OffChoicetop(Gc_Choice_Start,b)) {
    /* sweep trail segment to get rid of multiple 'undo setarg'
       trailings of the same location.  Go from old to new. */
    REGISTER TAGGED *x;
    REGISTER TAGGED t1;
      
    for (x=TagToPointer(b->trail_top); !OffTrailtop(x,tr); (void)TrailNext(x))
      if (TagIsHVA(t1 = *x)) {
        if (CTagToHVA(t1) & 1)
          *TrailOffset(x,-1) = *x = heap_last;
        else
          CTagToHVA(t1) ^= 1; /* turn mark bit on */
      }

    /* sweep trail segment to get rid of unconditional entries.
       Keep count of relevant entries.  Turn mark bits off.
       Go from new to old. */
    SetShadowregs(b);
    x=TagToPointer(b->trail_top);
    while (!OffTrailtop(x,tr)){
      REGISTER TAGGED t1 /*, *pt */ ; /* unused */

      t1 = TrailPop(tr);
      if (!IsVar(t1)) {
        /* kill unconditional 'undo setarg' */
        if (TagIsSTR(t1) &&
            TagToHeadfunctor(t1)==functor_Dsetarg &&
            !CondHVA(TagHVA(TagToPointer(CTagToArg(t1,2)))))
          *tr = 0;
      } else
        if (t1 & TagBitSVA) {
          if (!CondSVA(t1))
            *tr = 0;
        }
        else if (!(t1 & TagBitCVA)) {
          CTagToHVA(t1) ^= 1; /* turn mark bit off */
          if (!CondHVA(t1))
            *tr = 0;
        } else if (wake_count>0) --wake_count;
	  else if (!CondCVA(t1)) *tr = 0;
    }
    b = ChoiceCharOffset(b,-b->next_alt->node_offset);
  }
  
  /* restore misc. registers used above */

  b = w->node;
  SetShadowregs(b);
}
