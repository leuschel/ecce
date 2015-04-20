/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

int start_of_savedump = 0;                    /* Must be the first symbol */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

#include "compat.h"
#include "threads.h"
#include "datadefs.h"
#include "support.h"
#include "instrdefs.h"
#include "predtyp.h"
#include "wam.h"
#include "task_areas.h"

#include "attr_defs.h"
#include "builtin_defs.h"
#include "initial_defs.h"
#include "inout_defs.h"
#include "interrupt_defs.h"
#include "start_defs.h"
#include "misc_defs.h"
#include "nondet_defs.h"
#include "objareas_defs.h"
#include "stacks_defs.h"
#include "support_defs.h"
#include "term_support_defs.h"
#include "wam_defs.h"
#include "locks_defs.h"
#include "timing_defs.h"
#include "profile_defs.h"


/* private function declarations */


/* These are for error handling (DCG) */
/* Non shared --- into WAM structure */
/*
  int ErrArgNo;
  TAGGED Culprit;
*/

/*
extern BOOL cunify_args(), prolog_dif();
extern void choice_overflow(), heap_overflow(), stack_overflow();
extern void collect_one_pending_unification(), collect_goals_from_trail();
extern void control_c_normal PROTO((Argdecl));
extern BOOL foreign_ci_inarg(), foreign_ci_outarg(), foreign_ci_retval();
extern void explicit_heap_overflow(), clock_overflow();
*/

/*
#if defined(FOREIGN_FILES)
extern unsigned long *ci_table;
#endif
*/

extern struct definition                  /* Attributed variables support */
  *address_pending_unifications,
  *address_uvc,
  *address_ucc;


#define SAVE_FIELD(Name) worker->wam_private_state.Name = Name

#define SAVE_WAM_STATE \
  SAVE_FIELD(p); \
  SAVE_FIELD(i);\
  SAVE_FIELD(pt1);\
  SAVE_FIELD(pt2);\
  SAVE_FIELD(t0);\
  SAVE_FIELD(t1);\
  SAVE_FIELD(t2);\
  SAVE_FIELD(t3);\
  SAVE_FIELD(ptemp);\
  SAVE_FIELD(wam_exit_code);\
  SAVE_FIELD(ins)


#define RECOVER_FIELD(Name) Name = worker->wam_private_state.Name

#define RECOVER_WAM_STATE \
  RECOVER_FIELD(p);\
  RECOVER_FIELD(i);\
  RECOVER_FIELD(pt1);\
  RECOVER_FIELD(pt2);\
  RECOVER_FIELD(t0);\
  RECOVER_FIELD(t1);\
  RECOVER_FIELD(t2);\
  RECOVER_FIELD(t3);\
  RECOVER_FIELD(ptemp);\
  RECOVER_FIELD(wam_exit_code);\
  RECOVER_FIELD(ins)

int wam(Arg, worker)
     Argdecl;
     goal_descriptor_p worker;
{
                         /* KERNEL OF EMULATOR. */

  /* If the wam() local variables are changed, those on task_areas.h should
     be changed as well to reflect the current state! They should as well be
     saved and recovered in SAVE_WAM_STATE and RECOVER_WAM_STATE */

  REGISTER INSN *p;		                       /* program counter */
  REGISTER int i = ~0;  /* Avoid compiler complaints */
  REGISTER TAGGED
    *pt1 = NULL,		/* B and E share this.  B is valid after fail
				   and over short sequences of code.  E is
				   valid after allocate and after proceed. 
				   Allocate does not update w->frame.  */
    *pt2 = NULL,		/* H and S share this.  H is valid in
				   write mode.  S is valid in read mode.  */
    t0 = ~0,
    t1 = ~0, 
    t2 = ~0, 
    t3 = ~0;		/* temps for terms (decreasing importance) */
  REGISTER INSN *ptemp = NULL;		/* reg. decl. not critical */
  int wam_exit_code = 0;	/* halt/0, abort/0, reinitialise/0 */
  struct instance *ins;		/* clause/2, instance/2 */
  struct worker *new_worker;    /* Temp - for changes in regbanksize */
#if defined(PROFILE)
  BOOL count_retry = FALSE;
  BOOL prev_profile_trace = FALSE;
#endif

#if defined(DEBUG)
  if (debug_threads)
    printf("Worker state address is %x\n", (unsigned int)worker);
#endif

  if (worker && (worker->action & BACKTRACKING)) {
    RECOVER_WAM_STATE;
    goto fail;                                             /* Probably... */
  }

  goto proceed_r;

				/* MISCELLANEOUS SUPPORT */

 unify_t0_t1:

  SwitchOnVar(t0,i,
	      goto t0_is_hva;,
	      goto t0_is_cva;,
	      goto t0_is_sva;,
	      ;);
  
				/* one non variable */
  SwitchOnVar(t1,i,
	      { BindHVA(t1,t0); goto ReadMode; },
	      { BindCVA(t1,t0); Wake; goto ReadMode; },
	      { BindSVA(t1,t0); goto ReadMode; },
	      ;);

				/* two non variables */
  if (!(t1 ^= t0))		/* are they equal? */
    goto ReadMode;
  else if (t1>=QMask)		/* not the same type? */
    goto fail;
  else if (!(t0 & TagBitComplex)) /* atomic? */
    goto fail;
  else if (!(t0 & TagBitFunctor)) { /* lists? */
    t1 ^= t0;			/* restore t1 */
    if (cunify_args(Arg,2,TagToCar(t0),TagToCar(t1)))
      goto ReadMode;
    else goto fail;
  } else  {				/* structures */
    t1 ^= t0;			/* restore t1 */
    if (TagToHeadfunctor(t0) != (i=TagToHeadfunctor(t1)))
      goto fail;
    else if (i&QMask) {	/* large number */
      for (i = LargeArity(i)-1; i>0; i--)
        if (CTagToArg(t0,i) != CTagToArg(t1,i)) goto fail;
      goto ReadMode;
    } else if (cunify_args(Arg,Arity(i),TagToArg(t0,1),TagToArg(t1,1)))
      goto ReadMode;
    else goto fail;
  }
  
 t0_is_hva:
  SwitchOnVar(t1,i,
	      { if (t0==t1)
                ;
	      else if (YoungerHeapVar(TagToHVA(t1),TagToHVA(t0)))
		BindHVA(t1,t0)
                  else
                BindHVA(t0,t1); },
	      { BindHVA(t0,t1); },
	      { BindSVA(t1,t0); },
	      { BindHVA(t0,t1); });
  goto ReadMode;

 t0_is_cva:
  SwitchOnVar(t1,i,
	      { BindHVA(t1,t0); },
	      { if (t0==t1)
		  ;
	      else if (YoungerHeapVar(TagToCVA(t1),TagToCVA(t0)))
		{ BindCVA(t1,t0); Wake; }
	      else
		{ BindCVA(t0,t1); Wake; } },
	      { BindSVA(t1,t0); },
	      { BindCVA(t0,t1); Wake; });
  goto ReadMode;

 t0_is_sva:
  for (; TagIsSVA(t1); t1 = i) {
    RefSVA(i,t1);
    if (t1 == i) {
      if (t0==t1)
        goto ReadMode;
      else if (YoungerStackVar(TagToSVA(t1),TagToSVA(t0)))
        BindSVA(t1,t0)
	  else
	    BindSVA(t0,t1);
      goto ReadMode;
    }
  }
  BindSVA(t0,t1); goto ReadMode;

 suspend_on_t1:			/* Func, H must be live. */
  EMUL_TO_GOAL;
  if (TagIsSVA(t1=X(0)))	/* t1 may have been globalised. */
    RefSVA(t1,X(0));

 suspend_t3_on_t1:
  /* suspend the goal  t3  on  t1.  Func, H must be live. */
  if (TagIsHVAw(t1)) {
    LoadCVA(t0,H);
    pt1 = w->trail_top;
    if (CondHVA(t1)){
      TrailPush(pt1,t1);
      BindingOfHVA(t1) = t0;
    } else
      CTagToHVA(t1) = t0;
    goto check_trail;
  } else if (!CondCVA(t1)) {
    HeapPush(H,CTagToGoal(t1));
    HeapPush(H,CTagToDef(t1));
    CTagToGoal(t1) = Tag(LST,HeapOffset(H,-2));
    CTagToDef(t1) = Tag(LST,H);
  } else {
    LoadCVA(t0,H);
    HeapPush(H,Tag(LST,TagToGoal(t1)));
    HeapPush(H,Tag(LST,HeapOffset(H,1)));
    pt1 = w->trail_top;
    TrailPush(pt1,t1);
    BindingOfCVA(t1) = t0;
  check_trail:
    w->trail_top = pt1;
    if (ChoiceYounger(w->node,TrailOffset(pt1,CHOICEPAD)))
      choice_overflow(Arg,CHOICEPAD);
  }
  HeapPush(H,t3);
  HeapPush(H,PointerToTerm(Func));
  goto proceed_w;
  
 escape_to_p2:
  t2 = PointerToTerm(Func->code.intinfo);
 escape_to_p:
  EMUL_TO_GOAL;
  P = ptemp;
  X(0) = t3;
  X(1) = t2;
  goto switch_on_pred;
  
  /* FAILING */

 undo:
  w->trail_top = pt2;
  w->frame = B->frame;
  w->next_insn = B->next_insn;
  SetE(B->local_top);
  E->frame = w->frame;
  E->next_insn = w->next_insn;
  w->frame = E;
  w->next_insn = failcode;
  SetA(E,Offset(E,EToY0));
  LoadH;
  X(0) = t0;

  goto call1;

  /* FAILING */

 fail:

/* The profiling code must be here */
  PROFILE__HOOK_FAIL;
/*   (w->node->next_alt!=NULL); */

#if defined(DEBUG)
  if (debug_choicepoints){
  fprintf(stderr, "Failing: node = %x, next_node = %x, conc. node = %x\n",   
         (int)w->node, (int)w->next_node, (int)TopConcChpt);
  if ((w->misc->top_conc_chpt < w->node) &&
      (w->misc->top_conc_chpt < w->next_node))
    fprintf(stderr, "********** what happened here?\n");
  }
#endif
  Heap_Warn_Soft = Int_Heap_Warn;
  SetB(w->node);
  if (TrailYounger(pt2=w->trail_top,t1=(TAGGED)TagToPointer(B->trail_top))) {
    do
      PlainUntrail(pt2,t0,{goto undo;})
    while (TrailYounger(pt2,t1));
    w->trail_top = pt2;
  }

  RestoreGtop(B);

  if ((P = (INSN *)w->next_alt) == NULL) {           /* deep backtracking */
#if defined(DEBUG)
    if (debug_choicepoints)
      fprintf(stderr, "deep backtracking, node = %x\n", (int)w->node);
#endif
                                                  /* 7-8 contiguous moves */
    P = (INSN *)B->next_alt;
    w->frame = B->frame;
    w->next_insn = B->next_insn;
    RestoreLtop(B);

    /* Dirty hack: always pop n registers (heuristic measure, I guess) and
       see later if we need to reload more; had we needed less, we simply do
       not use the additional ones.  I have simplified the code (see below),
       in part because it was giving problems in some architectures: the
       initial choicepoint of a concurrent goal was being accessed out of
       the scope of the memory allocated for the choicepoint stack.  MCL. */


    /*
    X(0) = B->term[0];
    X(1) = B->term[1];
    X(2) = B->term[2];
    X(3) = B->term[3];
    i = ((struct try_node *)P)->node_offset;
    w->next_node = ChoiceCharOffset(B,-i);
    if (i>ArityToOffset(3)){
      S = (TAGGED *)w->next_node;
      i = OffsetToArity(i)-3;
      do
        (w->term+2)[i] = ChoiceNext(S);
      while (--i);
      }
    */


    i = ((struct try_node *)P)->node_offset;
    w->next_node = ChoiceCharOffset(B,-i);
    if (i>ArityToOffset(0)){
      TAGGED *wt = w->term;

      S = (TAGGED *)w->next_node;
      i = OffsetToArity(i) - 1;
#if defined(DEBUG)
      if (debug_choicepoints)
        fprintf(stderr, "Reloading %d words from node %x\n", 
                i, (int)w->node);
#endif
      while(i >= 0)
        wt[i--] = ChoiceNext(S);
    }
#if defined(PROFILE)
    count_retry=TRUE;
#endif
  }
#if defined(PROFILE)
  else
    count_retry=FALSE;
  PROFILE__HOOK_RETRY;
#endif

  if ((w->next_alt = ((struct try_node *)P)->next)==NULL) {
    w->node = SetB(w->next_node);
    SetShadowregs(B);
  }

  P = ((struct try_node *)P)->emul_p;
  t0 = X(0);
  if (!IsVar(t0))
    goto ReadMode;
  LoadH;  
  goto WriteMode;

			       /* ENTERING A PREDICATE:  H always live. */
 enter_predicate:          /* Take into account attributed variables !! */

  /* #if defined(ATTRVARS) */
  if (OffHeaptop(H,Heap_Warn_Soft)) {
    int wake_count;

    if (Stop_This_Goal(Arg)) 
      goto exit_toplevel; 

    wake_count = WakeCount;
    
    if (OffHeaptop(H+4*wake_count,Heap_Warn)) {
      SETUP_PENDING_CALL(address_true);
      StoreH;
      heap_overflow(Arg,CALLPAD+4*wake_count);
      LoadH;
    }
    if (wake_count>0) {
      if (wake_count==1) { 
        SETUP_PENDING_CALL(address_uvc);
        collect_one_pending_unification(Arg);         /* does not touch H */
        DEREF(t0,X(1)); 
        if ( TagIsCVA(t0) ) {
          X(1)=CTagToGoal(t0); 
          Setfunc(address_ucc);	        /* patch prev. SETUP_PENDING_CALL */
        }
      } else {
        SETUP_PENDING_CALL(address_pending_unifications);
        StoreH;
        collect_pending_unifications(Arg,wake_count);
        LoadH;
      }
    }
    if (OffStacktop(w->frame,Stack_Warn)){
      SETUP_PENDING_CALL(address_true);
      stack_overflow(Arg);
    }
    if (Int_Heap_Warn != (Heap_Warn_Soft = Heap_Warn)) {
      SETUP_PENDING_CALL(address_help);
      control_c_normal(Arg);
    }
  }

  /*  Code without attributed variables 
#else
  if (OffHeaptop(H,Heap_Warn_Soft)) {
    int wake_count = WakeCount;
    
    if (OffHeaptop(H+2*wake_count,Heap_Warn)) {
      SETUP_PENDING_CALL(address_true);
      StoreH;
      heap_overflow(Arg,CALLPAD+2*wake_count);
      LoadH;
    }
    if (wake_count>0) {
      SETUP_PENDING_CALL(address_apply);
      StoreH;
      collect_goals_from_trail(Arg,wake_count);
      LoadH;
    }
    if (OffStacktop(w->frame,Stack_Warn)) {
      SETUP_PENDING_CALL(address_true);
      stack_overflow(Arg);
    }
    
    if (Int_Heap_Warn != (Heap_Warn_Soft = Heap_Warn)) {
      SETUP_PENDING_CALL(address_help);
      control_c_normal(Arg);
    }
  }
#endif
*/

 switch_on_pred:
  i = Func->enter_instr;
 switch_on_pred_sub:
  switch (i) {
  case ENTER_FASTCODE_INDEXED:
  case ENTER_FASTCODE:
  case ENTER_UNDEFINED:
    PredTrace("U",Func);
    ptemp = (INSN *)address_undefined_goal;
    goto escape_to_p;
  case ENTER_INTERPRETED:
    PredTrace("I",Func);
    ptemp = (INSN *)address_interpret_c_goal;
    goto escape_to_p2;
  case ENTER_C:
    PredTrace("C",Func);
    StoreH;
    /* Changed by DCG to handle errors in Prolog */
    i = (*Func->code.cinfo)(Arg);
    if (Expanded_Worker) {
#if defined(DEBUG)
        printf("wam() detected worker expanded by C predicate\n");
#endif
	if (worker == NULL) {
	  printf("bug: invalid WAM expansion\n"); /* JFKK this is temp
                                                     sometimes wam is called
                                                     without gd */
	  abort();
	}
        worker->worker_registers = Arg = Expanded_Worker;
        Expanded_Worker = NULL;
    }
    if (i == FALSE)                                           /* i == 0 */
      goto fail;
    else if (i == TRUE)                                       /* i == 1 */
      goto proceed_r;
    else {                                                     /* Error */
      X(4) = Culprit;                /* Culprit arg.   */
      X(3) = MakeSmall(ErrArgNo);    /* Arg. number    */
      X(0) = MakeSmall(-i);          /* Error code     */
      X(1) = Func->printname;        /* Builtin name   */
      X(2) = MakeSmall(Func->arity); /* Builtin arity  */
      P = (INSN *)address_error;
      goto switch_on_pred;
    }
    
  case BUILTIN_TRUE:
    PredTrace("B",Func);
    goto proceed_w;
    
  case BUILTIN_FAIL:
    PredTrace("B",Func);
    goto fail;
    
  case BUILTIN_CURRENT_INSTANCE:
    PredTrace("B",Func);
    StoreH;
    ins = current_instance(Arg);
    if (!ins)
      goto fail;
    P = ins->emulcode;
    goto ReadMode;
    
  case BUILTIN_COMPILE_TERM:
    PredTrace("B",Func);
    StoreH;
    if (!compile_term(Arg, &new_worker)) goto fail;
    if (new_worker) {
      if (worker == NULL) {
	printf("bug: invalid WAM expansion\n"); /* JFKK this is temp
						   sometimes wam is called
						   without gd */
	abort();
      }
      worker->worker_registers = Arg = new_worker;
#if defined(DEBUG)
      fprintf(stderr, "Reallocation of wrb detected in wam()\n");
#endif
    }
    goto proceed_r;
    
  case BUILTIN_INSTANCE:
    /* ASSERT: X(2) is a dereferenced integer */
    PredTrace("B",Func);
    LoadHVA(X(3),H);
    ins = TagToInstance(X(2));
    P = ins->emulcode;
    goto WriteMode;
    
  case BUILTIN_GELER:
    PredTrace("B",Func);
    t1 = X(0);  DerefSwitch(t1,t0,;);
    t3 = X(1);  DerefSwitch(t3,t0,;);
    Setfunc(find_definition(predicates_location,t3,&w->structure,TRUE));
    goto suspend_t3_on_t1;
    
  case BUILTIN_NODEBUGCALL:
    PredTrace("B",Func);
    t0 = X(0);  DerefSwitch(t0,X(0),;);
    Setfunc(find_definition(predicates_location,t0,&w->structure,FALSE));
    if (Func==NULL)
      goto fail;
    i = Func->enter_instr;
    goto call4;
    
  case BUILTIN_SYSCALL:
    PredTrace("B",Func);
    t0 = X(0); 
    DerefSwitch(t0,X(0),;);
  call1:
    /*Setfunc(find_definition(&prolog_predicates,t0,&w->structure,FALSE));*/
    Setfunc(find_definition(predicates_location,t0,&w->structure,FALSE));
    if (Func==NULL){
      Setfunc(address_undefined_goal);
      goto switch_on_pred;
    }
    i = Func->enter_instr;
    goto call4;
      
/*
#if !defined(ATTRVARS)
    case BUILTIN_APPLY:
      PredTrace("B",Func);
      w->structure = TagToArg(X(0),1);
      Setfunc(TagToFunctor(X(1)));
      i = Func->enter_instr;
      goto call4;
#endif
*/

  case BUILTIN_CALL:
    PredTrace("B",Func);
    t0 = X(0); 
    DerefSwitch(t0,X(0),;);
    Setfunc(find_definition(predicates_location,t0,&w->structure,FALSE));
    if (Func==NULL){
      Setfunc(address_undefined_goal);
      goto switch_on_pred;
    }
    if (Current_Debugger_Mode != atom_off){
      Setfunc(address_trace);
      goto switch_on_pred;
    }
    i = Func->enter_instr;
  call4:
    switch (i){
    case ENTER_INTERPRETED:
      PredTrace("I",Func);
      X(1) = PointerToTerm(Func->code.intinfo);
      Setfunc(address_interpret_goal);
      goto switch_on_pred;
      
    case BUILTIN_DIF:
      PredTrace("B",Func);
      pt1 = w->structure;
      RefHeapNext(t0,pt1);
      DerefHeapSwitch(t0,t2,;);
      RefHeapNext(t1,pt1);
      DerefHeapSwitch(t1,t2,;);
      goto dif1;
      
    case SPYPOINT:
      if (!Func->properties.wait)
        goto call5;
      
    case WAITPOINT:
      RefHeap(t0,w->structure);
      DerefHeapSwitch(t0,t1,{t3 = X(0); goto suspend_t3_on_t1;});
    call5:
#if defined(DBG)
      if (Func->properties.breakp)
        i = BREAKPOINT;
      else
#endif
        i = Func->predtyp;
      goto call4;
      
    default:
      if ((t0 = Func->arity)) {
        StoreH;
        pt1 = w->term;
        pt2 = w->structure;
        do
          PushRefHeapNext(pt1,pt2)
            while (--t0);
        LoadH;
      }
      goto switch_on_pred_sub;
    }
    
  case BUILTIN_DIF:
    PredTrace("B",Func);
    t0=X(0); DerefSwitch(t0,t2,;);
    t1=X(1); DerefSwitch(t1,t2,;);
    w->structure = NULL;
    /* check fast cases first */
  dif1:
    if (t0==t1)
      goto fail;
    else if ((!IsVar(t0 & t1)) &&
             (IsAtomic(t0) || IsAtomic(t1)))
      goto proceed_w;
    else
      X(0)=t0, X(1)=t1;
    StoreH;
    if (!prolog_dif(Arg,Func))
      goto fail;
    goto proceed_r;
    
  case BUILTIN_ABORT:
    /* cut all the way and fail, leaving wam with a return code */
    PredTrace("B",Func);
    t0 = X(0); DerefSwitch(t0,t1,;);
    wam_exit_code = GetSmall(t0);
    w->next_node = InitialNode;

#if defined(PROFILE)
    if(profile) {
      printf("{NOTE: node stack:\n");
      prev_profile_trace = profile_trace;
      profile_trace = 1;
    }
#endif
    DOCUT;
#if defined(PROFILE)
    if(profile) {
      profile_trace = prev_profile_trace;
      printf("}\n");
    }
#endif
    goto fail;
    
  case SPYPOINT:
    if (Current_Debugger_Mode != atom_off){
      ptemp = (INSN *)address_trace;
      goto escape_to_p;
    }
    if (!Func->properties.wait)
      goto nowait;
    
  case WAITPOINT:
    t1 = X(0);
    DerefSwitch(t1,X(0),{goto suspend_on_t1;});
  nowait:
#if defined(DBG)
    if (Func->properties.breakp)
      goto breakpoint;
#endif
    i = Func->predtyp;
    goto switch_on_pred_sub;
    
  case BREAKPOINT:
#if defined(DBG)
  breakpoint:
  i_breakPred(Func);
#endif
  i = Func->predtyp;
  goto switch_on_pred_sub;
  
  case ENTER_COMPACTCODE_INDEXED:
  case ENTER_PROFILEDCODE_INDEXED:
    PredTrace("E",Func);
    t0 = X(0);
    DerefSwitch(t0,X(0),TRYEACH_W(Func->code.incoreinfo->varcase));
    
    StoreH;                                             /* non variable */
    if (t0 & TagBitComplex){
      if (t0 & TagBitFunctor)
        S = TagToArg(t0,0), t1 = HeapNext(S);
      else {
        S = TagToLST(t0);
        TRYEACH_R(Func->code.incoreinfo->lstcase);
      }
    } else t1 = t0;
    
    SetHtab(Func->code.incoreinfo->othercase);
    
    for (i=0, t2=t1, t1 &= Htab->mask;
         ;
         i+=sizeof(struct sw_on_key_node), t1=(t1+i) & Htab->mask){
      SetHtabNode(&Htab->tab.aschar[t1]);
      if (HtabNode->key==t2 || !HtabNode->key)
        TRYEACH_R(HtabNode->value.try_chain);
    }
    
  case ENTER_COMPACTCODE:
  case ENTER_PROFILEDCODE:
    PredTrace("E",Func);
    TRYEACH_W(Func->code.incoreinfo->varcase);
  }

				/* READ MODE DISPATCHER */

 tryeach_r:
#if defined(GAUGE)
  INCR_COUNTER(Alts->entry_counter+1);
#endif
  P = Alts->emul_p2;
  w->next_node = w->node;
  if ((w->next_alt=Alts->next)!=NULL) {
    SetB(w->node);
    ComputeLtop(B);
    w->node = SetB(ChoiceCharOffset(B,w->next_alt->node_offset));
    B->next_alt = NULL;
    B->trail_top = w->trail_top;
    SaveGtop(B,w->global_top);
    NewShadowregs(w->global_top);
#if defined(DEBUG)
    if (debug_choicepoints)
      fprintf(stderr, "WAM created choicepoint (r), node = %x\n", 
                       (int)w->node);
#endif    
    /* segfault patch -- jf */
    if (ChoiceYounger(ChoiceOffset(B,CHOICEPAD),w->trail_top))
      choice_overflow(Arg, CHOICEPAD);
  }

 ReadMode:			/* Here with H in memory. */

#if defined(DBG)
  if( --d_exec==0 )  {
    w->insn = P;
    w->structure = S;
    d_command();
    S = w->structure;
  }
#endif

  PROFILE__INCR_BYTECODE_COST_READ;
#if defined(APOLLO_CC_BUG)
  OPCODE;
  switch (*Pplus0)
#else
    switch (OPCODE)
#endif
      {
#include "wamread.c"
    default:
      goto illop;
    }

				/* WRITE MODE DISPATCHER */

 tryeach_w:
#if defined(GAUGE)
    INCR_COUNTER(Alts->entry_counter);
#endif
  P = Alts->emul_p;
  w->next_node = w->node;
  if ((w->next_alt=Alts->next)!=NULL) {
    SetB(w->node);
    ComputeLtop(B);
    w->node = SetB(ChoiceCharOffset(B,w->next_alt->node_offset));
    B->next_alt = NULL;
    B->trail_top = w->trail_top;
    SaveGtop(B,H);
    NewShadowregs(H);
#if defined(DEBUG)
    if (debug_choicepoints)
      fprintf(stderr, "WAM created choicepoint (w), node = %x\n",
                      (int)w->node);
#endif    
    /* segfault patch -- jf */
    if (ChoiceYounger(ChoiceOffset(B,CHOICEPAD),w->trail_top))
      choice_overflow(Arg, CHOICEPAD);
  }

 WriteMode:			/* Here with H in register. */

#if defined(DBG)
  if( --d_exec==0 ) {
    w->insn = P;
    w->structure = NULL;
    StoreH;
    d_command();
    LoadH;
  }
#endif

  PROFILE__INCR_BYTECODE_COST_WRITE;
#if defined(APOLLO_CC_BUG)
  OPCODE;
  switch (*Pplus0)
#else
  switch (OPCODE)
#endif
    {
#include "wamwrite.c"
  default:
    goto illop;
  }

 exit_toplevel:
  w->insn = P;
  /* What should we save here? */
  /* w->node = B; */                                       /* MCL */
  /* w->frame = E->frame; */                               /* MCL */
  if (worker && (worker->action & KEEP_STACKS)) {     /* We may backtrack */
    SAVE_WAM_STATE;
  }
  /* We may have been signaled and jumped here from enter_predicate: */
  if (Stop_This_Goal(Arg))
    wam_exit_code = WAM_INTERRUPTED;
#if defined(DEBUG)
  /* printf("Goal %x returning!\n", worker); */
#endif
  return wam_exit_code;

  illop:
  SERIOUS_FAULT("unimplemented WAM instruction");
}
