/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */

#include "threads.h"
#include "locks.h"
#include "datadefs.h"
#include "support.h"
#include "wam.h"
#include "predtyp.h"
#include "task_areas.h"

/* declarations for global functions accessed here */

#include "alloc_defs.h"
#include "tasks_defs.h"
#include "start_defs.h"
#include "misc_defs.h"
#include "bignum_defs.h"
#include "stacks_defs.h"
#include "nondet_defs.h"
#include "term_support_defs.h"
#include "timing_defs.h"
#include "profile_defs.h"
#if defined(INTERNAL_CALLING)
#include "wam_defs.h"
#endif

/* local declarations */

static int compare_aux(Argdecl, TAGGED x1, TAGGED x2);
static int compare_args_aux(Argdecl, register int arity, register TAGGED *pt1, register TAGGED *pt2, TAGGED *x1, TAGGED *x2);

/*
  $set_global_logic_var(Value, WhichOne)
*/

#if defined(GLOBVARS)
BOOL set_glv(Arg)
     Argdecl;
{
  int which_glv;
  
  DEREF(X(1), X(1));
  if (!IsInteger(X(1))) 
    ERROR_IN_ARG(X(1), 2, INTEGER);
  
  which_glv = GetInteger(X(1));
  if ((which_glv > MAXGLOBALVARS - 1) || which_glv < 0) 
    USAGE_FAULT("$set_global_logical_var: var number out of bounds");

  DEREF(X(0), X(0));
  GLOBVAR(which_glv) = X(0);
  return TRUE;
}
/*
  $get_global_logic_var(Value, WhichOne)
*/

BOOL get_glv(Arg)
     Argdecl;
{
  int which_glv;
  
  DEREF(X(1), X(1));
  if (!IsInteger(X(1))) 
    ERROR_IN_ARG(X(1), 2, INTEGER);
  
  which_glv = GetInteger(X(1));
  if ((which_glv > MAXGLOBALVARS - 1) || (which_glv < 0))
    USAGE_FAULT("$get_global_logical_var: var number out of bounds");

  DEREF(X(0), X(0));
  X(0) = GLOBVAR(which_glv);
  return TRUE;
}

#else
BOOL set_glv(Arg)
     Argdecl;
{
  USAGE_FAULT("global vars not working in this version")
    }  

BOOL get_glv(Arg)
     Argdecl;
{
  USAGE_FAULT("global vars not working in this version")
    }  
#endif

#if defined(USE_GLOBAL_VARS)
BOOL prolog_global_vars_set_root(Argdecl) {
  /* Warning: program must not backtrack after executing this code */
  DEREF(GLOBAL_VARS_ROOT,X(0));
  return TRUE;
}
BOOL prolog_global_vars_get_root(Argdecl) {
  DEREF(X(0),X(0));
  return cunify(Arg, GLOBAL_VARS_ROOT, X(0));
}
#else
BOOL prolog_global_vars_set_root(Argdecl) {
  USAGE_FAULT("This engine was not compiled with global variables support (USE_GLOBAL_VARS)");
}
BOOL prolog_global_vars_get_root(Argdecl) {
  USAGE_FAULT("This engine was not compiled with global variables support (USE_GLOBAL_VARS)");
}
#endif

/*
extern BOOL bn_positive();
extern int bn_compare();
extern BOOL prolog_init_radix();
extern int find_constraints();

static int compare_args_aux PROTO((struct worker *w, int arity, TAGGED *pt1, TAGGED *pt2, TAGGED *x1, TAGGED *x2));
static int compare_aux PROTO((struct worker *w,TAGGED x1, TAGGED x2));
*/

/* help function for builtin compare/3
 * returns -1 if u @< v
 * returns  0 if u == v
 * returns +1 if u @> v
 */
int compare_help(Arg,x1,x2)
     Argdecl;
     TAGGED x1, x2;
{
  int result = compare_aux(Arg,x1,x2);
  REGISTER int i = w->value_trail;

  if (i<InitialValueTrail)
    {
      REGISTER TAGGED *pt1, *pt2;
      
      pt2 = (TAGGED *)w->node;
      do
	{
	  pt1 = (TAGGED *)pt2[i++];
	  *pt1 = pt2[i++];
	}
      while (i<InitialValueTrail);
      w->value_trail = InitialValueTrail;
    }

  return result;
}


static int compare_aux(Arg,x1,x2)
     Argdecl;
     TAGGED x1, x2;
{
  REGISTER TAGGED u, v, t1;
  REGISTER TAGGED *pt1, *pt2;
  int i, j, urank, vrank;	/* FLO=1, INT=2, ATM=3, COMPLEX=4 */

 in:
  u=x1, v=x2;
  DerefSwitch(u,t1,goto var_x;);
  DerefSwitch(v,t1,return 1;);
  if (u==v) return 0;
  if (TagIsSmall(u) && TagIsSmall(v))
    goto var_var;
  if (u & TagBitComplex)
    {
      if (u & TagBitFunctor)
	t1 = TagToHeadfunctor(u),
        /*	urank = (!(t1&TagBitFunctor) ? 2 : t1&QMask ? 1 : 4);*/
	urank = (!(t1&TagBitFunctor) ? 1 : t1&QMask ? 2 : 4);
      else
	urank = 4;
    }
  else
    /*    urank = (u&TagBitFunctor ? 3 : 1);*/
    urank = (u&TagBitFunctor ? 3 : 2);
  if (v & TagBitComplex)
    {
      if (v & TagBitFunctor)
	t1 = TagToHeadfunctor(v),
        /*	vrank = (!(t1&TagBitFunctor) ? 2 : t1&QMask ? 1 : 4);*/
	vrank = (!(t1&TagBitFunctor) ? 1 : t1&QMask ? 2 : 4);
      else
	vrank = 4;
    }
  else
    /*    vrank = (v&TagBitFunctor ? 3 : 1);*/
    vrank = (v&TagBitFunctor ? 3 : 2);

  if (urank<vrank) return -1;
  if (urank>vrank) return 1;
  switch (urank)
    {
    case 1:			/* FLO, FLO */
      {
	ENG_FLT f1 = GetFloat(u);
	ENG_FLT f2 = GetFloat(v);

	if (f1<f2) return -1;
	if (f1>f2) return 1;
	{
	  REGISTER unsigned int *i1 = (unsigned int *)(&f1);
	  REGISTER unsigned int *i2 = (unsigned int *)(&f2);

	  f1 = -f1;
	  f2 = -f2;
	  if (*i1 == *i2)
	    i1++, i2++;

	  return (*i1 < *i2 ? -1 : *i1 > *i2 ? 1 : 0);
	}
      }
    case 2:			/* INT, INT */
      {
	if (TagIsSmall(u)&&TagIsSmall(v))
	  return (u<v ? -1 : u>v); 
	else if (TagIsSmall(u))
	  return (bn_positive(TagToSTR(v)) ? -1 : 1);
	else if (TagIsSmall(v))
	  return (bn_positive(TagToSTR(u)) ? 1 : -1);
	else
	  return bn_compare(TagToSTR(u),TagToSTR(v));
      }
    case 3:			/* ATM, ATM */
      break;
    case 4:			/* COMPLEX, COMPLEX */
      if (u & TagBitFunctor)
	pt1 = TagToSTR(u), u = *pt1++, i = Arity(u);
      else
	pt1 = TagToLST(u), u = functor_list, i = 2;
      if (v & TagBitFunctor)
	pt2 = TagToSTR(v), v = *pt2++, j = Arity(v);
      else
	pt2 = TagToLST(v), v = functor_list, j = 2;
      
      if (u==v)
	{
	  int result = compare_args_aux(Arg,i,pt1,pt2,&x1,&x2);
	  
	  if (result) return result;
	  goto in;
	}
      else if (i!=j)
	return (i<j ? -1 : 1);
      else
	break;
    }

				/* UNSIGNED strcmp */
  {
    REGISTER unsigned char *up = (unsigned char *)GetString(u);
    REGISTER unsigned char *vp = (unsigned char *)GetString(v);

    while ((u = *up++) && (v = *vp++))
      if (u!=v) return (u<v ? -1 : 1);
    return (u ? 1 : v ? -1 : 0);
  }

 var_x:
  DerefSwitch(v,t1,goto var_var;);
  return -1;
 var_var:
  return (u<v ? -1 : u>v ? 1 : 0);
}


static int compare_args_aux(Arg,arity,pt1,pt2,x1,x2)
     Argdecl;
     REGISTER int arity;
     REGISTER TAGGED *pt1, *pt2;
     TAGGED *x1, *x2;
{
  int result;
  REGISTER TAGGED 
    t1 = ~0, t2 = ~0,  /* Avoid compiler complaints */
    t3;
  
  /* Adapted from terminating unification of complex structures:
     See cunify_args(). */
  
  if (ChoiceYounger(ChoiceOffset(w->node,2*CHOICEPAD-w->value_trail),w->trail_top))
				/* really: < 2*arity */
    choice_overflow(Arg,2*CHOICEPAD);
  for (result=0; !result && arity>0; --arity) {
    t1 = *pt1;
    t2 = *pt2;
    if (t1 != t2) {
      DerefHeapSwitch(t1,t3,goto noforward;);
      DerefHeapSwitch(t2,t3,goto noforward;);
      if (t1!=t2 && IsComplex(t1&t2)) {
        /* replace smaller value by larger value,
           using choice stack as value trail */
        REGISTER TAGGED *b = (TAGGED *)w->node;
        REGISTER int i = w->value_trail;
        
        if (t1>t2)
          b[--i] = *pt1,
            b[--i] = (TAGGED)pt1,
            *pt1 = t2;
        else
          b[--i] = *pt2,
            b[--i] = (TAGGED)pt2,
            *pt2 = t1;
        w->value_trail = i;
      }
    noforward:
      if (arity>1 && t1!=t2)
        result = compare_aux(Arg,t1,t2);
    }
    (void)HeapNext(pt1);
    (void)HeapNext(pt2);
  }
  
  if (!result) *x1 = t1, *x2 = t2;
  
  if (ChoiceYounger(ChoiceOffset(w->node,CHOICEPAD-w->value_trail),w->trail_top))
    choice_overflow(Arg,CHOICEPAD);
  
  return result;
}


/* ---------------------------------------------------------------- */
/*         BUILTIN C PREDICATES                                     */
/* ---------------------------------------------------------------- */
extern char source_path[];

#if defined(INTERNAL_CALLING)
BOOL prolog_internal_call(Arg)
     Argdecl;
{
  
  INSN *next_insn;

  printf("In current_executable, internal_calling is %lx\n", 
         (long unsigned int)address_internal_call);  
  next_insn = Arg->next_insn;
  Arg->next_insn = internal_calling;
  wam(Arg, NULL);
  Arg->next_insn = next_insn;
  return TRUE;
}
#endif

BOOL prolog_current_executable(Arg)
     Argdecl;
{
  DEREF(X(0),X(0));
  return cunify(Arg, MakeString(source_path), X(0));
}


BOOL prompt(Arg)
     Argdecl;
{
  Unify_constant(current_prompt,X(0));
  DEREF(current_prompt,X(1)); 
  return TRUE;
}

BOOL unknown(Arg)
     Argdecl;
{
  Unify_constant(current_unknown,X(0));
  DEREF(current_unknown,X(1)); 
  return TRUE;
}


BOOL metachoice(Arg)
     Argdecl;
{
  Unify_constant(ChoiceToInt(w->node),X(0));
  return TRUE;
}


BOOL metacut(Arg)
     Argdecl;
{
  DEREF(X(0),X(0));
  w->node = ChoiceFromInt(X(0));
  SetShadowregs(w->node);
  /*  ConcChptCleanUp(TopConcChpt, w->node);*/
  PROFILE__HOOK_METACUT;
  return TRUE;

}


BOOL retry_cut(Arg)
     Argdecl;
{
  TAGGED number;
  REGISTER struct node *nd;

  DEREF(X(0),X(0));
  if (!TagIsSmall(X(0)))
    return FALSE;
  for (nd = w->node;
       ChoiceYounger(nd,Choice_Start);
       nd = ChoiceCharOffset(nd,-nd->next_alt->node_offset))
    {
      DEREF(number,nd->term[0]);
      if (nd->term[3]==atom_retry_hook && number<=X(0))
	{
	  nd->term[1] = X(1);	/* always dereferenced */
	  w->node = nd;
	  SetShadowregs(nd);
	  break;
	}
    }
  return ChoiceYounger(nd,Choice_Start);
}



/* $setarg(+I, +Term, +Newarg, +Mode):
 * Replace (destructively) I:th arg of Term by Newarg.
 * Mode=on -> dereference, undo on backtracking;
 * Mode=off -> we're undoing now, don't dereference;
 * Mode=true -> dereference, don't undo later.
 *
 * Put in at the express request of Seif Haridi.
 */
BOOL setarg(Arg)
     Argdecl;
{
  REGISTER TAGGED t1, t2, *ptr;
  TAGGED oldarg, number, complex, newarg, *x;
  
  number = X(0);
  complex = X(1);
  newarg = X(2);
  DEREF(X(3),X(3));
  
  if (X(3) != atom_off) {
    DerefSwitch(number,t1,{goto barf1;});
    DerefSwitch(complex,t1,{goto barf2;});
    DerefSwitch(newarg,t1,{goto unsafe_value;});
  } else {
  unsafe_value:
    if (TagIsSVA(newarg)){
      ptr = w->global_top;
      LoadHVA(t1,ptr);
      w->global_top = ptr;
      BindSVA(newarg,t1);
      newarg = t1;
    }
  }
  
  if (TagIsSTR(complex)) {
    REGISTER int i = GetSmall(number);
    REGISTER TAGGED f = TagToHeadfunctor(complex);
    
    if (i<=0 || i>Arity(f) || f&QMask)
      goto barf1;
    
    ptr = TagToArg(complex,i);
  } else if (IsComplex(complex)){	/* i.e. list */
    if (number==MakeSmall(1))
      ptr = TagToCar(complex);
    else if (number==MakeSmall(2))
      ptr = TagToCdr(complex);
    else
      goto barf1;
  } else goto barf2;
  
  RefHeap(oldarg,ptr);
  *ptr = newarg;
  
  if ((X(3)==atom_on) && CondHVA(TagHVA(ptr))) {
    /* undo setarg upon backtracking */
    TAGGED *limit = TagToPointer(w->node->trail_top);
    
    /* check first if location already trailed is same segment */
    t1 = TagHVA(ptr);
    
    for (x=w->trail_top; TrailYounger(x,limit);) {
      t2 = TrailPop(x);
      if (t1 == t2)
        return TRUE;
    }
    
    ptr = w->global_top;
    t2 = Tag(STR,ptr);
    HeapPush(ptr,functor_Dsetarg);
    HeapPush(ptr,number);
    HeapPush(ptr,complex);
    HeapPush(ptr,oldarg);
    HeapPush(ptr,atom_off);
    TrailPush(w->trail_top,t2);
    w->global_top = ptr;
    
    /* trail smashed location for segmented GC */
    TrailPush(w->trail_top,t1);
    
    if (ChoiceYounger(ChoiceOffset(w->node,CHOICEPAD),w->trail_top))
      choice_overflow(Arg,CHOICEPAD);
  }
  
  return TRUE;
  
 barf1:
  MINOR_FAULT("setarg/3: incorrect 1st argument");
  
 barf2:
  MINOR_FAULT("setarg/3: incorrect 2nd argument");
}

BOOL undo(Arg)
     Argdecl;
{
  REGISTER TAGGED goal, t1;
  
  goal = X(0);
  DerefSwitch(goal,t1,{MINOR_FAULT("$undo/1: invalid argument");});
  TrailPush(w->trail_top,goal);
  if (ChoiceYounger(ChoiceOffset(w->node,CHOICEPAD),w->trail_top))
    choice_overflow(Arg,CHOICEPAD);
  return TRUE;
}

BOOL frozen(Arg)		/* x1 = constraints on x0,
				   or '[]' if there are none */
     Argdecl;
{
  DEREF(X(0),X(0));
  if (!IsVar(X(0)))
    return FALSE;
  else if (VarIsCVA(X(0)))
    return cunify(Arg,Tag(LST,TagToGoal(X(0))),X(1));
  Unify_constant(atom_nil,X(1));
  return TRUE;
}

BOOL defrost(Arg)
     Argdecl;
{
  TAGGED t;
  TAGGED *h = w->global_top;
  
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  if (X(1)==atom_nil)
    {
      LoadHVA(t,h);
    }
  else
    {
      LoadCVA(t,h);
      HeapPush(h,CTagToCar(X(1)));
      HeapPush(h,CTagToCdr(X(1)));
    }
  BindCVA(X(0),t);
  w->global_top = h;
  return TRUE;
}

BOOL debugger_state(Arg)
     Argdecl;
{
#if defined(DEBUG)
  if (debug_gc)
    fprintf(stderr,
            "Thread %d is in debbuger_state\n", (int)Thread_Id);

  /*
  DEREF(X(0), X(0));
  if (IsVar(X(0)))
    fprintf(stderr, "First Arg. is Var\n");
  else 
    wr_functor("First Arg. is ", X(0));

  DEREF(X(1), X(1));
  if (IsVar(X(1)))
    fprintf(stderr, "Second Arg. is Var\n");
  else 
    wr_functor("Second Arg. is ", X(1));
  */
#endif

  if (!cunify(Arg,Current_Debugger_State,X(0)))
    return FALSE;
  DEREF(Current_Debugger_State,X(1));
  return TRUE;
}

BOOL debugger_mode(Arg)
     Argdecl;
{
#if defined(DEBUG)
  if (debug_gc)
    fprintf(stderr, "Thread %d is changing debbuger mode\n", (int)Thread_Id);
#endif

  if (TagIsSTR(Current_Debugger_State)) {
#if defined(DEBUG)
  if (debug_gc)  fprintf(stderr, "Current_Debugger_State is structure\n");
#endif
    RefArg(Current_Debugger_Mode,Current_Debugger_State,2);
    if (Current_Debugger_Mode != atom_off)
/*
  #if !defined(ATTRVARS)
  address_apply = address_slow_apply,
  #endif
*/
      address_interpret_c_goal = address_interpret_compiled_goal;
    else
/*
  #if !defined(ATTRVARS)
  address_apply = address_fast_apply,
  #endif
*/
      address_interpret_c_goal = address_interpret_goal;
  } else {
    Current_Debugger_Mode = atom_off;
/*
  #if !defined(ATTRVARS)
  address_apply = address_fast_apply;
  #endif
*/
    address_interpret_c_goal = address_interpret_goal;
  }
  return TRUE;
}

/****
void debugger_trap()
{
  if (TagIsSTR(current_debugger_state))
  {
      CTagToArg(current_debugger_state,2) = atom_trace;
      CTagToArg(current_debugger_state,3) = MakeSmall(1000000);
      current_debugger_mode = atom_trace;
      address_apply = address_slow_apply;
      address_interpret_c_goal = address_interpret_compiled_goal;
  }
}
****/



/*
BOOL leash_mode(Arg)
     Argdecl;
{
  Unify_constant(current_leash_mode,X(0));
  DEREF(current_leash_mode,X(1)); 
  return TRUE;
}

BOOL maxdepth(Arg)
     Argdecl;
{
  Unify_constant(current_maxdepth,X(0));
  DEREF(current_maxdepth,X(1)); 
  return TRUE;
}

BOOL printdepth(Arg)
     Argdecl;
{
  Unify_constant(current_printdepth,X(0));
  DEREF(current_printdepth,X(1)); 
  return TRUE;
}

BOOL breaklevel(Arg)
     Argdecl;
{
  Unify_constant(current_breaklevel,X(0));
  DEREF(X(1),X(1));
  current_breaklevel += X(1)-TaggedZero;
  return TRUE;
}
*/

BOOL compiling(Arg)
     Argdecl;
{
  Unify_constant(current_compiling,X(0));
  DEREF(X(1),X(1));
  if (
      X(1)!=atom_unprofiled 
#if defined(GAUGE)
      && X(1)!=atom_profiled 
#endif
      )
    return FALSE;

  current_compiling = X(1);
  return TRUE;
}

BOOL ferror_flag(Arg)
     Argdecl;
{
  Unify_constant(current_ferror_flag,X(0));
  DEREF(current_ferror_flag,X(1)); 
  return TRUE;
}

/*
BOOL single_var_flag(Arg)
     Argdecl;
{
  Unify_constant(current_single_var_flag,X(0));
  DEREF(current_single_var_flag,X(1)); 
  return TRUE;
}

BOOL character_escapes_flag(Arg)
     Argdecl;
{
  Unify_constant(current_character_escapes_flag,X(0));
  DEREF(current_character_escapes_flag,X(1)); 
  return TRUE;
}

BOOL redefine_flag(Arg)
     Argdecl;
{
  Unify_constant(current_redefine_flag,X(0));
  DEREF(current_redefine_flag,X(1)); 
  return TRUE;
}
*/

BOOL quiet_flag(Arg)
     Argdecl;
{
  Unify_constant(current_quiet_flag,X(0));
  DEREF(current_quiet_flag,X(1)); 
  return TRUE;
}

BOOL spypoint(Arg)
     Argdecl;
{
  TAGGED *junk;
  struct definition *func;
  
  DEREF(X(0),X(0));
  func = find_definition(predicates_location,X(0),&junk,FALSE);
  if (!func  /* || func->properties.public */)
    return FALSE;
  if (func->properties.spy)
    Unify_constant(atom_on,X(1))
  else
    Unify_constant(atom_off,X(1));

  DEREF(X(2),X(2));
  func->properties.spy = (X(2)==atom_on);

  SetEnterInstr(func,func->predtyp);
  return TRUE;
}

BOOL prolog_radix(Arg)
     Argdecl;
{
  Unify_constant(current_radix,X(0));
  DEREF(current_radix,X(1));
  prolog_init_radix();
  return TRUE;
}

BOOL constraint_list(Arg)
     Argdecl;
{
  int pad = HeapDifference(w->global_top,Heap_End);
  
  REGISTER TAGGED *h;
  TAGGED l, v, clist;
  
  DEREF(X(0),X(0));
  while ((find_constraints(Arg, TagToPointer(X(0)))<<1)+CONTPAD > pad)
    {
      l = *w->trail_top;
      while (l!=atom_nil)
	v = l,
	l = CTagToCVA(v),
	CTagToCVA(v) = v;

      explicit_heap_overflow(Arg,pad<<=1,2);
    }
  h = w->global_top;
  l = *w->trail_top;
  clist = atom_nil;
  while (l!=atom_nil)
    v = l,
    l = CTagToCVA(v),
    CTagToCVA(v) = v,
    HeapPush(h,v),
    HeapPush(h,clist),
    clist = Tag(LST,HeapOffset(h,-2));
  w->global_top = h;
  return cunify(Arg,clist,X(1));
}

int find_constraints(Arg,limit)
     Argdecl;
     TAGGED *limit;
{
  REGISTER struct node *purecp; /* oldest CVA-free cp */
  REGISTER struct node *cp;
  int found = 0;
  
  cp = purecp = ChoiceCharOffset(w->node,ArityToOffset(0));
  cp->next_alt = fail_alt;
  cp->trail_top = w->trail_top;
  cp->global_top = w->global_top;
  *w->trail_top = atom_nil;
  while (limit<cp->global_top)
    {
      REGISTER struct node *prevcp =
	ChoiceCharOffset(cp,-cp->next_alt->node_offset);
      
      if (1 /* !ChoiceptTestNoCVA(cp)*/)
	{
	  REGISTER TAGGED *h = prevcp->global_top; 

	  if (h<limit) h = limit;
	  while (h<cp->global_top)
	    {
	      TAGGED v = *h++;
	      
	      if (v&QMask) h += LargeArity(v);
	      else if (TagIsATM(v)) h += Arity(v);
	      else if (v==Tag(CVA,h-1))
		{
		  h[-1] = *w->trail_top;
		  *w->trail_top = v;
		  found++;
		  h += 2;
		  purecp = prevcp;
		}
	    }
	  /* Christian Holzbaur pointed out that this is unsafe, e.g.
             | ?- dif(X,1), (true; fail), (X=2; frozen(X,Fr)).
	  if (purecp!=prevcp && limit<=prevcp->global_top)
	    ChoiceptMarkNoCVA(cp); */
	}
      
      cp = prevcp;
    }
  
  return found;
}

/* support for circularity checks:
   $eq(X,Y) :- X==Y, occupies_same_location(X,Y).
 */
BOOL prolog_eq(Arg)
     Argdecl;
{
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  
  return (X(0)==X(1));
}

/* Support function for dif/2.
   Fast cases have already been tested in wam().
   X(0) and X(1) are dereferenced.
   w->structure-1  points at an existing goal if non-NULL.
*/
BOOL prolog_dif(Arg,address_dif)
     Argdecl;
     struct definition *address_dif;
{
  REGISTER struct node *b;
  REGISTER TAGGED t0, t1, t2, *pt1, *pt2;
  REGISTER int i;
  TAGGED item, other;
				/* avoid stack variables */
  if (!w->structure)
    {
      if (TagIsSVA(t0=X(0)))
	{
	  LoadHVA(X(0),w->global_top);
	  BindSVA(t0,X(0));
	}
      if (TagIsSVA(t0=X(1)))
	{
	  LoadHVA(X(1),w->global_top);
	  BindSVA(t0,X(1));
	}
    }
				/* establish skeletal choicepoint */
  b = w->node;
  w->next_alt = address_nd_repeat; /* arity=0 */
  ComputeA(w->local_top,b);
  w->node = b = ChoiceCharOffset(b,ArityToOffset(0));
  b->next_alt = NULL;
  b->trail_top = w->trail_top;
  SaveGtop(b,w->global_top);
  NewShadowregs(w->global_top);
  
  if (cunify(Arg,X(0),X(1))) /* this could use AB, HB, TR, B. */
    item = atom_equal,
    other = TagHVA(w->global_top);
  else
    item = other = atom_lessthan;
  
  /* quasi failure */
  
  Heap_Warn_Soft = Int_Heap_Warn;
  b = w->node;
  t2 = (TAGGED)TagToPointer(b->trail_top);
  if (TrailYounger(pt1=w->trail_top, t2))
    {
      do
	{
	  if (IsVar(other))
	    {
	      item = pt1[-1];	/* variable */
	      other = CTagToPointer(item);
	    }
	  PlainUntrail(pt1,t0,;);
	}
      while (TrailYounger(pt1, t2));
      w->trail_top = pt1;
    }
  
  RestoreGtop(b);
  w->node = b = ChoiceCharOffset(b,-ArityToOffset(0));
  w->next_alt = NULL;
  SetShadowregs(b);

				/* succeed, fail, or suspend */
  if (item==atom_lessthan)
    return TRUE;
  else if (item==atom_equal)
    return FALSE;
  

				/* construct goal on the heap */
  pt2 = w->global_top;
  if (w->structure)
    X(2) = Tag(STR,w->structure-1);
  else
    {
      X(2) = Tag(STR,pt2);
      HeapPush(pt2,SetArity(address_dif->printname,2));
      HeapPush(pt2,X(0));
      HeapPush(pt2,X(1));
    }


				/* constrain pivot variable(s) */
  for (i=0, t1=item; i<2; i++, t1=other)
    {
      if (IsVar(t1))
	  {
	    if (TagIsHVA(t1))
	      {
		LoadCVA(t0,pt2);
		if (CondHVA(t1))
		  {
		    TrailPush(pt1,t1);
		    BindingOfHVA(t1) = t0;
		  }
		else
		  CTagToHVA(t1) = t0;
		goto check_trail;
	      }
	    else if (!CondCVA(t1))
	      {
		HeapPush(pt2,CTagToGoal(t1));
		HeapPush(pt2,CTagToDef(t1));
		CTagToGoal(t1) = Tag(LST,HeapOffset(pt2,-2));
		CTagToDef(t1) = Tag(LST,pt2);
	      }
	    else
	      {
		LoadCVA(t0,pt2);
		HeapPush(pt2,Tag(LST,TagToGoal(t1)));
		HeapPush(pt2,Tag(LST,HeapOffset(pt2,1)));
		TrailPush(pt1,t1);
		BindingOfCVA(t1) = t0;
	      check_trail:
		if (ChoiceYounger(w->node,TrailOffset(pt1,CHOICEPAD)))
		  w->trail_top = pt1,
		  choice_overflow(Arg,CHOICEPAD),
		  pt1 = w->trail_top;
	      }
	    HeapPush(pt2,X(2));
	    HeapPush(pt2,PointerToTerm(address_dif));
	  }
    }
  w->global_top = pt2;
  w->trail_top = pt1;
  
  return TRUE;
}

BOOL large_data(Arg)
     Argdecl;
{
  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  if (!TagIsLarge(X(1)))
    return FALSE;
  return cunify(Arg,MakeInteger(Arg,(int)CTagToArg(X(1),GetSmall(X(0)))),X(2));
}


struct int_info *current_clauses_aux PROTO((TAGGED head));

BOOL prolog_interpreted_clause(Arg)
     Argdecl;
{
  TAGGED Head, Body, t;
  /*TAGGED ListHB;*/
  struct instance *object;
  struct int_info *root;
  struct worker *new_worker ;

  DEREF(t, X(1));

  if (TagIsSTR(t) && (TagToHeadfunctor(t) == functor_neck)) {
    DerefArg(Head,t,1);
    DerefArg(Body,t,2);    
    
    new_worker = NULL;
    object = compile_term_aux(Arg, Head, Body, &new_worker);
#if defined(DEBUG)
      if (new_worker)
        fprintf(stderr, "wrb reallocation in prolog_interpreted_clause()\n");
#endif
    if ((root = current_clauses_aux(Head)) == NULL) 
      MAJOR_FAULT("Root == NULL @ c_interpreted_clause!!!");

    return insertz_aux(root, object);

  } else return FALSE;
}

struct int_info *current_clauses_aux(head)
     TAGGED head;
{
  if (!IsVar(head)) {
    TAGGED *junk;
    struct definition *d = 
      find_definition(predicates_location, head, &junk, FALSE);
    
    if ((d!=NULL) && (d->predtyp==ENTER_INTERPRETED))
      return d->code.intinfo;
  }
  return NULL; 
}


BOOL insertz_aux(root, n)
     struct int_info *root;
     struct instance *n;
{
    REGISTER struct instance **loc;
    ENG_INT current_mem = total_mem_count;
    
    if (!root->first) {
      n->rank = TaggedZero;
      n->backward = n;
      root->first = n;
    }
    else if (root->first->backward->rank == TaggedHigh)
      SERIOUS_FAULT("database node full in assert or record")
    else {
      n->rank = root->first->backward->rank+4;
      n->backward = root->first->backward;
      root->first->backward->forward = n;
      root->first->backward = n;
    }

    n->root = root;
    n->birth = use_clock = def_clock;
    n->death = 0xffff;
    n->forward = NULL;
    n->next_forward = NULL;

    loc = (n->key==ERRORTAG ? &root->varcase :
	   n->key==functor_list ? &root->lstcase :
	   &dyn_puthash(&root->indexer,n->key)->value.instp);
    
    if (!(*loc)){
      n->next_backward = n;
      (*loc) = n;
    } else {
      n->next_backward = (*loc)->next_backward;
      (*loc)->next_backward->next_forward = n;
      (*loc)->next_backward = n;
    }
    
    INC_MEM_PROG((total_mem_count - current_mem));
    return TRUE;
}



#if defined(ATOMGC)
BOOL prolog_erase_atom(Arg)
     Argdecl;
{
  long int index;

  DEREF(X(0), X(0));
  index = IndexPart(X(0));

#if defined(DEBUG)
  /*  printf("erasing atom %s at %ld\n", atmtab[index]->value.atomp->name, index);*/
#endif

/* atmtab[i] point to parts of other data structure, so we fix the values
   there and then set a null pointer in atmtab[] */

/* 1 cannot be the key of any entry (see init_atom_check()), and is used to
   mark a deleted entry (see atom_gethash()) */

  atmtab[index]->key = 1;
  atmtab[index]->value.atomp = NULL;
  atmtab[index] = NULL;
  prolog_atoms->count--;
  return TRUE;
}
#endif



/* 
   Support for generating new atoms with "funny names", always different.
   Make sure that the generation works OK with concurrency.  */

/* This seems to be the right size: one character less, and time (at large)
   doubles; one character more, and comparison in the symbol table takes
   longer. */
#define NEW_ATOM_LEN 13
#define NUM_OF_CHARS 62
static char allowed_char_table[NUM_OF_CHARS + 1] =
"0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
static char new_atom_str[] = "!!!!!!!!!!!!!";
#define FIRST_CHAR 0
#define LAST_CHAR  (NUM_OF_CHARS-1)

unsigned int x = 13*17;

BOOL prolog_new_atom(Arg)
     Argdecl;
{
  int i;
  int previous_atoms_count;
  TAGGED new_atom;

  DEREF(X(0), X(0));
  if (!IsVar(X(0)))
    ERROR_IN_ARG(X(0), 1, VARIABLE);

  Wait_Acquire_slock(atom_id_l);

  previous_atoms_count = prolog_atoms->count;
  do {
    for (i = 0; i < NEW_ATOM_LEN; i++) {
      x = (((new_atom_str[i] + x - FIRST_CHAR) * 13) + 300031);
      new_atom_str[i] = allowed_char_table[(x % NUM_OF_CHARS) + FIRST_CHAR];
      x = x / NUM_OF_CHARS;
    }
    new_atom = init_atom_check(new_atom_str);
    /* Make sure no smart guy already inserted the atom we have in mind */
  } while(prolog_atoms->count == previous_atoms_count);

  Release_slock(atom_id_l);
  return cunify(Arg, X(0), new_atom);
}


