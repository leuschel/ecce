/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */

#include "threads.h"
#include "datadefs.h"
#include "support.h"
#include "predtyp.h"
#include "task_areas.h"
#include "profile_defs.h"

#include <assert.h>

/* declarations for global functions accessed here */

#include "builtin_defs.h"
#include "wam_defs.h"
#include "inout_defs.h"

/* local declarations */

#if defined(DBG) || defined(DEBUG)
static TAGGED safe_deref(TAGGED t);
#endif

BOOL set_predtrace(Arg)
     Argdecl;
{
  TAGGED x;
  DEREF(x,X(0));
  if (!TagIsSmall(x))
    return FALSE;
  predtrace = (BOOL)GetSmall(x);
#if defined(PROFILE)
  if (profile||predtrace) stop_on_pred_calls = TRUE;
#else
  if (predtrace) stop_on_pred_calls = TRUE;
#endif
  return TRUE;
}


/* run_determ_c(goal) runs the goal and returns returns TRUE
   if goal is defined as a c predicate, even if that predicate fails.
   Otherwise it returns false.
   */

extern INSN *bootcode;                                          /* Shared */
#if defined(INTERNAL_CALLING)
extern INSN *internal_calling;                                          /* Shared */
#endif
extern INSN *startgoalcode;                                          /* Shared */
extern INSN *startgoalcode_cont;                                     /* Shared */



/*
myDEREF(Xderef,X) 
     TAGGED Xderef, X;
{ 
  REGISTER TAGGED m_i, m_j; 
  TAGGED aux1, aux2, aux3;
  TAGGED *aux4;
  m_i = X; 

  if (IsVar(m_i)) 
    do {
      aux1 = (TAGGED)(m_i) & POINTERMASK;
      aux2 = aux1 + MallocBase;
      aux4 = (TAGGED *)aux2;
      aux3 = *aux4;
      m_i = aux3;
      if (m_i == m_j)
	break;
    }
    while (IsVar(m_i=m_j)); 

  Xderef = m_i; 
}
*/

BOOL run_determ_c(Arg,goal)
     Argdecl;
     TAGGED goal;
{
  struct definition *func;
  REGISTER int i;
  REGISTER TAGGED *s;

/*
  prolog_display(Arg);
  printf("\n");
  */
  

  DEREF(goal,goal);
  /*myDEREF(goal,goal);*//* Was for debugging */
  /*func = find_definition(&prolog_predicates,goal,&w->structure,FALSE);*/
  func = find_definition(predicates_location,goal,&w->structure,FALSE);

  if (func==NULL) return FALSE;

  if (func->enter_instr == ENTER_C){
  
    for (i=func->arity, s=w->structure; --i>=0;)
      RefHeap(w->term[i],HeapOffset(s,i));
    
#if 0                                                        /* was GAUGE */
    return (*func->code.cinfo->procedure)(Arg);
#else
    return (*func->code.cinfo)(Arg);
#endif
  } else
    if (func->enter_instr == BUILTIN_CALL){
      w->next_insn = bootcode;          /* Should have been initialized */
      wam(w, NULL);
      return TRUE;
    }
  return FALSE;
}


/* CALLQ|call/1|goal=X(0)|exit_toplevel */

#if defined(DBG) || defined(DEBUG)
static TAGGED safe_deref(t)
     TAGGED t;
{
   REGISTER TAGGED aux;
   
   DerefSwitch(t,aux,;);
 
   return (t & ~3);
}


void wr_tagged(Arg,t)
     Argdecl;
     TAGGED t;
{
  wr_tagged_rec(Arg,t);
  putchar('\n');
}


void wr_tagged_rec(Arg,t)
     Argdecl;
     TAGGED t;
{
  REGISTER TAGGED temp;
  int arity,i;

  t = safe_deref(t);
  switch(TagOf(t)) {
  case LST:
    putchar('[');
    RefCar(temp,t);
    wr_tagged_rec(Arg,temp);
    RefCdr(temp,t);
    t = safe_deref(temp);
    while(TagIsLST(t))	{
      putchar(',');
      RefCar(temp,t);
      wr_tagged_rec(Arg,temp);
      RefCdr(temp,t);
      t = safe_deref(temp);
    }
    if(t!=atom_nil) {
      putchar('|');
      wr_tagged_rec(Arg,t);
    }
    putchar(']');
    break;
  case STR:
    if (STRIsLarge(t))
      goto number;
    wr_tagged_rec(Arg,TagToHeadfunctor(t));
    putchar('(');
    arity = Arity(TagToHeadfunctor(t));
    for(i=1; i<=arity; i++){
      if(i>1) putchar(',');
      RefArg(temp,t,i);
      wr_tagged_rec(Arg,temp);
    }
    putchar(')');
    break;
  case UBV:
  case SVA:
  case HVA:
  case CVA:
    print_variable(Arg,stream_user_output,t);
    break;
  case ATM:
    print_atom(Arg,stream_user_output,t);
    break;
  case NUM:
  number:
  print_number(Arg, stream_user_output,t);
  break;
  }
}
#endif

static ENG_FLT fzero = 0.0;    /* Shared, no locked */
static unsigned long *zeros = (unsigned long *)(&fzero);        /* Shared */

void checkasserts()
{
  assert((sizeof(ENG_INT) == 4));
  assert((sizeof(TAGGED *) == 4));
  assert((sizeof(ENG_FLT) == 8));
  assert((zeros[0]==0 && zeros[1]==0));
}


void wr_functor(s,func)
     char *s;
     struct definition *func;
{
  printf("%s: ",s);
  wr_functor_1(func);
  putchar('\n');
}

/* unused */
/*
static struct definition *which_parent(func)
     struct definition *func;
{
  REGISTER struct definition *func1;

  do
    func1 = func,
    func = (struct definition *)TagToPointer(func1->printname);
  while (!(func1->printname & 2));
  return func;
}
*/

/* unused */
/*
static which_child(func)
     struct definition *func;
{
   REGISTER int i; 
  struct definition *f1;  

  for (i=1, f1 = which_parent(func)->code.incoreinfo->subdefs;
       f1 != func;
       i++, f1 = (struct definition *)TagToPointer(f1->printname))
    ;

  return i;

  printf("Out of order!!\n");
}
*/

void wr_functor_1(func)
     struct definition *func;
{
  if (!(func->printname & 1))
    printf("%s/%d", GetString(func->printname), func->arity);
  else
    printf("(?)");
/*
    {
      putchar('(');
      wr_functor_1(which_parent(func));
      printf("-%d)/%d", which_child(func), func->arity);
    }
*/
} 

void display_term(Argdecl, TAGGED term, struct stream_node *stream, BOOL quoted);

void wr_call(Arg,s,func)
     Argdecl;
     char *s;
     struct definition *func;
{
  short i;

  printf("%s: ",s);

  if (!(func->printname & 1))
    {
      printf(GetString(func->printname));
      if (func->arity > 0) {
        putchar('(');
        DEREF(X(0),X(0));
        display_term(Arg,X(0),Output_Stream_Ptr, TRUE);
        for (i = 1; i < func->arity; i++) printf(",_");
        putchar(')');
      }
    }
  else
    printf("(?)");

  putchar('\n');
}

#if defined(DBG) || defined(DEBUG)

void wr_functor_spec(Arg,t)
     Argdecl;
     TAGGED t;
{
  wr_tagged(Arg,t);
  printf("/%ld\n",Arity(t));
}

void wr_x(Arg,i)
     Argdecl;
     int i;
{
  wr_tagged(Arg,X(i));
}

void wr_y(Arg,i)
     Argdecl;
     int i;
{
  REGISTER struct frame *E = w->frame;
  
  wr_tagged(Arg,Y(i));
}

#endif
