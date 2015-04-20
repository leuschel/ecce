/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

#include "datadefs.h"
#include "support.h"
#include "wambuiltin.h"

/* declarations for global functions accessed here */

#include "wambuiltin_defs.h"
#include "misc_defs.h"
#include "bignum_defs.h"

/* local declarations */


static char *illexp = "illegal arithmetic expression";          /* Shared */

/*
  extern int bn_compare PROTO((Bignum *x, Bignum *y));
  extern BOOL bn_positive PROTO((Bignum *x));
*/

BOOL bu2_numeq(Arg, x0,x1)
     Argdecl;
     TAGGED x0,x1;
{
  REGISTER TAGGED t1,t,u;
  
  Numstack_End = NULL;
  t=x0; NDEREF(Arg, t,t1,return FALSE;);
  u=x1; NDEREF(Arg, u,t1,return FALSE;);
  if (TagIsSmall(t)&&TagIsSmall(u))
    return (t==u);
  else if (IsFloat(t) || IsFloat(u))
    return (GetFloat(t)==GetFloat(u));
  else if (TagIsSmall(t) || TagIsSmall(u))
    return FALSE;
  else
    return (bn_compare(TagToSTR(t),TagToSTR(u))==0);
}

BOOL bu2_numne(Arg,x0,x1)
     Argdecl;
     TAGGED x0,x1;
{
  REGISTER TAGGED t1,t,u;
  
  Numstack_End = NULL;
  t=x0; NDEREF(Arg, t,t1,return FALSE;);
  u=x1; NDEREF(Arg, u,t1,return FALSE;);
  if (TagIsSmall(t)&&TagIsSmall(u))
    return (t!=u);
  else if (IsFloat(t) || IsFloat(u))
    return (GetFloat(t)!=GetFloat(u));
  else if (TagIsSmall(t) || TagIsSmall(u))
    return TRUE;
  else
    return (bn_compare(TagToSTR(t),TagToSTR(u))!=0);
}

BOOL bu2_numlt(Arg,x0,x1)
     Argdecl;
     TAGGED x0,x1;
{
  REGISTER TAGGED t1,t,u;
  
  Numstack_End = NULL;
  t=x0; NDEREF(Arg, t,t1,return FALSE;);
  u=x1; NDEREF(Arg, u,t1,return FALSE;);
  if (TagIsSmall(t)&&TagIsSmall(u))
    return (t<u);
  else if (IsFloat(t) || IsFloat(u))
    return (GetFloat(t)<GetFloat(u));
  else if (TagIsSmall(t))
    return bn_positive(TagToSTR(u));
  else if (TagIsSmall(u))
    return !bn_positive(TagToSTR(t));
  else
    return (bn_compare(TagToSTR(t),TagToSTR(u))<0);
}

BOOL bu2_numle(Arg,x0,x1)
     Argdecl;
     TAGGED x0,x1;
{
  REGISTER TAGGED t1,t,u;
  
  Numstack_End = NULL;
  t=x0; NDEREF(Arg, t,t1,return FALSE;);
  u=x1; NDEREF(Arg, u,t1,return FALSE;);
  if (TagIsSmall(t)&&TagIsSmall(u))
    return (t<=u);
  else if (IsFloat(t) || IsFloat(u))
    return (GetFloat(t)<=GetFloat(u));
  else if (TagIsSmall(t))
    return bn_positive(TagToSTR(u));
  else if (TagIsSmall(u))
    return !bn_positive(TagToSTR(t));
  else
    return (bn_compare(TagToSTR(t),TagToSTR(u))<=0);
}

BOOL bu2_numgt(Arg,x0,x1)
     Argdecl;
     TAGGED x0,x1;
{
  REGISTER TAGGED t1,t,u;
  
  Numstack_End = NULL;
  t=x0; NDEREF(Arg, t,t1,return FALSE;);
  u=x1; NDEREF(Arg, u,t1,return FALSE;);
  if (TagIsSmall(t)&&TagIsSmall(u))
    return (t>u);
  else if (IsFloat(t) || IsFloat(u))
    return (GetFloat(t)>GetFloat(u));
  else if (TagIsSmall(t))
    return !bn_positive(TagToSTR(u));
  else if (TagIsSmall(u))
    return bn_positive(TagToSTR(t));
  else
    return (bn_compare(TagToSTR(t),TagToSTR(u))>0);
}

BOOL bu2_numge(Arg,x0,x1)
     Argdecl;
     TAGGED x0,x1;
{
  REGISTER TAGGED t1,t,u;
  
  Numstack_End = NULL;
  t=x0; NDEREF(Arg, t,t1,return FALSE;);
  u=x1; NDEREF(Arg, u,t1,return FALSE;);
  if (TagIsSmall(t)&&TagIsSmall(u))
    return (t>=u);
  else if (IsFloat(t) || IsFloat(u))
    return (GetFloat(t)>=GetFloat(u));
  else if (TagIsSmall(t))
    return !bn_positive(TagToSTR(u));
  else if (TagIsSmall(u))
    return bn_positive(TagToSTR(t));
  else
    return (bn_compare(TagToSTR(t),TagToSTR(u))>=0);
}

BOOL bu1_atom(Arg,x0)
     Argdecl;
     REGISTER TAGGED x0;
{
  REGISTER TAGGED t0;

  DerefSwitch(x0,t0,{return FALSE;})
  return (TermIsATM(x0));
}

BOOL bu1_atomic(Arg,x0)
     Argdecl;
     REGISTER TAGGED x0;
{
  REGISTER TAGGED t0;

  DerefSwitch(x0,t0,{return FALSE;})
  return (!(x0 & TagBitComplex) || TagIsLarge(x0));
}

BOOL bu1_float(Arg,x0)
     Argdecl;
     REGISTER TAGGED x0;
{
  REGISTER TAGGED t0;

  DerefSwitch(x0,t0,{return FALSE;})
  return IsFloat(x0);
}

BOOL bu1_integer(Arg,x0)
     Argdecl;
     REGISTER TAGGED x0;
{
  REGISTER TAGGED t0;

  DerefSwitch(x0,t0,{return FALSE;})
  return IsInteger(x0);
}

BOOL bu1_number(Arg,x0)
     Argdecl;
     REGISTER TAGGED x0;
{
  REGISTER TAGGED t0;

  DerefSwitch(x0,t0,{return FALSE;})
  return IsNumber(x0);
}

BOOL bu1_var(Arg,x0)
     Argdecl;
     REGISTER TAGGED x0;
{
  REGISTER TAGGED t0;

  DerefSwitch(x0,t0,{return TRUE;})
  return FALSE;
}

BOOL bu1_nonvar(Arg,x0)
     Argdecl;
     REGISTER TAGGED x0;
{
  REGISTER TAGGED t0;

  DerefSwitch(x0,t0,{return FALSE;})
  return TRUE;
}

BOOL bu2_lexeq(Arg,x0,x1)
     Argdecl;
     TAGGED x0,x1;
{
  extern int compare_help();

  return (x0==x1 || compare_help(Arg,x0,x1)==0);
}

BOOL bu2_lexne(Arg,x0,x1)
     Argdecl;
     TAGGED x0,x1;
{
  return (x0!=x1 && compare_help(Arg,x0,x1)!=0);
}

BOOL bu2_lexlt(Arg,x0,x1)
     Argdecl;
     TAGGED x0,x1;
{
  return (x0!=x1 && compare_help(Arg,x0,x1)<0);
}

BOOL bu2_lexle(Arg,x0,x1)
     Argdecl;
     TAGGED x0,x1;
{
  return (x0==x1 || compare_help(Arg,x0,x1)<=0);
}

BOOL bu2_lexgt(Arg,x0,x1)
     Argdecl;
     TAGGED x0,x1;
{
  return (x0!=x1 && compare_help(Arg,x0,x1)>0);
}

BOOL bu2_lexge(Arg,x0,x1)
     Argdecl;
     TAGGED x0,x1;
{
  return (x0==x1 || compare_help(Arg,x0,x1)>=0);
}

TAGGED fu2_compare(Arg,x1,x2)
     Argdecl;
     TAGGED x1,x2;
{
  int i;

  if (x1==x2)
    return atom_equal;
  else if ((i=compare_help(Arg,x1,x2)) < 0)
    return atom_lessthan;
  else if (i>0)
    return atom_greaterthan;
  else
    return atom_equal;
}


/* ---------------------------------------------------------------- */

TAGGED fu2_arg(Arg,number,complex)
     Argdecl;
     REGISTER TAGGED number, complex;
{
  REGISTER TAGGED t0;
  
  DerefSwitch(number,t0,{goto barf1;});
  DerefSwitch(complex,t0,{goto barf2;});


  if (TagIsSTR(complex))
    {
      REGISTER int i = GetSmall(number);
      REGISTER TAGGED f = TagToHeadfunctor(complex);

      if (i<=0 || i>Arity(f) || f&QMask)
	goto barf1;
      
      RefArg(t0,complex,i);
      return t0;
    }
  else if (IsComplex(complex))	/* i.e. list */
    {
      if (number==MakeSmall(1))
	{
	  RefCar(t0,complex);
	  return t0;
	}
      else if (number==MakeSmall(2))
	{
	  RefCdr(t0,complex);
	  return t0;
	}
      else
	goto barf1;
    }
  else
    goto barf2;

 barf1:
  MINOR_FAULT("arg/3: incorrect 1st argument");

 barf2:
  MINOR_FAULT("arg/3: incorrect 2nd argument");
}

/*---------------------------------------------------------------*/


BOOL bu3_functor(Arg,term,name,arity)
     Argdecl;
     REGISTER TAGGED term,name,arity;
{
  REGISTER TAGGED t0;
  
  DerefSwitch(term,t0,{goto construct;});
    {
      TAGGED tagarity;
      
      if (TermIsAtomic(term))
	tagarity = TaggedZero;
      else if (!(term & TagBitFunctor))
	term = atom_list,
	tagarity = MakeSmall(2);
      else
	{
	  TAGGED f = TagToHeadfunctor(term);
	  
	  term = SetArity(f,0),
	  tagarity = MakeSmall(Arity(f));
	}

      Unify_constant(tagarity,arity);
      return cunify(Arg,term,name);
    }
 construct:
    {
      DerefSwitch(name,t0,;);
      DerefSwitch(arity,t0,;);
      if (TermIsAtomic(name) && (arity==TaggedZero))
	return cunify(Arg,name,term);
      else if (TagIsATM(name) &&
	       (arity>TaggedZero) && (arity<MakeSmall(ARITYLIMIT)))
	return cunify(Arg,
                      make_structure(Arg, SetArity(name,GetSmall(arity))),
                      term);
      else
	return FALSE;
    }
}



/*---------------------------------------------------------------*/

BOOL bu2_univ(Arg,term,list)
     Argdecl;
     REGISTER TAGGED term;
     TAGGED list;
{
#if !sequent
  REGISTER
#endif
    TAGGED car, cdr, *argp, *argq;
  int arity;
  TAGGED f;

  DerefSwitch(term,car,{goto construct;});
  cdr = atom_nil;
  if (TermIsAtomic(term))
    {
      MakeLST(cdr,term,cdr);
      return cunify(Arg,cdr,list);
    }
  
  if (term & TagBitFunctor)
    f = TagToHeadfunctor(term),
    argp = TagToArg(term,1),
    argq = HeapOffset(argp,Arity(f));
  else
    f = functor_list,
    argp = TagToCar(term),
    argq = HeapOffset(argp,2);
  
  while HeapYounger(argq,argp)
    {
      HeapDecr(argq);
      RefHeap(car,argq);
      MakeLST(cdr,car,cdr);
    }
  MakeLST(cdr,SetArity(f,0),cdr);
  return cunify(Arg,cdr,list);

 construct:
  cdr = list;
  DerefSwitch(cdr,car,;);
  arity = 0;

  if (IsVar(cdr))
    goto bomb;
  if (!TagIsLST(cdr))
    MINOR_FAULT("=../2: incorrect 2nd argument");
  DerefCar(f,cdr);
  DerefCdr(cdr,cdr);
  if (TermIsAtomic(f) && (cdr==atom_nil))
    return cunify(Arg,f,term);
  else if (IsVar(f))
    goto bomb;
  else if (!TagIsATM(f))
    MINOR_FAULT("=../2: incorrect 2nd argument");
  
  argp = w->global_top;
  HeapPush(w->global_top,f);
  while (TagIsLST(cdr) && arity<ARITYLIMIT)
    {
      DerefCar(car,cdr);
      DerefCdr(cdr,cdr);
      HeapPush(w->global_top,car);
      arity++;
    }
  if (IsVar(cdr))
    goto bomb;
  if (cdr!=atom_nil || arity==ARITYLIMIT)
    MINOR_FAULT("=../2: incorrect 2nd argument");
  
  f = SetArity(f,arity);
  if (f==functor_list)
    {
      w->global_top = argp;
      argq = HeapOffset(w->global_top,1);
      RefHeapNext(car,argq);
      RefHeapNext(cdr,argq);
      HeapPush(w->global_top,car);
      HeapPush(w->global_top,cdr);
      return cunify(Arg,Tag(LST,argp),term);
    }
  else
    {
      *argp = f;
      return cunify(Arg,Tag(STR,argp),term);
    }

 bomb:
    USAGE_FAULT("=../2: illegal arguments");
}

/* Support for if/3 */
BOOL bu1_if(Arg,x0)
     Argdecl;
     REGISTER TAGGED x0;
{
  DEREF(x0,x0);
  CTagToCar(x0) = atom_true;
  return TRUE;
}
