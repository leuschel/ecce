/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */


#if defined(GAUGE)

#include "datadefs.h"
#include "support.h"
#include "predtyp.h"

/* declarations for global functions accessed here */

#include "gauge_defs.h"

/* local declarations */



#define ISNOTRY(T)		(((T)==NULL) || ((T)==fail_alt))

/* Prolog predicates. */


BOOL counter_values(Arg)
     Argdecl;
{
  REGISTER TAGGED *h;
  REGISTER ENG_INT *current_counter, *max_counter, counter;
  int count;
  TAGGED values;

  DEREF(X(0),X(0));
  current_counter = (ENG_INT *)TermToPointer(X(0));

  DEREF(X(1),X(1));
  count = GetInteger(X(1));
  if (count > 255)
    {
      USAGE_FAULT("$clause_counters/2: Too many counters per clause");
    }

  max_counter = current_counter + count;
  
  h = w->global_top;
  values = Tag(STR,h);
  HeapPush(h,SetArity(atom_counter,count));
  
  while (current_counter < max_counter)
    {
      counter = *current_counter++;
      if (!IntIsSmall(counter))
	{
	  USAGE_FAULT("$clause_counters/2: Counter value exceeds 1<<25");
	}
      else
	HeapPush(h,MakeSmall(counter));
    }
  w->global_top = h;
  
  return cunify(Arg,values,X(2));
}

BOOL reset_counters(Arg)
     Argdecl;
{
  ENG_INT *current_counter, *max_counter;

  DEREF(X(0),X(0));
  current_counter = (ENG_INT *)TermToPointer(X(0));
  DEREF(X(1),X(1));
  max_counter = current_counter + GetInteger(X(1));

  while (current_counter < max_counter)
    *current_counter++ = 0;

  return TRUE;
}

BOOL emulated_clause_counters(Arg)
     Argdecl;
{
  struct definition *d;
  REGISTER struct emul_info *cl;
  REGISTER int i, count;
  
  DEREF(X(0),X(0));
  d = parse_definition(X(0));
  if (!d)
    return FALSE;
  DEREF(X(1),X(1));
  for (i=GetSmall(X(1)), cl=d->code.incoreinfo->clauses; i>1; --i)
    cl = cl->next;
  Unify_constant(PointerToTerm(cl->counters),X(2));
  count = NumberOfCounters(cl);
  return cunify(Arg,MakeInteger(count),X(3));
}

#endif /* GAUGE */
