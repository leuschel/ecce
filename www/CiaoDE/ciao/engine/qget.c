/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */
#include <stdlib.h>

#include "threads.h"
#include "datadefs.h"
#include "support.h"


/* declarations for global functions accessed here */

#include "qget_defs.h"
#include "qread_defs.h"
#include "bignum_defs.h"
#include "start_defs.h"

/* local declarations */



/* Shared? Might be; then, only one thread may read in ql's at a time.
   Since it is not a very common situation, we might as well lock ql
   reading.  In any case, access to program area should be locked, and so we
   do not loose anything. */

#define WORKSTRINGLEN (STATICMAXATOM)

/* Called just once at the beginning of the execution */
/* static int workstringlen;*/

static char workstring[WORKSTRINGLEN]; 

/*
void init_workstring()
{
  workstringlen = STATICMAXATOM;    
  workstring    = malloc(workstringlen+1); 
} 
*/  

/* Increase the size of the (temporary) string which holds the .po code */
/*
static void expand_workstring()
{
  workstringlen *= 2;
  workstring     = realloc(workstring, workstringlen+1);
}
*/

int getshort(f)
     FILE *f;
{
  REGISTER char *ws;
#if defined(DEBUG)
  REGISTER int used_length = 0;
#endif

  for(ws = workstring; (*ws++ = GETC(f)); )
#if defined(DEBUG)
    if (++used_length > WORKSTRINGLEN)
      SERIOUS_FAULT("workstring length exceeded in getshort()")
#else
    ;
#endif
  return atoi(workstring);
}

ENG_INT getlong(f)
     FILE *f;
{
  REGISTER char *ws = workstring;

  while ((*ws++ = GETC(f)))
#if defined(DEBUG)
    if ((ws - workstring) > WORKSTRINGLEN)
      SERIOUS_FAULT("workstring length exceeded in getlong()")
#endif
    ;
    
  return atol(workstring);
}

TAGGED getlarge(Arg,f)
     Argdecl;
     FILE *f;
{
  REGISTER int used_length = 0;
  REGISTER char *ws = Atom_Buffer;


  while ((ws[used_length++] = GETC(f)))
    if (used_length == Atom_Buffer_Length){
      EXPAND_ATOM_BUFFER(Atom_Buffer_Length*2);
      ws = Atom_Buffer; /* New buffer */
    }

  if (bn_from_string(ws,w->global_top,Heap_End,GetSmall(current_radix))) {
      SERIOUS_FAULT("$qload: miscalculated heap usage");
  } else {
      TAGGED *h = w->global_top;
      int ar = LargeArity(h[0]);
      
      if (ar==2 && IntIsSmall((int)h[1]))
	return MakeSmall(h[1]);
      else {
	  w->global_top += ar+1;
	  h[ar] = h[0];
	  return Tag(STR,h);
	}
    }
}


ENG_FLT getdouble(f)
     FILE *f;
{
  REGISTER char *ws = workstring;

  while((*ws++ = GETC(f)))
#if defined(DEBUG)
    if ((ws - workstring) > WORKSTRINGLEN)
      SERIOUS_FAULT("workstring length exceeded in getdouble()")
#endif
    ;
  return atof(workstring);
}

char *getstring(Arg, f)
     Argdecl;
     FILE *f;
{
  REGISTER int used_length = 0;
  REGISTER char *ws = Atom_Buffer;   /* Try to avoid indirection through WAM */

  while ((ws[used_length++] = GETC(f)))
    if (used_length == Atom_Buffer_Length){
      EXPAND_ATOM_BUFFER(Atom_Buffer_Length*2);
      ws = Atom_Buffer;
    }
  return ws;
}

#define USE_STR2NUM_IN_GET_BYTECODE 1

void getbytecode(Arg,f,insn_p,length)
     Argdecl;
     FILE *f;
     INSN *insn_p;
     int length;
{
  REGISTER char c;

  while ((c=GETC(f))) {
    switch (c) {
    case 'G': {
#if defined(USE_STR2NUM_IN_GET_BYTECODE)
      BOOL string_to_number(Argdecl, unsigned char *AtBuf, int base, TAGGED *strnum);
      void explicit_heap_overflow(Argdecl, int pad, int arity);
      REGISTER int i = 0;
      REGISTER char *ws = Atom_Buffer;
      TAGGED t, *h;
      
      while ((c = ws[i++] = GETC(f))) {
        if (i == Atom_Buffer_Length) {
            EXPAND_ATOM_BUFFER(Atom_Buffer_Length*2);
            ws = Atom_Buffer;
        }
      }

      /* todo: This is can be improved. 

         We ensure that there is at least (length/sizeof(tagged_t)+4)
	 available words in the heap. We load the number into the
	 heap, copy it to the bytecode, and then move the heap pointer
	 back.
      */
      {
	int arity = 2;
	int cells = (length / sizeof(TAGGED)) + 4 + arity;
	if (HeapDifference(w->global_top,Heap_End)<cells) {
	  explicit_heap_overflow(Arg,cells,5);
	}
      }
      h = w->global_top;
      if (!string_to_number(w, (unsigned char *)Atom_Buffer, 10, &t)) {
	SERIOUS_FAULT("$qread: wrong number!");
      }
      insn_p += compile_large(t, insn_p);
      w->global_top = h;
      break;
#else
      REGISTER int i = 0;
      REGISTER char *ws = Atom_Buffer;
      BOOL floatp = FALSE;
      TAGGED *wp = (TAGGED *)insn_p;
      
      while ((c = ws[i++] = GETC(f))) {
        if (c == '.') floatp = TRUE;
        if (i == Atom_Buffer_Length) {
            EXPAND_ATOM_BUFFER(Atom_Buffer_Length*2);
            ws = Atom_Buffer;
        }
      }

      if (floatp) {
        ENG_FLT f = atof(ws);
        TAGGED *fp = (TAGGED *)(&f);
        
        *wp++ = MakeFunctorFloat;
        *wp++ = fp[0];
        *wp++ = fp[1];
        insn_p += 6;
      } else {
        bn_from_string(ws,(Bignum *)insn_p,(Bignum *)((char *)insn_p+length), GetSmall(current_radix));
        insn_p += LargeArity(*wp)<<1;
      }
      break;
#endif
    }
      
    case '+': {
      REGISTER char *ws = workstring;
      
      while ((*ws++ = GETC(f)))
#if defined(DEBUG)
        if ((ws - workstring) > WORKSTRINGLEN)
          SERIOUS_FAULT("workstring length exceeded in getbytecode() [+]")
#endif
        ;

      *(ENG_INT *)insn_p = atol(workstring);
      insn_p += BPL;
      break;
    }
      
    case 'C': {
      REGISTER char *ws = workstring;
      
      while ((*ws++ = GETC(f)))
#if defined(DEBUG)
        if ((ws - workstring) > WORKSTRINGLEN)
          SERIOUS_FAULT("workstring length exceeded in getbytecode() [C]")
#endif
        ;

      *(CInfo *)insn_p = builtintab[atoi(workstring)];
      insn_p += BPTP;
      break;
    }
      
    case 'F': {
      REGISTER char *ws = workstring;
      
      while ((*ws++ = GETC(f)))
#if defined(DEBUG)
        if ((ws - workstring) > WORKSTRINGLEN)
          SERIOUS_FAULT("workstring length exceeded in getbytecode() [F]" )
#endif
        ;

      /* Was:
       *insn_p++ = (INSN)builtintab[atoi(workstring)]);
       ant it issued a mesage since INSN is short int and *builtintab
       is a pointer to a function */
      *insn_p++ = (INSN)((unsigned long int)builtintab[atoi(workstring)]);
      break;
    }
    case 'D': {
      REGISTER char *ws = workstring;
      
      while ((*ws++ = GETC(f)))
#if defined(DEBUG)
        if ((ws - workstring) > WORKSTRINGLEN)
          SERIOUS_FAULT("workstring length exceeded in getbytecode() [D]")
#endif
        ;

      *(CInfo *)insn_p = builtintab[atoi(workstring)];

      ws = workstring;

      while ((*ws++ = GETC(f)))
#if defined(DEBUG)
        if ((ws - workstring) > WORKSTRINGLEN)
          SERIOUS_FAULT("workstring length exceeded in getbytecode() [D+]")
#endif
        ;

      *(long *)insn_p += atol(workstring);
      insn_p += BPTP;
      break;
    }
      
    case 'E': {
      REGISTER char *ws = workstring;
      
      while ((*ws++ = GETC(f)))
#if defined(DEBUG)
        if ((ws - workstring) > WORKSTRINGLEN)
          SERIOUS_FAULT("workstring length exceeded in getbytecode() [E]")
#endif
        ;

      *(unsigned long *)insn_p =
        (unsigned long)builtintab[atoi(workstring)] - (unsigned long)insn_p;
      insn_p += BPTP;
      break;
    }
      
    default: {
      REGISTER char *ws = workstring;
      
      *ws++ = c;
      while ((*ws++ = GETC(f)))
#if defined(DEBUG)
        if ((ws - workstring) > WORKSTRINGLEN)
         SERIOUS_FAULT("workstring length exceeded in getbytecode() [default]")
#endif
        ;

      *insn_p++ = atoi(workstring);
    }
    }
  }
}

