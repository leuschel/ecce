/* Copyright (C) 1996,1997,1998,1999,2000,2001,2002 UPM-CLIP */

#include "common_headers.h"
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>

#define FASTRW_VERSION  'C'
#define FASTRW_MAX_VARS 1024

BOOL prolog_fast_read_on_c(Arg)		/* OPA */
     Argdecl;
{
  int i,lastvar = 0;
  TAGGED term, vars[FASTRW_MAX_VARS];

  if ((i = getc(Input_Stream_Ptr->streamfile)) < -1)
     BUILTIN_ERROR(READ_PAST_EOS_ERROR,atom_nil,0)
  if (i != FASTRW_VERSION) return FALSE;

  if (!prolog_fast_read_on_c_aux(Arg,&term,vars,&lastvar)) return FALSE;
  return cunify(Arg,X(0),term);
}

BOOL prolog_fast_read_on_c_aux(Arg,out,vars,lastvar)
     Argdecl;
     TAGGED *out,*vars;
     int *lastvar;
{
  int i,k,j;
  unsigned char *s = (unsigned char *) Atom_Buffer;
  TAGGED *h = w->global_top;

  if ((k = getc(Input_Stream_Ptr->streamfile)) < -1)
     BUILTIN_ERROR(READ_PAST_EOS_ERROR,atom_nil,0)

  switch(k) {
  case ']':
    *out = atom_nil;
    return TRUE;
  case '[':
    w->global_top += 2;
    if (!prolog_fast_read_on_c_aux(Arg,h,vars,lastvar)) return FALSE;
    if (!prolog_fast_read_on_c_aux(Arg,h+1,vars,lastvar)) return FALSE;
    *out = Tag(LST,h);
    return TRUE;
  case '_':
  case 'I':
  case 'F':
  case 'A':
  case '"':
  case 'S':
    j = 1;
    for (i=0; j; i++) {
      if (i== Atom_Buffer_Length) {
	Atom_Buffer = (char *)checkrealloc((TAGGED *)Atom_Buffer,
					   i, Atom_Buffer_Length<<=1);
	s = (unsigned char *)Atom_Buffer+i;}
      if ((j = getc(Input_Stream_Ptr->streamfile)) < -1)
	BUILTIN_ERROR(READ_PAST_EOS_ERROR,atom_nil,0)
      *s++ = j;
    }
    switch (k) {
    case '_':
      if ((i = atoi(Atom_Buffer)) == *lastvar)
	*h = vars[(*lastvar)++] = TagHVA(w->global_top++);
      *out = vars[i];
      return TRUE;
    case 'I':
      if(i = bn_from_string(Atom_Buffer,h, Heap_End-CONTPAD)) {
	explicit_heap_overflow(Arg,i+CONTPAD, 2);
	if (bn_from_string(Atom_Buffer,w->global_top, Heap_End-CONTPAD))
	  SERIOUS_FAULT("miscalculated size of bignum");
      }
      if ((i = LargeArity(h[0])) ==2 && IntIsSmall((int)h[1]))
	*out = MakeSmall(h[1]);
      else {
	*out = Tag(STR,h);
	w->global_top += i+1;
	h[i] = h[0];
      }	
      return TRUE;
    case 'F':
      *out = MakeFloat(Arg,atof(Atom_Buffer));
      return TRUE;
    case 'A':
      *out = MakeString(Atom_Buffer);
      return TRUE;
    case '"':
      for (i--;i--;) MakeLST(*out,MakeSmall(Atom_Buffer[i]),*out);
      if (!prolog_fast_read_on_c_aux(Arg,h+1,vars,lastvar)) return FALSE;
      return TRUE;
    case 'S':
      if ((i = getc(Input_Stream_Ptr->streamfile)) < -1)
	BUILTIN_ERROR(READ_PAST_EOS_ERROR,atom_nil,0)
      *h = SetArity(MakeString(Atom_Buffer),i);
      *out = Tag(STR,h++);
      for(w->global_top += i+1;i--;)
	if (!prolog_fast_read_on_c_aux(Arg,h++,vars,lastvar)) return FALSE;
      return TRUE;
    }
  default:
    return FALSE;
  }
}

BOOL prolog_fast_write_on_c(Arg)		/* OPA */
     Argdecl;
{
  TAGGED vars[FASTRW_MAX_VARS];
  int lastvar = 0;

  DEREF(X(0),X(0));
  writechar(FASTRW_VERSION,1,Output_Stream_Ptr);
  prolog_fast_write_on_c_aux(Arg,X(0),vars,&lastvar);
  return TRUE;
}

prolog_fast_write_on_c_aux(Arg,in,vars,lastvar)
     Argdecl;
     TAGGED in, *vars;
     int *lastvar;
{
  int i,j;
  TAGGED term;

  switch (TagOf(in))
    {
    case LST:
      DerefCar(term,in);
      DerefCdr(in,in);
      if (TagIsSmall(term) && GetSmall(term) && ((i = GetSmall(term)) < 256)){
	for(writechar('"',1,Output_Stream_Ptr);i && (i < 256);) {
	  writechar(i,1,Output_Stream_Ptr);
	  if (TagOf(in) == LST) {
	    DerefCar(term,in);
	    DerefCdr(in,in);
	    if (!TagIsSmall(term)) break;
	    else i = GetSmall(term);
	  }
	  else {
	    writechar(0,1,Output_Stream_Ptr);
	    prolog_fast_write_on_c_aux(Arg,in,vars,lastvar);
	    return;
	  }	  
	}
	writechar(0,1,Output_Stream_Ptr);
      }
      writechar('[',1,Output_Stream_Ptr);
      prolog_fast_write_on_c_aux(Arg,term,vars,lastvar);
      prolog_fast_write_on_c_aux(Arg,in,vars,lastvar);
      return;
    case UBV:
    case SVA:
    case HVA:
    case CVA:
      writechar('_',1,Output_Stream_Ptr);
      DEREF(in,in);
      for (i = 0;i < *lastvar; i++)
	if (vars[i] == in) break;
      if (i == *lastvar) vars[(*lastvar)++] = in;
      snprintf((char *) Atom_Buffer,4,"%i",i);
      print_string(Output_Stream_Ptr,Atom_Buffer);
      writechar(0,1,Output_Stream_Ptr);
      return;
    case STR:
      if (!STRIsLarge(in)) {
      writechar('S',1,Output_Stream_Ptr);
      print_string(Output_Stream_Ptr,TagToAtom(TagToHeadfunctor(in))->name);
      writechar(0,1,Output_Stream_Ptr);
      writechar(j = Arity(TagToHeadfunctor(in)),1,Output_Stream_Ptr);
      for(i = 1; i <= j; prolog_fast_write_on_c_aux(Arg,term,vars,lastvar))
	DerefArg(term,in,i++);
      return;
      }
    case NUM:
      if (IsFloat(in)) writechar('F',1,Output_Stream_Ptr);
      else writechar('I',1,Output_Stream_Ptr);
      print_number(Arg,Output_Stream_Ptr,in);
      writechar(0,1,Output_Stream_Ptr);
      return;
    case ATM:
      if (in != atom_nil) {
	writechar('A',1,Output_Stream_Ptr);
	print_string(Output_Stream_Ptr,TagToAtom(in)->name);
	writechar(0,1,Output_Stream_Ptr);
      }
      else writechar(']',1,Output_Stream_Ptr);
      return;
    }
}

void fastrw_init(module)
     char *module;
{
  define_c_mod_predicate(module, "fast_read", prolog_fast_read_on_c, 1);
  define_c_mod_predicate(module, "fast_write", prolog_fast_write_on_c, 1);
}

void fastrw_end(module)
     char *module;
{
  undefine_c_mod_predicate(module, "fast_read", 1);
  undefine_c_mod_predicate(module, "fast_write", 1);
}
