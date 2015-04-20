/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */

#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>  /* for atoi MCL */
#include <string.h>

void perror() /*, ENG_perror()*/;
extern int errno;

#include "compat.h"
#include "threads.h"
#include "datadefs.h"
#include "support.h"
#include "ciao.h"
#include "task_areas.h"

/* declarations for global functions accessed here */

#include "term_support_defs.h"
#include "inout_defs.h"
#include "streams_defs.h"
#include "tasks_defs.h"
#include "start_defs.h"
#include "alloc_defs.h"
#include "bignum_defs.h"
#include "stacks_defs.h"

/* local declarations */

static int readchar(register struct stream_node *s, int type, struct definition *pred_address);
void display_term(Argdecl, TAGGED term, struct stream_node *stream, BOOL quoted);
BOOL code_class(Argdecl);
BOOL peek(Argdecl);
BOOL peek2(Argdecl);


BOOL code_class(Arg)
     Argdecl;
{
  int i;
  
  DEREF(X(0),X(0));
  if (!TagIsSmall(X(0)) || (i = GetSmall(X(0))) & ~255) /* Not a byte */
    ERROR_IN_ARG(X(0),1,BYTE)

  return cunify(Arg,X(1),MakeSmall(symbolchar[i]));
}

#define INC_COUNTS(ch,stream) \
{ \
    stream->char_count++; \
    if (ch == 0xd) { \
      stream->last_nl_pos = stream->char_count; \
      stream->nl_count++; \
    } else if (ch == 0xa) { \
      stream->last_nl_pos = stream->char_count; \
      if (stream->previous_char != 0xd) \
        stream->nl_count++; \
    } \
    stream->previous_char = ch; \
}

void writechar(ch,i,s)
     int ch;
     REGISTER int i;
     REGISTER struct stream_node *s;
{
  REGISTER FILE *f = s->streamfile;
  
  if (s->isatty)
    s = root_stream_ptr;
  while (--i >= 0){
    if (s->streammode != 's')                              /* Not a socket */
      putc(ch, f);
    else {
      char p;
      p = (char)ch;
      write(GetInteger(s->label), &p, (size_t)1);
    }
    INC_COUNTS(ch,s);
  }
}


int (*ENG_read_hook)() = NULL;

#define SAVE_XREGS(where, how_many) \
{ \
    int i; \
    struct worker *w = get_my_worker(); \
    if (!w) SERIOUS_FAULT("Could not get worker in SAVE_XREGS"); \
    for(i = 0; i < how_many; i++) \
      (where)[i] = X(i); \
}

#define RESTORE_XREGS(where, how_many) \
{ \
    int i; \
    struct worker *w = get_my_worker(); \
    if (!w) SERIOUS_FAULT("Could not get worker in RESTORE_XREGS"); \
    for(i = 0; i < how_many; i++) \
      X(i) = (where)[i];  \
}

#define REGS_TO_SAVE 32                            /* How many, actually? */

#define DELRET -5
#define PEEK   -4
#define GET    -3
#define GET1   -2
#define SKIPLN -1

#define EXITCOND(op,i) \
  ( op<GET1 || i==EOF || (op==GET1 && symbolchar[i]>0) || \
    (op==SKIPLN && (i==0xa || i==0xd)) || op==i )

#define GIVEBACKCOND(op,i) \
  (op==PEEK || (op==SKIPLN && i==EOF) || (op==DELRET && i!=0xa))

/* Returns -2 when attempting to read past end of file //) */
static int readchar(s, op_type, pred_address)
     struct stream_node *s;
     int op_type;  /* DELRET, PEEK, GET, GET1, SKIPLN, or >= 0 for SKIP */
     struct definition *pred_address;
{
  FILE *f = s->streamfile;
  int i;
  unsigned char ch;
  TAGGED saved_regs[REGS_TO_SAVE];

  if (s->isatty) {
    int_address = pred_address;
    while (TRUE) {
      if (root_stream_ptr->char_count==root_stream_ptr->last_nl_pos){
        print_string(stream_user_output,GetString(current_prompt));
          /* fflush(stdout); into print_string() MCL */
      }
                                                     /* MCL: ENG_read_hook */
      if (ENG_read_hook != NULL){
        SAVE_XREGS(saved_regs, REGS_TO_SAVE);
        do               /* Call ENG_read_hook until input available at fd */
          i = (*ENG_read_hook)(fileno(f));
        while (i == 0);
        RESTORE_XREGS(saved_regs, REGS_TO_SAVE);
      }

      if (s->pending_char >= -1) { /* There is a char returned by peek */
        i = s->pending_char;
        s->pending_char = -100;
      } else
        i = getc(f);

      if GIVEBACKCOND(op_type,i)
        s->pending_char = i;
      else
        INC_COUNTS(i,root_stream_ptr);

      if (i==EOF) clearerr(f);

      if (EXITCOND(op_type,i)) {
        int_address = NULL; 
        return i;
      }
    }
  } else if (s->streammode != 's') { /* Not a socket */

    if (feof(f) && s->pending_char == -100) return -2; /* attempt to read past end of stream */
    
    while (TRUE) {
      if (s->pending_char >= -1) { /* There is a char returned by peek */
        i = s->pending_char;
        s->pending_char = -100;
      } else
        i = getc(f);

      if GIVEBACKCOND(op_type,i)
        s->pending_char = i;
      else
        INC_COUNTS(i,s);
      
      if (EXITCOND(op_type,i)) return i;
    } 
  } else {                                                  /* A socket */
    int fildes = GetInteger(s->label);
    
    if (s->socket_eof) return -2; /* attempt to read past end of stream */
    
    while (TRUE) {
      if (s->pending_char >= -1) { /* There is a char returned by peek */
        i = s->pending_char;
        s->pending_char = -100;
      } else {
        switch(read(fildes, (void *)&ch, 1)){
        case 0:
          i = EOF;
          break;
        case 1: 
          i = (int)ch;
          break;
        default:
          perror("read() in readchar(): ");
          SERIOUS_FAULT("Aborting");
        }
      }
      
      if GIVEBACKCOND(op_type,i)
        s->pending_char = i;
      else {
        INC_COUNTS(i,s);
        if (i==EOF) s->socket_eof = TRUE;
      }

      if (EXITCOND(op_type,i)) return i;

    }
  }
}

/*----------------------------------------------------------------*/

BOOL flush_output(Arg)
     Argdecl;
{
  if ((Output_Stream_Ptr->streammode != 's')
      && fflush(Output_Stream_Ptr->streamfile)) {
    ENG_perror("% fflush in flush_output/1");
  }
  return TRUE;
}

/*----------------------------------------------------------------*/

BOOL flush_output1(Arg)
     Argdecl;
{
  int errcode;
  struct stream_node *s;
  
  s = stream_to_ptr_check(X(0), 'w', &errcode);
  if (!s)
    BUILTIN_ERROR(errcode,X(0),1);

  if ((s->streammode != 's') && fflush(s->streamfile)) {
    ENG_perror("% fflush in flush_output/1");
  }
  return TRUE;
}

/*----------------------------------------------------------------*/

BOOL getct(Arg)
     Argdecl;
{
  int i;

  i = readchar(Input_Stream_Ptr,GET,address_getct);

  if (i < -1)
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0)

  return cunify(Arg,X(0),MakeSmall(i)) 
         && cunify(Arg,X(1),MakeSmall(i == -1 ? -1 : symbolchar[i]));
}


/*----------------------------------------------------------------*/

BOOL getct1(Arg)
     Argdecl;
{
  int i;
  
  i = readchar(Input_Stream_Ptr,GET1,address_getct1); /* skip whitespace */

  if (i < -1)
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0)

  return cunify(Arg,X(0),MakeSmall(i)) 
         && cunify(Arg,X(1),MakeSmall(i == -1 ? -1 : symbolchar[i]));
}


/*----------------------------------------------------------------*/

BOOL get(Arg)
     Argdecl;
{
  int i;

  i = readchar(Input_Stream_Ptr,GET,address_get);

  if (i < -1)
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0)

  return cunify(Arg,X(0),MakeSmall(i));
}

/*----------------------------------------------------------------*/

BOOL get2(Arg)
     Argdecl;
{
  int i;
  struct stream_node *s;
  
  s = stream_to_ptr_check(X(0), 'r', &i);
  if (!s)
    BUILTIN_ERROR(i,X(0),1);

  i = readchar(s,GET,address_get2);

  if (i < -1)
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),X(0),1)

  return cunify(Arg,X(1),MakeSmall(i));
}

/*----------------------------------------------------------------*/

BOOL get1(Arg)
     Argdecl;
{
  int i;
  
  i = readchar(Input_Stream_Ptr,GET1,address_get1); /* skip whitespace */

  if (i < -1)
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0)

  return cunify(Arg,X(0),MakeSmall(i));
}


/*----------------------------------------------------------------*/


BOOL get12(Arg)
     Argdecl;
{
  int i;
  struct stream_node *s;
  
  s = stream_to_ptr_check(X(0), 'r', &i);
  if (!s)
    BUILTIN_ERROR(i,X(0),1);

  i = readchar(s,GET1,address_get12); /* skip whitespace */

  if (i < -1)
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),X(0),1)

  return cunify(Arg,X(1),MakeSmall(i));
}


/*----------------------------------------------------------------*/

BOOL peek(Arg)
     Argdecl;
{
  int i;

  i = readchar(Input_Stream_Ptr,PEEK,address_peek);

  if (i < -1)
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0)

  return cunify(Arg,X(0),MakeSmall(i));
}

/*----------------------------------------------------------------*/

BOOL peek2(Arg)
     Argdecl;
{
  int i;
  struct stream_node *s;
  
  s = stream_to_ptr_check(X(0), 'r', &i);
  if (!s)
    BUILTIN_ERROR(i,X(0),1);

  i = readchar(s,PEEK,address_peek2);

  if (i < -1)
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),X(0),1)

  return cunify(Arg,X(1),MakeSmall(i));
}

/*----------------------------------------------------------------*/

BOOL nl(Arg)
     Argdecl;
{
  writechar('\n',1,Output_Stream_Ptr);
  return TRUE;
}


/*----------------------------------------------------------------*/

BOOL nl1(Arg)
     Argdecl;
{
  int errcode;
  struct stream_node *s;
  
  s = stream_to_ptr_check(X(0), 'w', &errcode);
  if (!s)
    BUILTIN_ERROR(errcode,X(0),1);

  writechar('\n',1,s);
  return TRUE;
}

/*----------------------------------------------------------------*/

BOOL put(Arg)
     Argdecl;
{
  int i;

  DEREF(X(0),X(0));
  if (!TagIsSmall(X(0)) || (i = GetSmall(X(0))) & ~255) /* Not a byte */
    ERROR_IN_ARG(X(0),1,BYTE)

  writechar(i,1,Output_Stream_Ptr);
  return TRUE;
}

/*----------------------------------------------------------------*/

BOOL put2(Arg)
     Argdecl;
{
  int i;
  struct stream_node *s;

  s = stream_to_ptr_check(X(0), 'w', &i);
  if (!s)
    BUILTIN_ERROR(i,X(0),1);

  DEREF(X(1),X(1));
  if (!TagIsSmall(X(1)) || (i = GetSmall(X(1))) & ~255) /* Not a byte */
    ERROR_IN_ARG(X(1),2,BYTE)

  writechar(i,1,s);
  return TRUE;
}

/*----------------------------------------------------------------*/
/* output stream always write or append */

BOOL tab(Arg)
     Argdecl;
{
  DEREF(X(0),X(0));
  if (!IsInteger(X(0)))
    ERROR_IN_ARG(X(0),1,INTEGER)

  writechar(' ',GetInteger(X(0)),Output_Stream_Ptr);
  return TRUE;
}


/*----------------------------------------------------------------*/

BOOL tab2(Arg)
     Argdecl;
{
  int errcode;
  struct stream_node *s;
  
  s = stream_to_ptr_check(X(0), 'w', &errcode);
  if (!s)
    BUILTIN_ERROR(errcode,X(0),1);

  DEREF(X(1),X(1));
  if (!IsInteger(X(1)))
    ERROR_IN_ARG(X(1),2,INTEGER)

  writechar(' ',GetInteger(X(1)),s);
  return TRUE;
}

/*----------------------------------------------------------------*/

BOOL skip(Arg)
     Argdecl;
{
  int i, ch;

  DEREF(X(0),X(0));
  if (!TagIsSmall(X(0)) || (i = GetSmall(X(0))) & ~255) /* Not a byte */
    ERROR_IN_ARG(X(0),1,BYTE)

  for (ch=i+1; ch!=i && ch>=-1;)
    ch = readchar(Input_Stream_Ptr,i,address_skip);

  if (ch < -1)
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0)

  return TRUE;
}

/*----------------------------------------------------------------*/


BOOL skip2(Arg)
     Argdecl;
{
  int i, ch;
  struct stream_node *s;
  
  s = stream_to_ptr_check(X(0), 'r', &i);
  if (!s)
    BUILTIN_ERROR(i,X(0),1);

  DEREF(X(1),X(1));
  if (!TagIsSmall(X(1)) || (i = GetSmall(X(1))) & ~255) /* Not a byte */
    ERROR_IN_ARG(X(1),2,BYTE)

  for (ch=i+1; ch!=i && ch>=-1;)
    ch = readchar(s,i,address_skip2);

  if (ch < -1)
    BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),X(0),1)

  return TRUE;
}

/*----------------------------------------------------------------*/

BOOL skip_line(Arg)
     Argdecl;
{
  int ch;

  for (ch=0; ch!=0xa && ch!=0xd && ch>=0;)
    ch = readchar(Input_Stream_Ptr,SKIPLN,address_skip_line);

  if (ch == 0xd) /* Delete a possible 0xa (win end-of-line) */
    readchar(Input_Stream_Ptr,DELRET,address_skip_line);

  return TRUE;
}

/*----------------------------------------------------------------*/


BOOL skip_line1(Arg)
     Argdecl;
{
  int errcode, ch;
  struct stream_node *s;
  
  s = stream_to_ptr_check(X(0), 'r', &errcode);
  if (!s)
    BUILTIN_ERROR(errcode,X(0),1);

  for (ch=0; ch!=0xa && ch!=0xd && ch>=0;)
    ch = readchar(s,SKIPLN,address_skip_line1);

  if (ch == 0xd) /* Delete a possible 0xa (win end-of-line) */
    readchar(s,DELRET,address_skip_line1);

  return TRUE;
}

/*----------------------------------------------------------------*/
/*                       Moved from streams.c (DCG)               */
/*----------------------------------------------------------------*/

/* This is essentially an open-coded fputs().  
   fputs() starts paying off at string lengths above 50 or so.
 */
void print_string(stream,p)
     REGISTER struct stream_node *stream;
     REGISTER char *p;
{
  REGISTER FILE *fileptr = stream->streamfile;
  REGISTER int i;

  if (stream->isatty)
    stream = root_stream_ptr;

  if (stream->streammode != 's')                       /* Is not a socket */
    for (i = *p++; i; i = *p++) {
      putc(i,fileptr);
      INC_COUNTS(i,stream);
    }
  else {
    size_t size = 0;
    char *q = p;

    for (i = *q++; i; i = *q++) {
      INC_COUNTS(i,stream);
      size++;
    }
    write(GetInteger(stream->label), p, size);
  }
  fflush(fileptr);
}


void print_variable(Arg,stream,term)
     Argdecl;
     TAGGED term;
     struct stream_node *stream;
{
  if (IsStackVar(term))
    term = TagHVA(Heap_End+(TagToSVA(term)-Stack_Start));
  print_string(stream, "_");
  number_to_string(Arg,MakeInteger(Arg,TagToPointer(term)-Heap_Start), 10);
  print_string(stream, Atom_Buffer);
}

void print_number(Arg, stream,term)
     Argdecl;
     struct stream_node *stream;
     TAGGED term;
{
  number_to_string(Arg,term, 10);
  print_string(stream, Atom_Buffer);
}

void print_atom(Arg, stream,term)
     Argdecl;
     TAGGED term;
     struct stream_node *stream;
{
  struct atom *atomptr = TagToAtom(term);
  
  if (!atomptr->has_special)
    print_string(stream, atomptr->name);
  else
    {
#if defined(USE_DYNAMIC_ATOM_SIZE)
      char *buf = (char *)checkalloc(2*MAXATOM+3);
#else
      char buf[2*MAXATOM+3]; 
#endif
      REGISTER char *ch = atomptr->name;
      REGISTER char *bp = buf;
      REGISTER int i;
      
      *bp++ = '\'';
      if (atomptr->has_squote)
	while ((i = *ch++))
	  {
	    if (i=='\'' || i=='\\')
	      *bp++ = i;
	    *bp++ = i;
	  }
      else
	while ((i = *ch++))
	  {
	    if (i=='\\')
	      *bp++ = i;
            *bp++ = i;
          }
      *bp++ = '\'';
      *bp++ = 0;
      print_string(stream, buf);
#if defined(USE_DYNAMIC_ATOM_SIZE)
      checkdealloc((TAGGED *)buf, 2*MAXATOM+3);
#endif
    }
}

/*   --------------------------------------------------------------  */	 

void display_term(Arg, term, stream, quoted)
     Argdecl;
     TAGGED term;
     struct stream_node *stream;
     BOOL quoted;
{
  REGISTER TAGGED aux;
  int arity,i;

  switch (TagOf(term)) {
  case LST:
    writechar('[',1,stream);
    DerefCar(aux,term);
    display_term(Arg,aux, stream, quoted);
    DerefCdr(term,term);
    while(TagIsLST(term)) {
      writechar(',',1,stream);
      DerefCar(aux,term);
      display_term(Arg,aux, stream, quoted);
      DerefCdr(term,term);
    }
    if(term!=atom_nil){
      writechar('|',1,stream);
      display_term(Arg,term, stream, quoted);
    }
    writechar(']',1,stream);
    break;
  case STR:
    if (STRIsLarge(term))
      goto number;
    display_term(Arg,TagToHeadfunctor(term),stream, quoted);
    writechar('(',1,stream);
    arity = Arity(TagToHeadfunctor(term));
    for(i=1; i<=arity; i++){
      if(i>1) writechar(',',1,stream);
      DerefArg(aux,term,i);
      display_term(Arg,aux, stream, quoted);
    }
    writechar(')',1,stream);
    break;
  case UBV:
  case SVA:
  case HVA:
  case CVA:
    {
      print_variable(Arg,stream,term);
      break;
    }
  case ATM:
    if (quoted)
      print_atom(Arg,stream,term);
    else
      print_string(stream,TagToAtom(term)->name);
    break;
  case NUM:
  number:
  print_number(Arg,stream,term);
  break;
  }
}

BOOL prolog_display(Arg)
     Argdecl;
{
  DEREF(X(0),X(0));
  display_term(Arg,X(0),Output_Stream_Ptr, FALSE);
  return TRUE;
}

BOOL prolog_display2(Arg)
     Argdecl;
{
  int errcode;
  struct stream_node *stream;
  
  stream = stream_to_ptr_check(X(0), 'w', &errcode);
  if (stream==NULL)
    BUILTIN_ERROR(errcode,X(0),1);

  DEREF(X(1),X(1));
  display_term(Arg,X(1),stream, FALSE);
  return TRUE;
}

BOOL prolog_displayq(Arg)
     Argdecl;
{
  DEREF(X(0),X(0));
  display_term(Arg,X(0), Output_Stream_Ptr, TRUE);
  return TRUE;
}

BOOL prolog_displayq2(Arg)
     Argdecl;
{
  int errcode;
  struct stream_node *stream;
  
  stream = stream_to_ptr_check(X(0), 'w', &errcode);
  if (stream==NULL)
    BUILTIN_ERROR(errcode,X(0),1);

  DEREF(X(1),X(1));
  display_term(Arg,X(1), stream, TRUE);
  return TRUE;
}

BOOL prolog_clearerr(Arg)
     Argdecl;
{
  int errcode;
  struct stream_node *s;
  
  s = stream_to_ptr_check(X(0), 'r', &errcode);
  if (!s)
    BUILTIN_ERROR(errcode,X(0),1);
  
  if (s->streammode != 's') clearerr(s->streamfile);

  return TRUE;
}

/*----------------------------------------------------*/

#define FASTRW_VERSION  'C'
#define FASTRW_MAX_VARS 1024

#define SPACE_FACTOR 64  /* kludge to ensure more heap space before reading */

BOOL prolog_fast_read_in_c_aux(Argdecl,
                               TAGGED *out,
                               TAGGED *vars,
                               int *lastvar);


BOOL prolog_fast_read_in_c(Arg)		/* OPA */
     Argdecl;
{
  int i,lastvar = 0;
  TAGGED term, vars[FASTRW_MAX_VARS];


/* MCL, JC: Changed getc() to readchar() because of wrong assumptions when
   using sockets (i.e., streamfile = NULL.  */

 /* NULL as predaddress (really did not bother to find out what to put)  */

  if ((i = readchar(Input_Stream_Ptr, GET, NULL)) < -1)  
     BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0)
  if (i != FASTRW_VERSION) return FALSE;

  if (HeapDifference(w->global_top,Heap_End) < CONTPAD+SPACE_FACTOR*kCells)
        explicit_heap_overflow(Arg,CONTPAD+SPACE_FACTOR*kCells,1);

  if (!prolog_fast_read_in_c_aux(Arg,&term,vars,&lastvar)) return FALSE;

  return cunify(Arg,X(0),term);
}

#if defined(DEBUG)
#define CHECK_HEAP_SPACE \
  if (HeapDifference(w->global_top,Heap_End) < CONTPAD) \
     fprintf(stderr, "Out of heap space in fast_read()\n")
#else
#define CHECK_HEAP_SPACE
#endif

BOOL prolog_fast_read_in_c_aux(Arg,out,vars,lastvar)
     Argdecl;
     TAGGED *out,*vars;
     int *lastvar;
{
  int i,k,j;
  unsigned char *s = (unsigned char *) Atom_Buffer;
  TAGGED *h = w->global_top;
  int base;

  if ((k = readchar(Input_Stream_Ptr, GET, NULL)) < -1)
     BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0)


  switch(k) {
  case ']':
    *out = atom_nil;
    CHECK_HEAP_SPACE;
    return TRUE;
  case '[':
    w->global_top += 2;
    if (!prolog_fast_read_in_c_aux(Arg,h,vars,lastvar)) return FALSE;
    if (!prolog_fast_read_in_c_aux(Arg,h+1,vars,lastvar)) return FALSE;
    *out = Tag(LST,h);
    CHECK_HEAP_SPACE;
    return TRUE;
  case '_':
  case 'I':
  case 'F':
  case 'A':
  case '"':
  case 'S':
    j = 1;
    for (i=0; j; i++) {
      if (i == Atom_Buffer_Length) {
        /*
	Atom_Buffer = (char *)checkrealloc((TAGGED *)Atom_Buffer,
					   i, Atom_Buffer_Length<<=1);
        */  
        EXPAND_ATOM_BUFFER(Atom_Buffer_Length*2);
	s = (unsigned char *)Atom_Buffer+i;
      }
      if ((j = readchar(Input_Stream_Ptr, GET, NULL)) < -1)
	BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0)
      *s++ = j;
    }
    switch (k) {
    case '_':
      if ((i = atoi(Atom_Buffer)) == *lastvar)
	*h = vars[(*lastvar)++] = TagHVA(w->global_top++);
      *out = vars[i];
      CHECK_HEAP_SPACE;
      return TRUE;
    case 'I':
      base = GetSmall(current_radix);
      if((i = bn_from_string(Atom_Buffer, h, Heap_End-CONTPAD, base))) {
	explicit_heap_overflow(Arg,i+CONTPAD, 1);
        h = w->global_top;        
	if (bn_from_string(Atom_Buffer, h, Heap_End-CONTPAD, base))
	  SERIOUS_FAULT("miscalculated size of bignum");
      }
      if ((i = LargeArity(h[0])) ==2 && IntIsSmall((int)h[1]))
	*out = MakeSmall(h[1]);
      else {
	*out = Tag(STR,h);
	w->global_top += i+1;
	h[i] = h[0];
      }	
      CHECK_HEAP_SPACE;
      return TRUE;
    case 'F':
      *out = MakeFloat(Arg,atof(Atom_Buffer));
      CHECK_HEAP_SPACE;
      return TRUE;
    case 'A':
      *out = MakeString(Atom_Buffer);
      CHECK_HEAP_SPACE;
      return TRUE;
    case '"':
      i--;
      /*
      if (HeapDifference(w->global_top,Heap_End)<CONTPAD+(i<<1)){
        printf("Prev HeapDifference is %d\n",
               HeapDifference(w->global_top,Heap_End));
        explicit_heap_overflow(Arg,CONTPAD+(i<<1),1);
      }
      */
      while (i--) MakeLST(*out,MakeSmall(((unsigned char *) Atom_Buffer)[i]),*out);
      if (!prolog_fast_read_in_c_aux(Arg,h+1,vars,lastvar)) return FALSE;
      CHECK_HEAP_SPACE;
      return TRUE;
    case 'S':
      if ((i = readchar(Input_Stream_Ptr, GET, NULL)) < -1)
	BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0)
          /*
      if (HeapDifference(w->global_top,Heap_End)<CONTPAD+(i+1))
        explicit_heap_overflow(Arg,CONTPAD+(i+1),1);
          */
      *h = SetArity(MakeString(Atom_Buffer),i);
      *out = Tag(STR,h++);
      w->global_top += i+1;
      while(i--)
	if (!prolog_fast_read_in_c_aux(Arg,h++,vars,lastvar)) return FALSE;
      CHECK_HEAP_SPACE;
      return TRUE;
    }
  default:
    return FALSE;
  }
}

void prolog_fast_write_in_c_aux(Argdecl,
                                TAGGED in,
                                TAGGED *vars, 
                                int *lastvar);

BOOL prolog_fast_write_in_c(Arg)		/* OPA */
     Argdecl;
{
  TAGGED vars[FASTRW_MAX_VARS];
  int lastvar = 0;

  DEREF(X(0),X(0));
  writechar(FASTRW_VERSION,1,Output_Stream_Ptr);
  prolog_fast_write_in_c_aux(Arg,X(0),vars,&lastvar);
  return TRUE;
}

void prolog_fast_write_in_c_aux(Arg,in,vars,lastvar)
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
      if (TagIsSmall(term) && (i = GetSmall(term)))
	if ((i > 0) && (i < 256)) {
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
	      prolog_fast_write_in_c_aux(Arg,in,vars,lastvar);
	      return;
	    }	  
	  }
	  writechar(0,1,Output_Stream_Ptr);
	}
      writechar('[',1,Output_Stream_Ptr);
      prolog_fast_write_in_c_aux(Arg,term,vars,lastvar);
      prolog_fast_write_in_c_aux(Arg,in,vars,lastvar);
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
/*snprintf((char *) Atom_Buffer,4,"%i",i);*/
/* We are not checking the result, anyway... */
      sprintf((char *) Atom_Buffer,"%i",i); 
      print_string(Output_Stream_Ptr,Atom_Buffer);
      writechar(0,1,Output_Stream_Ptr);
      return;
    case STR:
      if (!STRIsLarge(in)) {
      writechar('S',1,Output_Stream_Ptr);
      print_string(Output_Stream_Ptr,TagToAtom(TagToHeadfunctor(in))->name);
      writechar(0,1,Output_Stream_Ptr);
      writechar(j = Arity(TagToHeadfunctor(in)),1,Output_Stream_Ptr);
      for(i = 1; i <= j; prolog_fast_write_in_c_aux(Arg,term,vars,lastvar))
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

/*----------------------------------------------------*/

/* Routines for the compression and uncompression of bytecode, used on 
   the CIAO compiler (OPA) */ 

unsigned char sizeLZ(int n)
{ if (n > 2047) return 12;
  else if (n > 1023) return 11;
  else if (n > 511) return 10;
  else return 9;}

void outLZ(Arg,Buffer,BufferSize,Code,size)
     Argdecl;
     int *Buffer, Code;
     char *BufferSize;
     unsigned char size;
{ Buffer[0] += Code*(1<<(BufferSize[0]));
  for(BufferSize[0] += size; BufferSize[0] >= 8; BufferSize[0] -= 8) {
   writechar(Buffer[0] % 256,1,Output_Stream_Ptr);
   Buffer[0] /= 256;}}

BOOL compressLZ(Arg)
     Argdecl;
{ char *Dict[4096];
  char *First;
  char Vault[200000];
  char CarrySize = 0;
  int  i;
  int  Carry = 0;  
  int  Last = 256;
  int  PrefixSize = 0;
  int  Entry = 0;
  int  Size[4096];
  struct stream_node *s;
  
  s = stream_to_ptr_check(X(0), 'r', &i);
  if (!s) BUILTIN_ERROR(i,X(0),1);

  writechar(12,1,Output_Stream_Ptr);

  for (i = 0; i < 257; Size[i++] = 1) 
      { Dict[i] = &Vault[i];
        Vault[i] = i % 256; }
  First = &Vault[256];

  while((i = getc(s->streamfile)) != EOF) {
    if (i < -1) BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0)
    First[PrefixSize++] = i;
    for (i = Entry; Entry <= Last; Entry++) 
      if ((Size[Entry] == PrefixSize) && (Dict[Entry][0] == First[0])
          && !(memcmp(&Dict[Entry][1],&First[1],PrefixSize-1))) break;
    if (Entry > Last) {
      Entry = First[PrefixSize-1];
      outLZ(Arg,&Carry,&CarrySize,i,sizeLZ(Last));
      if (Last == 4095) First = &Vault[Last = 256];
      else {
	Dict[++Last] = First;
	Size[Last] = PrefixSize;
	First += PrefixSize; }
      First[0] = Entry;
      PrefixSize = 1;}}

  if (PrefixSize) outLZ(Arg,&Carry,&CarrySize,Entry,sizeLZ(Last));
  outLZ(Arg,&Carry,&CarrySize,256,sizeLZ(Last));
  outLZ(Arg,&Carry,&CarrySize,0,7);
  return TRUE;}


BOOL inLZ(Arg,s,Buffer,BufferSize,Code,size)
     Argdecl;
     int *Buffer, *Code;
     char *BufferSize, size;
     struct stream_node *s;
{ int i;
 
 for (; BufferSize[0] < size; BufferSize[0] += 8) {
   if ((i = getc(s->streamfile)) < -1)
     BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0)
   Buffer[0] += ((unsigned char) i)*(1<<BufferSize[0]);}
 Code[0] = Buffer[0] % (1<<size);
 Buffer[0] /= (1<<size);
 BufferSize[0] -= size;
 return TRUE;}    
      
BOOL copyLZ(Arg)
     Argdecl;
{ int  i;
  int  Last = 256;
  int  PrefixSize = 1;
  int  Carry = 0;
  char CarrySize = 0;
  char *Dict[4096];
  int  Size[4096];
  char *First;
  char Vault[200000];
  struct stream_node *s;
  
  s = stream_to_ptr_check(X(0), 'r', &i);
  if (!s) BUILTIN_ERROR(i,X(0),1);

  if ((i = getc(s->streamfile)) < -1)
	 BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0)
  
  if (i != 12) {
    while (i != EOF) {
      writechar(i,1,Output_Stream_Ptr);
      if ((i = getc(s->streamfile)) < -1)
	BUILTIN_ERROR(PERMISSION_ERROR(ACCESS, PAST_END_OF_STREAM),atom_nil,0)}
    return TRUE;}
  else {
    for (i = 0; i < 257; Size[i++] = 1) {
      Dict[i] = &Vault[i];
      Vault[i] = i % 256;}
    First = &Vault[256];
 
    if (inLZ(Arg,s,&Carry,&CarrySize,&i,9) != TRUE) return FALSE;
    First[0] = i;
    while(1) {
      for (i = 0; i < PrefixSize;) writechar(First[i++],1,Output_Stream_Ptr);
      if (inLZ(Arg,s,&Carry,&CarrySize,&i,sizeLZ(++Last % 4096)) != TRUE)
	return FALSE;;
      if (i == 256) return TRUE;
      if (Last == 4096) {
        (First = &Vault[Last = 256])[0] = i;
	PrefixSize = 1;}
      else {
	Size[Last] = PrefixSize+1;            
	(Dict[Last] = First)[PrefixSize] = Dict[i][0];
	bcopy(Dict[i],First += Size[Last],PrefixSize = Size[i]);}}}
  return FALSE;}
