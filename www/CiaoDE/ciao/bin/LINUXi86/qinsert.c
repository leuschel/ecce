
#include <stdlib.h>

#include "threads.h"
#include "datadefs.h"
#include "support.h"
#include "qinstrdefs.h"

/* declarations for global functions accessed here */

#include "qinsert_defs.h"
#include "bignum_defs.h"
#include "stacks_defs.h"
#include "start_defs.h"
#include "alloc_defs.h"

/* local declarations */

static int ql_getc(struct qlstream *s);
static int qlgetshort(struct qlstream *f);
static ENG_INT qlgetlong(struct qlstream *f);
static char *qlgetstring(struct qlstream *f);
static TAGGED qlgetlarge(register struct worker *w, struct qlstream *f);
static ENG_FLT qlgetdouble(struct qlstream *f);
static void ql_load_dbnode(register struct worker *w, int Li, struct qlstream *f, int codelength, int counter_cnt);


#define QL(I)	qlarray[I]
#define QLCHECK(I) \
{ if ((I)+qloffset >= qllimit) expand_qload(); }

struct qlinfo {
    struct qlinfo *next;
    TAGGED *qlarray;
    int qloffset;
    int qllimit;
};

extern char workstring[];                                   /*  In qget.c */
extern TAGGED *qlarray;                                     /* In qread.c */
extern int qloffset, qllimit;                               /* In qread.c */
extern struct qlinfo *qlstack;                              /* In qread.c */
extern struct emul_info *latest_bytecode;                   /* In qread.c */
extern int latest_bytecode_size;                            /* In qread.c */


/*
extern int getshort PROTO((FILE *file));
extern TAGGED getlarge PROTO((FILE *file));
extern ENG_INT getlong PROTO((FILE *file));
extern ENG_FLT getdouble PROTO((FILE *file));
extern char *getstring PROTO((FILE *file));
*/

extern void reloc_pointer();
extern void reloc_emul_entry();
extern void reloc_counter();
extern void expand_qload();


              /* Simluate a getc from a stream of QL codes */


#define GETC(F) ql_getc(F)

static int ql_getc(s)
     struct qlstream *s;
{
  int return_code;

  if (s->qlremains == 0)
    return_code = (int)EOF;
  else {
    s->qlremains--;
    return_code = (int)*(s->qlpointer);
    s->qlpointer++;
  }
  return return_code;
}


static int qlgetshort(f)
     struct qlstream *f;
{
  register char *ws;
  
  for(ws = workstring; (*ws++ = GETC(f));)
    ;

  return atoi(workstring);
}


static ENG_INT qlgetlong(f)
     struct qlstream *f;
{
  register char *ws;
  /*extern ENG_INT atol PROTO((char *str)); */
  
  for(ws = workstring; (*ws++ = GETC(f));)
    ;
  return atol(workstring);
}

static char *qlgetstring(f)
     struct qlstream *f;
{
  register char *ws;
  
  for(ws = workstring; (*ws++ = GETC(f));)
    ;
  return workstring;
}

static TAGGED qlgetlarge(Arg,f)
     Argdecl;
     struct qlstream *f;
{
  register int i;
  register char *ws;

  for (i=0; TRUE;) {
    ws = Atom_Buffer;
    while (i<Atom_Buffer_Length)
      if (!(ws[i++] = GETC(f)))
        goto out;
    Atom_Buffer = (char *)checkrealloc((TAGGED *)ws,
                                       i, Atom_Buffer_Length<<=1);
  }
 out:
  if (bn_from_string(ws,w->global_top,Heap_End,GetSmall(current_radix))) {
    SERIOUS_FAULT("$qload: miscalculated heap usage");
  } else  {
    TAGGED *h = w->global_top;
    int ar = LargeArity(h[0]);
    
    if (ar==2 && IntIsSmall((int)h[1]))
      return MakeSmall(h[1]);
    else{
      w->global_top += ar+1;
      h[ar] = h[0];
      return Tag(STR,h);
    }
  }
}


static ENG_FLT qlgetdouble(f)
     struct qlstream *f;
{
  register char *ws;
  /*extern ENG_FLT atof PROTO((char *str));*/
  
  for(ws = workstring; (*ws++ = GETC(f));)
    ;
  return atof(workstring);
}

void qlgetbytecode(Arg,f,insn_p,length)
     Argdecl;
     struct qlstream *f;
     INSN *insn_p;
     int length;
{
  register char c;
  /*extern ENG_INT atol PROTO((char *str)); */
  /*extern ENG_FLT atof PROTO((char *str));*/

  while ((c=GETC(f))) {
    switch (c) {
    case 'G': {
      register int i;
      register char *ws;
      BOOL floatp = FALSE;
      TAGGED *wp = (TAGGED *)insn_p;
      
      for (i=0; TRUE;) {
        ws = Atom_Buffer;
        while (i<Atom_Buffer_Length)
          if (!(ws[i++] = c = GETC(f)))
            goto out;
          else if (c == '.')
            floatp = TRUE;
        Atom_Buffer = (char *)checkrealloc((TAGGED *)ws,
                                           i, Atom_Buffer_Length<<=1);
      } 
    out:
      if (floatp) {
        ENG_FLT f = atof(ws);
        TAGGED *fp = (TAGGED *)(&f);
        
        *wp++ = MakeFunctorFloat;
        *wp++ = fp[0];
        *wp++ = fp[1];
        insn_p += 6;
      } else {
        bn_from_string(ws,
                       (Bignum *)insn_p,
                       (Bignum *)((char *)insn_p+length),
		       GetSmall(current_radix));
        insn_p += LargeArity(*wp)<<1;
      }
      break;
    }
      
    case '+': {
      register char *ws = workstring;
      
      while ((*ws++ = GETC(f)))
        ;
      *(ENG_INT *)insn_p = atol(workstring);
      insn_p += BPL;
      break;
    }
      
    case 'C': {
      register char *ws = workstring;
      
      while ((*ws++ = GETC(f)))
        ;
      *(CInfo *)insn_p = builtintab[atoi(workstring)];
      insn_p += BPTP;
      break;
    }
      
    case 'F': {
      register char *ws = workstring;
      
      while ((*ws++ = GETC(f)))
        ;
      /* Was:
       *insn_p++ = (INSN)builtintab[atoi(workstring)]);
       ant it issued a mesage since INSN is short int and *builtintab
       is a pointer to a function */
      *insn_p++ = (INSN)((unsigned long int)builtintab[atoi(workstring)]);
      break;
    }
    case 'D': {
      register char *ws = workstring;
      
      while ((*ws++ = GETC(f)))
        ;
      *(CInfo *)insn_p = builtintab[atoi(workstring)];
      ws = workstring;
      while ((*ws++ = GETC(f)))
        ;
      *(long *)insn_p += atol(workstring);
      insn_p += BPTP;
      break;
    }
      
    case 'E': {
      register char *ws = workstring;
      
      while ((*ws++ = GETC(f)))
        ;
      *(unsigned long *)insn_p =
        (unsigned long)builtintab[atoi(workstring)] 
          - (unsigned long)insn_p;
      insn_p += BPTP;
      break;
    }
      
    default: {
      register char *ws = workstring;
      
      *ws++ = c;
      while ((*ws++ = GETC(f)))
        ;
      *insn_p++ = atoi(workstring);
    }
    }
  }
}

static void ql_load_dbnode(Arg,Li,f,codelength,counter_cnt)
     Argdecl;
     int Li,codelength,counter_cnt;
     struct qlstream *f;
{
#if GAUGE
  int lsize = (sizeof(struct emul_info)-ANY*sizeof(INSN)+
	       codelength+
	       counter_cnt*sizeof(ENG_INT)+3) & ~3;
#else
  int lsize = sizeof(struct emul_info)-ANY*sizeof(INSN)+codelength;
#endif
  struct emul_info *db;

  db = (struct emul_info *)checkalloc(lsize);

  qlgetbytecode(Arg,f,db->emulcode,codelength);
  latest_bytecode = db;  
  latest_bytecode_size = codelength;
  db->next = NULL;
  db->objsize = lsize;
  db->subdefs = NULL;
#if GAUGE
  db->counters = (ENG_INT *)((char *)db+lsize)-counter_cnt;
  for (i=0; i<counter_cnt; i++)
    db->counters[i] = 0;
#endif
  QL(Li) = PointerToTerm(db);
}

BOOL qlinsert(Arg,qlcode,rungoal)
     Argdecl;
     struct qlstream *qlcode;
     TAGGED *rungoal;
{
  register int Li = 0, Lj = 0;
  register TAGGED *h = w->global_top;
  long pad;
  int c = GETC(qlcode);
  
  while (c!=EOF) {
    switch (c) {
    case ENSURE_SPACE:
      if (HeapDifference(h,Heap_End) < (pad=qlgetlong(qlcode)))
        w->global_top = h,
        explicit_heap_overflow(Arg,pad,2),
        h = w->global_top;
      break;
    case LOAD_ATOM:
      Li = qlgetshort(qlcode);
      QLCHECK(Li);
      QL(Li) = init_atom_check(qlgetstring(qlcode));
      break;
    case LOAD_FUNCTOR:
      Li = qlgetshort(qlcode);
      Lj = qlgetshort(qlcode);
      QLCHECK(Li);
      QLCHECK(Lj);
      QL(Li) = SetArity(QL(Lj),qlgetshort(qlcode));
      break;
    case LOAD_NUMBER_S:
      Li = qlgetshort(qlcode);
      QLCHECK(Li);
      QL(Li) = MakeSmall(qlgetshort(qlcode));
      break;
    case LOAD_NUMBER_L:
      Li = qlgetshort(qlcode);
      QLCHECK(Li);
      w->global_top = h;
      QL(Li) = qlgetlarge(Arg,qlcode);
      h = w->global_top;
      break;
    case LOAD_NUMBER_F:
      Li = qlgetshort(qlcode);
      QLCHECK(Li);
      w->global_top = h;
      QL(Li) = MakeFloat(Arg,qlgetdouble(qlcode));
      h = w->global_top;
      break;
    case LOAD_VARIABLE:
      Li = qlgetshort(qlcode);
      QLCHECK(Li); 
      LoadHVA(QL(Li),h);
      break;
    case LOAD_NIL:
      Li = qlgetshort(qlcode);
      QLCHECK(Li);
      QL(Li) = atom_nil;
      break;
    case LOAD_LIST:
      Li = qlgetshort(qlcode);
      QLCHECK(Li);
      QL(Li) = Tag(LST,h);
      break;
    case LOAD_TUPLE:
      Li = qlgetshort(qlcode);
      QLCHECK(Li);
      QL(Li) = Tag(STR,h);
      break;
    case LOAD_ARGUMENT:
      Li = qlgetshort(qlcode);
      QLCHECK(Li);
      HeapPush(h,QL(Li));
      break;
    case LOAD_DBNODE:
      Li = qlgetshort(qlcode);
      QLCHECK(Li);
      Lj = qlgetshort(qlcode);
      ql_load_dbnode(Arg,Li,qlcode,Lj,qlgetshort(qlcode));
      break;
    case RETURN:
      Li = qlgetshort(qlcode);
      *rungoal = QL(Li);
      w->global_top = h;
      return TRUE;
    case RELOC_POINTER:
      Li = qlgetshort(qlcode);
      reloc_pointer(Li,qlgetlong(qlcode));
      break;
    case RELOC_EMUL_ENTRY:
      Li = qlgetshort(qlcode);
      reloc_emul_entry(Li,qlgetlong(qlcode));
      break;
    case RELOC_COUNTER:
      reloc_counter(qlgetlong(qlcode));
      break;
    }
    c=GETC(qlcode);
  }
  w->global_top = h;
  return FALSE;
}

