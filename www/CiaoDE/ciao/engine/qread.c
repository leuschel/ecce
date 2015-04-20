/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */

#include "datadefs.h"
#include "support.h"
#include "qinstrdefs.h"

/* declarations for global functions accessed here */

#include "qread_defs.h"
#include "alloc_defs.h"
#include "qget_defs.h"
#include "stacks_defs.h"

#if defined(ALLOW_COMPRESSED_CODE)
#if defined(LINUX) || defined(Win32)
#include <string.h>
#else
#include <strings.h>
#endif
#endif

/* local declarations */

static void load_dbnode(Argdecl, int Li, FILE *f, int codelength, int counter_cnt);


#define QL(I)	qlarray[I]
#define QLCHECK(I) \
{ if ((I)+qloffset >= qllimit) expand_qload(); }

struct qlinfo {
    struct qlinfo *next;
    TAGGED *qlarray;
    int qloffset;
    int qllimit;
};

#if defined(BUFFERED_PO)

#define QLBFSIZE 1024
int  qlbuffidx, qlbuffend;
unsigned char qlbuff[QLBFSIZE];

/*
  Use an internal buffer of QLBFSIZE chars to store the contents of the .po
  files being read in.  When the buffer is full, we fill it again at once;
  the previous method was calling getc() once and again.  Preliminary tests
  show this method to be between 3 times (for dynamic executables, as
  ciaosh) to 5 times (for static stuff, as ciaoc) faster.  
*/

int buffered_input(stream)
     FILE *stream;
{
  if (qlbuffidx == qlbuffend) {
    if (qlbuffend < QLBFSIZE) return EOF;
    if (!(qlbuffend = 
          fread(qlbuff, sizeof(unsigned char), QLBFSIZE, stream)))
        return EOF;                /* Could not read after buffer emptied */
    qlbuffidx = 0;
  } 
  return (int)qlbuff[qlbuffidx++];
}

#endif

#if defined(ALLOW_COMPRESSED_CODE)

int  Last, PrefixSize, Size[4096], Buffer[3840], BufferP;
char *Dict[4096], *First, Vault[200000], compressed, remaining;

/* Allows the load of compressed bytecode. The compression algorithm used is
   based on Lempel-Ziv, reducing bytecode size to about 1/3 and varying load
   time to about 125% when using buffered input and to less than 60% when using
   unbuffered one (note: even thorough this, the load of compressed bytecode
   is faster when buffered input is enabled). Files containing compressed
   bytecode are recognized because they begin by ^L the bytecode sequence. */

#define InLZ(n,n2) { for (; size < n; size += 8) \
	                  i += GETC_LZ(stream)*(1<<size); \
                     Buffer[--BufferP] = i % n2; \
      	             i /= n2; \
                     size -= n; }    
      
int readLZ(FILE *stream)
{ 
  int  i = 0;
  char size = 0;

  if (compressed) {
    if (remaining)
      return (int)First[PrefixSize-remaining--];  
    if (!BufferP) { 
      BufferP = 3840;
      while (BufferP > 3584)
        InLZ(9,512);
      while (BufferP > 3072)
        InLZ(10,1024);	  
      while (BufferP > 2048)
        InLZ(11,2048);
      while (BufferP) 
        InLZ(12,4096);
      BufferP = 3840; 
    }
    if ((i=Buffer[--BufferP]) == 256) 
      return EOF;
    if (Last == 4095) 
      (First = &Vault[Last = 256])[0] = i;
    else { 
      Size[++Last] = PrefixSize+1;            
      (Dict[Last] = First)[PrefixSize] = Dict[i][0];
      bcopy(Dict[i],First += Size[Last],Size[i]);
    }
    remaining = (PrefixSize=Size[i])-1;
    return (int)First[0];
  }
  else return GETC_LZ(stream);}

#endif                                                                                                

/*
extern int getshort PROTO((FILE *file));
extern TAGGED getlarge PROTO((Argdecl, FILE *file));
extern ENG_INT getlong PROTO((FILE *file));
extern ENG_FLT getdouble PROTO((FILE *file));
extern char *getstring PROTO((FILE *file));
*/

TAGGED *qlarray=NULL;                   /* Shared, but with locked access */
int qloffset=0, qllimit=0;                       /* Shared, locked access */
struct qlinfo *qlstack=NULL;                     /* Shared, locked access */

BOOL push_qlinfo(Arg)
     Argdecl;
{
  struct qlinfo *p = (struct qlinfo *)checkalloc(sizeof(struct qlinfo));
  
#if defined(BUFFERED_PO)
  qlbuffidx = QLBFSIZE; /* Empty */
  qlbuffend = QLBFSIZE; 
#endif  
#if defined(ALLOW_COMPRESSED_CODE)
  compressed = 0;
#endif

  p->next = qlstack;
  qlstack = p;
  p->qllimit = qllimit;
  p->qloffset = qloffset;
  p->qlarray = qlarray;
  qllimit = QLOADSIZE;
  qloffset = qllimit>>1;
  qlarray = checkalloc(qllimit*sizeof(TAGGED))+qloffset;
  
  return TRUE;
}

BOOL pop_qlinfo(Arg)
    Argdecl;
{
    struct qlinfo *p = qlstack;

    qlstack = p->next;
    checkdealloc(qlarray-qloffset, qllimit*sizeof(TAGGED));
    qlarray = p->qlarray;
    qllimit = p->qllimit;
    qloffset = p->qloffset;
    checkdealloc((TAGGED *)p, sizeof(struct qlinfo));

    return TRUE;    
}

void expand_qload()
{
  REGISTER int i;
  REGISTER int o = qloffset;
  /*ENG_INT prog_mem = mem_prog_count;*/ /* preserve over reallocs */
  
  qlarray = checkrealloc(qlarray-o,
			 qllimit*sizeof(TAGGED),
			 qllimit*2*sizeof(TAGGED));
  for (i=qllimit; i>0;)
    --i, qlarray[i+o]=qlarray[i];
  qllimit<<=1;
  qloffset<<=1;
  qlarray+=qloffset;
  /*mem_prog_count = prog_mem;*/
}

struct emul_info *latest_bytecode;               /* Shared, locked access */
int latest_bytecode_size;                        /* Shared, locked access */

static void load_dbnode(Arg,Li,f,codelength,counter_cnt)
     Argdecl;
     int Li,codelength,counter_cnt;
     FILE *f;
{
#if defined(GAUGE)
  int lsize = (sizeof(struct emul_info)-sizeof(INSN)+
	       codelength+
	       counter_cnt*sizeof(ENG_INT)+3) & ~3;
#else
  int lsize = sizeof(struct emul_info)-sizeof(INSN)+codelength;
#endif
  struct emul_info *db;
  /*int i;*/
  
  db = (struct emul_info *)checkalloc(lsize);

  getbytecode(Arg,f,db->emulcode,codelength);
  latest_bytecode = db;  
  latest_bytecode_size = codelength;
  db->next = NULL;
  db->objsize = lsize;
  db->subdefs = NULL;
#if defined(GAUGE)
  db->counters = (ENG_INT *)((char *)db+lsize)-counter_cnt;
  for (i=0; i<counter_cnt; i++)
    db->counters[i] = 0;
#endif
  QL(Li) = PointerToTerm(db);
}

void reloc_pointer(Li,Label)
     int Li;
     ENG_INT Label; 
{
  REGISTER char *pos;
  
  do
    {
      pos = (char *)latest_bytecode->emulcode + Label;
      Label = *(ENG_INT *)pos;
      *(ENG_INT *)pos = QL(Li);
    }
  while(Label != 0);
}

/* Patch up the counter indexes in the BUMP_COUNTER instructions. */
void reloc_counter(Label)
     ENG_INT Label;
{
#if GAUGE
  REGISTER char *position;
  int counter_cnt = NumberOfCounters(latest_bytecode);
  REGISTER ENG_INT *current_counter = latest_bytecode->counters + counter_cnt;

  /*  
    Counters are linked together in REVERSE order.  Since
    make_bytecode_object assigns counters in order of occurrence,
    we must do the same here.
   */
  while (Label)
    {
      position = (char *)&latest_bytecode->emulcode[0] + Label;
      Label = *(ENG_INT *)position;
      *(ENG_INT **)position = --current_counter;
      --counter_cnt;
    }
  if (counter_cnt != 2)
    SERIOUS_FAULT("$qload: counter counts don't match");
#endif
}

void reloc_emul_entry(Li,Label)
     int Li;
     ENG_INT Label;
{
  REGISTER char *pos;
  short *addr = &parse_definition(QL(Li))->enter_instr;

  do {
    pos = (char *)latest_bytecode->emulcode + Label;
    Label = *(ENG_INT *)pos;
    *(short **)pos = addr;
  }
  while(Label != 0);
}


BOOL qread1(Arg,qfile,rungoal)
     Argdecl;
     FILE *qfile;
     TAGGED *rungoal;
{
  register int Li = 0, Lj = 0;
  register TAGGED *h = w->global_top;
  long pad;
  int c = GETC(qfile);
  
  while (c!=EOF)
    {
      switch (c)
	{
#if defined(ALLOW_COMPRESSED_CODE)
	case ISCOMPRESSED:
	  compressed = 1;
	  Last = 4095;
	  remaining = BufferP = 0;
	  { 
	    int i = 0;
	    for(;i < 257; Size[i++] = 1) (Dict[i]=&Vault[i])[0]= i%256;
	  }
	  break;
#endif
	case ISSCRIPT:
	  {
	    int chr;	    
	    do chr = GETC(qfile);
	    while ((chr != EOF) && (chr != 12));
	  }
          break;
	case ENSURE_SPACE:
	  if (HeapDifference(h,Heap_End) < (pad=getlong(qfile))){
	    w->global_top = h;
	    explicit_heap_overflow(Arg,pad,2);
	    h = w->global_top;
          }
	  break;
	case LOAD_ATOM:
	  Li = getshort(qfile);
	  QLCHECK(Li);
	  QL(Li) = init_atom_check(getstring(Arg, qfile));
	  break;
	case LOAD_FUNCTOR:
	  Li = getshort(qfile);
	  Lj = getshort(qfile);
	  QLCHECK(Li);
	  QLCHECK(Lj);
	  QL(Li) = SetArity(QL(Lj),getshort(qfile));
	  break;
	case LOAD_NUMBER_S:
	  Li = getshort(qfile);
	  QLCHECK(Li);
	  QL(Li) = MakeSmall(getshort(qfile));
	  break;
	case LOAD_NUMBER_L:
	  Li = getshort(qfile);
	  QLCHECK(Li);
	  w->global_top = h;
	  QL(Li) = getlarge(Arg,qfile);
	  h = w->global_top;
	  break;
	case LOAD_NUMBER_F:
	  Li = getshort(qfile);
	  QLCHECK(Li);
	  w->global_top = h;
	  QL(Li) = MakeFloat(Arg,getdouble(qfile));
	  h = w->global_top;
	  break;
	case LOAD_VARIABLE:
	  Li = getshort(qfile);
	  QLCHECK(Li); 
	  LoadHVA(QL(Li),h);
	  break;
	case LOAD_NIL:
	  Li = getshort(qfile);
	  QLCHECK(Li);
	  QL(Li) = atom_nil;
	  break;
	case LOAD_LIST:
	  Li = getshort(qfile);
	  QLCHECK(Li);
	  QL(Li) = Tag(LST,h);
	  break;
	case LOAD_TUPLE:
	  Li = getshort(qfile);
	  QLCHECK(Li);
	  QL(Li) = Tag(STR,h);
	  break;
	case LOAD_ARGUMENT:
	  Li = getshort(qfile);
	  QLCHECK(Li);
	  HeapPush(h,QL(Li));
	  break;
	case LOAD_DBNODE:
	  Li = getshort(qfile);
	  QLCHECK(Li);
	  Lj = getshort(qfile);
	  load_dbnode(Arg,Li,qfile,Lj,getshort(qfile));
	  break;
	case RETURN:
	  Li = getshort(qfile);
	  *rungoal = QL(Li);
	  w->global_top = h;
	  return TRUE;
	case RELOC_POINTER:
	  Li = getshort(qfile);
	  reloc_pointer(Li,getlong(qfile));
	  break;
	case RELOC_EMUL_ENTRY:
	  Li = getshort(qfile);
	  reloc_emul_entry(Li,getlong(qfile));
	  break;
      case RELOC_COUNTER:
          reloc_counter(getlong(qfile));
          break;
	}
      c=GETC(qfile);
      }
  w->global_top = h;
  return FALSE;
}

BOOL prolog_qread(Arg)
     Argdecl;
{
  struct stream_node *s;
  TAGGED goal;

  if ((s = stream_to_ptr(X(0), 'r')) != NULL)
    if (qread1(Arg,s->streamfile, &goal))
      return cunify(Arg,goal,X(1));

  Unify_constant(MakeSmall(-1),X(1));
  return TRUE;

}

