/* Copyright (C) 1996,1997,1998, 1999, 2000, 2001, 2002  UPM-CLIP */

#include "datadefs.h"
#include "support.h"
#include "predtyp.h"

/* declarations for global functions accessed here */

#include "locks_defs.h"
#include "indexing_defs.h"
#include "wamsupport_defs.h"
#include "support_defs.h"
#include "alloc_defs.h"
#include "objareas_defs.h"


/* local declarations */


static void set_nondet(struct try_node *t,
                       struct incore_info *def,
                       BOOL first);
static void incore_insert(struct try_node  **t0, 
                          int effar,
                          struct emul_info *ref,
                          struct incore_info *def);
static void incore_puthash(struct sw_on_key **psw, 
                           int effar, 
                           struct emul_info *current, 
                           struct incore_info *def,
                           TAGGED k);
static struct try_node *incore_copy(struct try_node *from);
static void free_try(struct try_node **t);
static void free_sw_on_key(struct sw_on_key **sw);
static void free_emulinfo(register struct emul_info *cl);
static void free_incoreinfo(register struct incore_info **p);
static void make_undefined(Argdecl, struct definition *f);
static void free_info(int insn, char *info);
static void init_interpreted(register struct definition *f);


#define ISNOTRY(T)		(((T)==NULL) || ((T)==fail_alt))

/* Indexing for the incore compiler. */


/* Patch bytecode information related to the last clause inserted */

static void set_nondet(t,def,first)
     REGISTER struct try_node *t;
     struct incore_info *def;
     BOOL first;
{
  REGISTER unsigned int i;
  REGISTER struct emul_info *cl;

    /* Check if we can use the last cached clause and number to insert */

#if defined(CACHE_INCREMENTAL_CLAUSE_INSERTION)

/* If this is activated, patching is sped up by caching the last insertion
  peformed, and using the cache not to advance in the chain of clauses from
  the beginning.  Inserting always at the end is simply not possible because
  the clause numbers to be accessed do not come ordered. The cache is used
  only if we are inserting farther than the last clause inserted. 

  We should check that the clause numbers have not changed --- i.e., that
  intermediate records are not erased --- as that would invalidate our
  count.
*/

  if (t->number >= def->last_inserted_num){/* farther than last insertion */
    cl = def->last_inserted_clause;
    i  = t->number - def->last_inserted_num;
  } else {
    i  = t->number - 1;
    cl = def->clauses;
  }

  for( ; i; i--)/* Skip until end of chain --- it is not NULL terminated! */
    cl = cl->next;

  def->last_inserted_clause = cl;
  def->last_inserted_num    = t->number;
#else
  for (i=t->number, cl=def->clauses; --i;) /* 1-based numbers */
    cl = cl->next;
#endif

 /* Patch previous emul_p "fail_alt" code */
  t->emul_p = cl->emulcode+1;
  if (first)			/* maintain emul_p2 optimization */
        t->emul_p2 = t->emul_p + p2_offset(*t->emul_p);

}


static void incore_insert(t0,effar,ref,def)
     REGISTER struct try_node **t0;
     int effar;
     struct emul_info *ref;
     struct incore_info *def;
{
  REGISTER struct try_node **t1 = t0;
  REGISTER struct try_node *t;

  /* Init the try_node to insert. */
  t = (struct try_node *)checkalloc(sizeof(struct try_node));
  t->node_offset = ArityToOffset(effar);
 /* Last "next" is num. of clauses: we are inserting at the end of the chain */
  t->number = (unsigned int)ref->next;
  t->emul_p = 
    (INSN *)(*ref->emulcode + (char *)ref->emulcode);/* initial p: det case */
#if defined(GAUGE)
  t->entry_counter = ref->counters;
#endif
  t->next = NULL;

  if (ISNOTRY(*t1))
    t->emul_p2 = t->emul_p + p2_offset(*t->emul_p);
  else {
    do {
      if ((*t1)->next==NULL)
        set_nondet(*t1,def,t0==t1);
      t1 = &(*t1)->next;
    } while (!ISNOTRY(*t1));
  }
  (*t1) = t;
}

static struct try_node *incore_copy(from)
     struct try_node *from;
{
  struct try_node *tcopy = fail_alt;
  struct try_node **to = &tcopy;

  for (from=from; !ISNOTRY(from); from=from->next) {
    (*to) = (struct try_node *)checkalloc(sizeof(struct try_node));
    (*to)->node_offset = from->node_offset;
    (*to)->number = from->number;
    (*to)->emul_p = from->emul_p;
    (*to)->emul_p2 = from->emul_p2;
    (*to)->next = NULL;
#if defined(GAUGE)
    (*to)->entry_counter = from->entry_counter;
#endif
    to = &(*to)->next;
  }

  return tcopy;
}

/* get location of try chain for a key */
struct sw_on_key_node *incore_gethash(sw,key)
     REGISTER struct sw_on_key *sw;
     TAGGED key;
{
  REGISTER struct sw_on_key_node *hnode;
  REGISTER int i;
  REGISTER TAGGED t0;

  for (i=0, t0=key & sw->mask;
       ;
       i+=sizeof(struct sw_on_key_node), t0=(t0+i) & sw->mask) {
    hnode = (struct sw_on_key_node *)&sw->tab.aschar[t0];
    if (hnode->key==key || !hnode->key)
      return hnode;
  }
}


struct sw_on_key *new_switch_on_key(size,otherwise)
     int size;
     struct try_node *otherwise;
{
  REGISTER int i;
  struct sw_on_key *sw;

  sw = (struct sw_on_key *)
       checkalloc(sizeof(struct sw_on_key)+
                  (size-ANY)*sizeof(struct sw_on_key_node));

  sw->mask = SizeToMask(size);
  sw->count = 0;
#if defined(ATOMGC)
  sw->next_index = 0;
#endif
  for (i=0; i<size; i++)
    sw->tab.asnode[i].key = 0,
    sw->tab.asnode[i].value.try_chain = otherwise;
  return sw;
}


/* I still could not make the "pointer to the last try_node" work with the
has table try nodes; I am passing a NULL pointer which is checked by
incore_insert() and not used.  */

static void incore_puthash(psw,effar,current,def,k)
     struct sw_on_key **psw;
     int effar;
     struct emul_info *current;
     struct incore_info *def;
     TAGGED k;
{
  REGISTER int i;
  REGISTER struct sw_on_key_node *h1;
  struct try_node *otherwise = NULL;
  int size = SwitchSize(*psw);

  if (k==ERRORTAG){		/* add an alt. to default and to every key */
    for (i=0; i<size; i++) {
      h1 = &(*psw)->tab.asnode[i];
      if (h1->key)
        incore_insert(&h1->value.try_chain,effar,current,def);
      else if (!otherwise){
        incore_insert(&h1->value.try_chain,effar,current,def);
        otherwise = h1->value.try_chain;
      } else
        h1->value.try_chain = otherwise;
    }
  } else {
    h1 = incore_gethash(*psw,k);
    if (!h1->key) {
      h1->key = k;
      h1->value.try_chain = incore_copy(otherwise=h1->value.try_chain);
      incore_insert(&h1->value.try_chain,effar,current,def);
      if (((*psw)->count+=1)<<1 > size)
        expand_sw_on_key(psw,otherwise,TRUE);
    } else incore_insert(&h1->value.try_chain,effar,current,def);
  }
}

static void free_try(t)
     struct try_node **t;
{
  struct try_node *t1, *t2;

  for (t1=(*t); !ISNOTRY(t1); t1=t2) {
    t2=t1->next;
    checkdealloc((TAGGED *)t1,sizeof(struct try_node));
    }
  (*t)=NULL;
}


static void free_sw_on_key(sw)
     struct sw_on_key **sw;
{
  REGISTER struct sw_on_key_node *h1;
  REGISTER int i;
  int size = SwitchSize(*sw);
  BOOL otherwise = FALSE;

  for (i=0; i<size; i++) {
    h1 = &(*sw)->tab.asnode[i];
    if (h1->key || !otherwise)
      free_try(&h1->value.try_chain);
    if (!h1->key)
      otherwise = TRUE;
  }

  checkdealloc((TAGGED *)*sw,sizeof(struct sw_on_key)+
		   (size-ANY)*sizeof(struct sw_on_key_node));

  (*sw)=NULL;
}

static void free_emulinfo(cl)
     REGISTER struct emul_info *cl;
{
  REGISTER struct definition *def, *sibling;

  for (def=cl->subdefs; def!=NULL; def=sibling) {
    sibling = DEF_SIBLING(def);
    free_info(def->predtyp, (char *)def->code.intinfo);
    checkdealloc((TAGGED *)def,sizeof(struct definition));
  }
  checkdealloc((TAGGED *)cl,cl->objsize);
}

static void free_incoreinfo(p)
     REGISTER struct incore_info **p;
{
  REGISTER struct emul_info *stop = *(*p)->clauses_tail;
  REGISTER struct emul_info *cl, *cl1;

  for (cl=(*p)->clauses; cl!=stop; cl=cl1) {
    cl1 = cl->next;
    free_emulinfo(cl);
  }
  checkdealloc((TAGGED *)(*p),sizeof(struct incore_info));
  (*p) = NULL;
}



/* Those have to do with garbage collecting of the abolished predicates.
   Should be made by only one worker?  Otherwise, access should be locked
   when doing this GC --- which means every predicate access should be
   locked! */

static int gcdef_count=0;             /* Shared, no locked */
static int gcdef_limit=0;             /* Shared, no locked */
static struct gcdef {                 /* Shared, no locked */
  short enter_instr;
  char *info;
} *gcdef_bin;

void leave_to_gc(type, info)
     int type;
     char *info;
{
  int size;

  if (gcdef_limit==0) {
    size = 2*sizeof(struct gcdef);
    gcdef_limit = 2;
    gcdef_bin = (struct gcdef *)checkalloc(size);
  } else if (gcdef_count==gcdef_limit) {
    size = gcdef_count*(sizeof(struct gcdef));
    gcdef_limit *= 2;
    gcdef_bin = (struct gcdef *)checkrealloc((TAGGED *)gcdef_bin,size,size*2);
  }

  gcdef_bin[gcdef_count].enter_instr = type;
  gcdef_bin[gcdef_count].info = info;
  gcdef_count++;
}

static void make_undefined(Arg,f)
     Argdecl;
     struct definition *f;
{
  /*Wait_Acquire_slock(prolog_predicates_l);*/
  leave_to_gc(f->predtyp, (char *)f->code.intinfo);
  if (f->predtyp==ENTER_INTERPRETED) {
    /* erase as much as possible */
    REGISTER struct instance *i, *j;

    for (i = f->code.intinfo->first; i; i=j) {
      j = i->forward;
      if (i->death==0xffff) {
        i->death = use_clock = def_clock;
        if (i->birth==i->death) { 

/* make_undefined() is called from abolish() and define_predicate(),
   which in turn are called directly from Prolog and do not put any
   lock.  When reloading Prolog code, the existent clauses (including
   those of concurrent predicates) are erased, so we better put a lock
   on those predicates.   MCL.
*/

          Cond_Begin(f->code.intinfo->clause_insertion_cond);
          expunge_instance(i);
          Broadcast_Cond(f->code.intinfo->clause_insertion_cond);
        }
      }
    }

    Cond_Begin(f->code.intinfo->clause_insertion_cond);
    (void)ACTIVE_INSTANCE(Arg,f->code.intinfo->first,use_clock,TRUE);
    Broadcast_Cond(f->code.intinfo->clause_insertion_cond);
  }

  /*f->properties.public = 0;*/
  f->properties.wait = 0;
  f->properties.multifile = 0;
  f->properties.dynamic = 0;
#if defined(THREADS)
  f->properties.concurrent = 0;
#endif
  SetEnterInstr(f,ENTER_UNDEFINED);

  /*Release_slock(prolog_predicates_l);*/
}

/* Really get rid of abolished predicate. */
BOOL empty_gcdef_bin(Arg)
     Argdecl;
{
  REGISTER struct gcdef *g;
  ENG_INT current_mem = total_mem_count;

  while (gcdef_count>0)  {
    g = &gcdef_bin[--gcdef_count];
    free_info(g->enter_instr, g->info);
  }
  INC_MEM_PROG((total_mem_count - current_mem));

  return TRUE;
}



void relocate_gcdef_clocks(clocks)
     CLOCK *clocks;
{
  REGISTER int i;

  for (i=0; i<gcdef_count; i++)
    if (gcdef_bin[i].enter_instr==ENTER_INTERPRETED)
      relocate_clocks(((struct int_info *)gcdef_bin[i].info)->first, clocks);
}



static void free_info(insn, info)
     int insn;
     char *info;
{
  switch(insn)
    {
    case ENTER_COMPACTCODE_INDEXED:
    case ENTER_PROFILEDCODE_INDEXED:
      free_try(&((struct incore_info *)info)->lstcase);
      free_sw_on_key(&((struct incore_info *)info)->othercase);
    case ENTER_COMPACTCODE:
    case ENTER_PROFILEDCODE:
      free_try(&((struct incore_info *)info)->varcase);
      free_incoreinfo((struct incore_info **)(&info));
      break;
    case ENTER_INTERPRETED:
      {
	REGISTER struct instance *n, *m;
	int size = SwitchSize(((struct int_info *)info)->indexer);

 	for (n = ((struct int_info *)info)->first; n; n=m) {
          m=n->forward;
          n->rank = ERRORTAG;
          checkdealloc((TAGGED *)n,n->objsize);
        }
        
	checkdealloc((TAGGED *)((struct int_info *)info)->indexer,
		     sizeof(struct sw_on_key)+
		     (size-ANY)*sizeof(struct sw_on_key_node));
        
	checkdealloc((TAGGED *)info,sizeof(struct int_info));
	break;
      }
    case TABLE:
      {
	int size = SwitchSize(((struct sw_on_key *)info));
	checkdealloc((TAGGED *)info, sizeof(struct sw_on_key)+
		     (size-ANY)*sizeof(struct sw_on_key_node));
	break;
      }
    case EMUL_INFO:
      {
	free_emulinfo((struct emul_info *)info);
	break;
      }
    case OTHER_STUFF:
      {
	checkdealloc((TAGGED *)((struct other_stuff *)info)->pointer,
		     ((struct other_stuff *)info)->size);
	checkdealloc((TAGGED *)info, sizeof(struct other_stuff));
	break;
      }
    default:
      break;
    }
}

/* JFMC: abolish/1 predicate calls abolish C function. */
BOOL prolog_abolish(Arg)
     Argdecl;
{
  TAGGED *junk;
  struct definition *f;

  DEREF(X(0),X(0));
  f = find_definition(predicates_location,X(0),&junk,FALSE);
  return abolish(Arg, f);  
}

/* JFMC: abolish is now a C function instead of a predicate. */
/* Make a predicate undefined.  Also, forget spypoints etc. */
BOOL abolish(Arg, f)
     Argdecl;
     REGISTER struct definition *f;
{
  ENG_INT current_mem = total_mem_count;
  /* MCL: abolish/1 must succeed even in the case of undefined predicates */
  if (!f) return TRUE; 
  if (/*f->predtyp == ENTER_C || */                               /* JFMC */
      f->predtyp > ENTER_INTERPRETED) return FALSE;
  if (f->predtyp != ENTER_UNDEFINED) {
    f->properties.spy = 0;
    f->properties.breakp = 0;
    make_undefined(Arg, f);
    num_of_predicates--;
  }
  INC_MEM_PROG((total_mem_count - current_mem));
  return TRUE;
}

/* Define an interpreted predicate.  It is open iff it is concurrent. */

static void init_interpreted(f)
     REGISTER struct definition *f;
{

  f->code.intinfo = (struct int_info *)checkalloc(sizeof(struct int_info));

  /* By default, make it DYNAMIC.  
     set_property() may change this behavior later. MCL. */

  f->code.intinfo->behavior_on_failure = DYNAMIC;

  /*f->code.intinfo->clause_insertion_cond = create_dynamic_lock();*/
  Init_Cond(f->code.intinfo->clause_insertion_cond);

  /*  MCL added on 26 Nov 98 */
  f->code.intinfo->x2_pending_on_instance = NULL;
  f->code.intinfo->x5_pending_on_instance = NULL;

  f->code.intinfo->first = NULL;
  f->code.intinfo->varcase = NULL;
  f->code.intinfo->lstcase = NULL;
  f->code.intinfo->indexer = new_switch_on_key(2,NULL);
  SetEnterInstr(f,ENTER_INTERPRETED);
}


BOOL define_predicate(Arg)
     Argdecl;
{
  struct definition *f;
  int type;
  ENG_INT current_mem = total_mem_count;

  DEREF(X(0),X(0));
  if ((f=parse_definition(X(0)))==NULL)
    USAGE_FAULT("$define_predicate: bad 1st arg");

  /*if (f->properties.public) return FALSE;*/

  if (f->properties.multifile){   /* DCG */
    INC_MEM_PROG((total_mem_count - current_mem));
    return TRUE;
  }

  if (f->predtyp!=ENTER_UNDEFINED)
    make_undefined(Arg,f);
  
  num_of_predicates++;      /* Decremented by make_undefined(), if called */

  DEREF(X(1),X(1));
  type = (X(1)==atom_unprofiled ?  ENTER_COMPACTCODE :
    	  X(1)==atom_profiled   ? ENTER_PROFILEDCODE :
          ENTER_INTERPRETED);

  switch (type) {
  case ENTER_INTERPRETED:
    init_interpreted(f);
    break;
  default:
    {
      struct incore_info *d;
      
      d = (struct incore_info *)checkalloc(sizeof(struct incore_info));
      
      d->clauses = NULL;
      d->clauses_tail = &d->clauses;
      d->varcase = fail_alt;
      d->lstcase = NULL;        /* Used by native preds to hold nc_info */
      d->othercase = NULL; /* Used by native preds to hold index_clause */
#if defined(CACHE_INCREMENTAL_CLAUSE_INSERTION)
      d->last_inserted_clause = NULL;
      d->last_inserted_num = ~0;
#endif
      f->code.incoreinfo = d;
    }
    f->properties.nonvar = 0;
    f->properties.var = 0;
    SetEnterInstr(f,type);
    break;
  }
  INC_MEM_PROG((total_mem_count - current_mem));
  return TRUE;
}




BOOL erase_clause(Arg)
     Argdecl;
{
  ENG_INT current_mem = total_mem_count;

  DEREF(X(0),X(0));
  free_emulinfo(TagToEmul(X(0)));
  INC_MEM_PROG((total_mem_count - current_mem));

  return TRUE;
}

BOOL clause_number(Arg)
     Argdecl;
{
  struct definition *f;

  DEREF(X(0),X(0));
  if ((f=parse_definition(X(0)))==NULL)
    USAGE_FAULT("$clause_number: bad 1st arg");
  Unify_constant(MakeSmall(*(int *)f->code.incoreinfo->clauses_tail),X(1));
  return TRUE;
}



BOOL compiled_clause(Arg)
     Argdecl;
{
  struct definition *f;
  struct emul_info *ref;
  unsigned int type;
  TAGGED t1, key;
  struct incore_info *d;
  unsigned int bitmap;
  REGISTER struct emul_info *ep, **epp;
  ENG_INT current_mem = total_mem_count;

  DEREF(X(0),X(0));		/* Predicate spec */
  if ((f=parse_definition(X(0)))==NULL)
    USAGE_FAULT("$emulated_clause: bad 1st arg");
  DEREF(X(1),X(1));		/* Bytecode object */
  ref = TagToEmul(X(1));
  DEREF(X(2),X(2));		/* Mode */
  DEREF(X(3),X(3));		/* f(Type,Key[,Base,Woff,Roff]) */
  DerefArg(t1,X(3),1);
  type = GetSmall(t1);
  DerefArg(key,X(3),2);
  if (IsVar(key))
    key = ERRORTAG;
  else if (TagIsSTR(key))
    key = TagToHeadfunctor(key);

  				/* add a new clause. */
  d = f->code.incoreinfo;

  ep = (struct emul_info *)d->clauses_tail;
  if (d->clauses != NULL && !ep->objsize) {
    for (epp = &d->clauses; *epp != ep; epp = (struct emul_info **)*epp)
      ;
    ref->subdefs = ep->subdefs;
    ref->next = ep->next;
    *epp = ref;
    checkdealloc((TAGGED *)ep, sizeof(struct emul_info));
  } else {
    ref->next = (struct emul_info *)(*(int *)d->clauses_tail + 1);
    *d->clauses_tail = ref;
  }
  d->clauses_tail = &ref->next;

  bitmap = 
    (type&0x1 &&  !f->properties.nonvar ? 0x1 : 0) | /* var   */
    (type&0x8 &&  !f->properties.var    ? 0x2 : 0) | /* lst   */
    (type&0x16 && !f->properties.var    ? 0x4 : 0) ; /* other */
  if ((type&0x21) == 0x21)
    f->properties.nonvar = 1;
  if ((type&0x3e) == 0x3e)
    f->properties.var = 1;

  if (!(f->predtyp&1) && bitmap!=0x7) {
    SetEnterInstr(f,f->predtyp+1);
    d->lstcase = incore_copy(d->varcase);
    d->othercase = new_switch_on_key(2,incore_copy(d->varcase));
  }

  if (!(f->predtyp&1))
    incore_insert(&d->varcase,f->arity,ref,d);
  else {
    if (bitmap&0x1)
      incore_insert(&d->varcase,f->arity,ref,d);
    if (bitmap&0x2)
      incore_insert(&d->lstcase,f->arity,ref,d);
    if (bitmap&0x4)
      incore_puthash(&d->othercase,f->arity,ref,d,key);
  }
  INC_MEM_PROG((total_mem_count - current_mem));
  return TRUE;
}

struct sw_on_key_node *dyn_puthash(swp,k)
     struct sw_on_key **swp;
     TAGGED k;
{
  REGISTER struct sw_on_key_node *h1;

  h1 = incore_gethash(*swp,k);
  if (h1->key)
    return h1;
  else {
    h1->key = k;
    if (((*swp)->count+=1)<<1 <= SwitchSize(*swp))
      return h1;
    else {
      expand_sw_on_key(swp,NULL,TRUE);
      return incore_gethash(*swp,k);
    }
  }
}

BOOL set_property(Arg)
     Argdecl;
{
  REGISTER struct definition *f;
  TAGGED *junk;
  short type;



  DEREF(X(0),X(0));
  if (!(f = find_definition(predicates_location,X(0),&junk,FALSE)))
    return FALSE;
  type = f->predtyp;
  if ((type > ENTER_FASTCODE_INDEXED && type != ENTER_INTERPRETED) ||
      (type <= ENTER_FASTCODE_INDEXED && f->code.incoreinfo->clauses) ||
      (type == ENTER_INTERPRETED && f->code.intinfo->first))
    return FALSE;

  DEREF(X(1),X(1));
  /*
    if (X(1)==atom_public)
    f->properties.public = 1;
    else
    f->properties.public = 0;
  */
  if (X(1)==atom_wait) {
    f->properties.wait = 1;
    SetEnterInstr(f,type);
  } else if ( (X(1)==atom_dynamic) || (X(1) == atom_concurrent)){  /* MCL */
    f->properties.dynamic = 1;
    f->properties.concurrent = X(1) == atom_concurrent;            /* MCL */

    if (type != ENTER_INTERPRETED) {          /* Change it to interpreted */
      free_incoreinfo(&f->code.incoreinfo);
      init_interpreted(f);
    }

    /* In any case, set the runtime behavior */
    f->code.intinfo->behavior_on_failure =
#if defined(THREADS)
      f->properties.concurrent ? CONC_OPEN : DYNAMIC;
#else
      DYNAMIC;
#endif
  }
  else if (X(1)==atom_multifile)
    f->properties.multifile = 1;

  return TRUE;
}
