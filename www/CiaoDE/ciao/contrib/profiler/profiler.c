#if !defined(PROFILE)
#define PROFILE

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "datadefs.h"
#include "initial.h"
#include "predtyp.h"
#include "profile_defs.h"
#include "timing_defs.h"
#include "alloc_defs.h"
#include "support.h"
#include "support_defs.h"
#include "debug.h"
#include "builtin_defs.h"
#include "profiler.h"

extern BOOL prof_include_time;

static int compare_calls(const void *arg1, const void *arg2);
static int compare_clicks(const void *arg1, const void *arg2);
static int compare_cci_calls(const void *arg1, const void *arg2);
static int compare_cci_clicks(const void *arg1, const void *arg2);
static int compare_ccc_calls(const void *arg1, const void *arg2);
static int compare_ccc_clicks(const void *arg1, const void *arg2);

struct cost_center_clip {
  struct cost_center *cc;
  long int realsize;
  long int realsize_oh;
  struct profile_currents total;
  struct profile_currents overhead;
};

struct ht_tab *cost_center_table = NULL;
struct cost_center *active_cc = NULL;
struct cost_center *default_cc = NULL;
struct cost_center *prev_cc = NULL;
struct cost_center_item *last_cci = NULL;

struct definition *last_called_predicate = NULL;
struct definition *profile__hook_cc_call = NULL;
struct definition *profile__hook_cc_redo = NULL;
struct definition *profile__hook_cc_redo_ = NULL;
struct definition *profile__hook_cc_exit = NULL;
struct definition *profile__hook_cc_fail = NULL;
struct definition *profile__hook_cc_fail_ = NULL;
int node_stack_point = 0;
int node_stack_size = CALL_STACK_INITIAL_SIZE;
struct node_stack_item *node_stack = NULL;
ENG_LINT click_last_addition = 0;
ENG_LINT click_ini_profiling = 0;
ENG_LINT click_start = 0;
# if defined(PROFILE__PROFTIME)
ENG_LINT click_profiling = 0;  /* time (in clicks) doing profiling (overhead) */
ENG_LINT click_end_profiling = 0;
# endif

/* #if defined(PROFILE__TRACER) */
int profile_trace = 0;
/* #endif */

# if !defined(PROFILE__USE_TIMESTAMP)
ENG_LINT (**profclick)(void) = &userclick;
ENG_LINT *profclockfreq = &stats.userclockfreq;
# endif

/* run time hooks */

void profile__hook_fail_(struct worker *w) {
  if(profile) {
    PROFILE__TIME_INI;
    if(node_stack_point >= 0) {
      struct cost_center_item *cce=NULL;   /* EMM */
      unsigned int choice, prev_choice, curr_choice;
      /* now redo from the node_stack up to the first actual choice point */
      ComputeA(w->local_top,w->node);
      /* choice = (TAGGED *)w->node->local_top - w->stack_start; */
      choice = ChoiceToInt(w->node);
      prev_choice = node_stack[node_stack_point].choice;
      node_stack_point--;
      while(((int)(node_stack[node_stack_point].choice)>=choice)&&(node_stack_point>=0))
        {
          curr_choice = node_stack[node_stack_point].choice;
          cce = node_stack[node_stack_point].first_cci;
          printf("%d\n", profile_trace);
          ShowFuncPoint(" unkn", node_stack_point, choice,
            node_stack[node_stack_point].last_cci->functor);
          if(prev_choice >= curr_choice) {
            ShowFuncPoint("+skip", node_stack_point, choice,
              node_stack[node_stack_point].last_cci->functor);
            cce->functor->prof.skips++;
            cce->prof.skips++;
          } else {
            ShowFuncPoint("+cuts", node_stack_point, choice,
              node_stack[node_stack_point].last_cci->functor);
          }
          prev_choice = curr_choice;
          node_stack[node_stack_point].first_cci=NULL;
          node_stack[node_stack_point].last_cci=NULL;
          node_stack_point--;
        }
    }
    PROFILE__TIME_END;
  };
}

void profile__hook_retry_(Arg, count_retry)
     int count_retry;
     Argdecl;
{
  if(profile) {
    PROFILE__TIME_INI;
    if(node_stack_point>=0) {
      struct cost_center_item *cci=NULL;
      cci = node_stack[node_stack_point].last_cci;
      if(cci!=NULL) {
        ShowFuncPoint("retry", node_stack_point,
          node_stack[node_stack_point].choice, cci->functor);
        if(count_retry) { /* w->node->next_alt */
          ShowClauseNumber;
          cci->functor->prof.retrys++;
          cci->prof.retrys++;
        }
        else
          ShowNoMoreAlts;
      }
    }
    PROFILE__TIME_END;
  }
}

void profile__hook_cut_(struct worker *w) {
  if(profile) {
    PROFILE__TIME_INI;
    if(node_stack_point >= 0) {
      struct cost_center_item *first_cci=NULL, *last_cci;   /* EMM */
      unsigned int choice, prev_choice, curr_choice, skips=0;
      choice = ChoiceToInt(w->node);
      last_cci = node_stack[node_stack_point].last_cci;
      prev_choice = node_stack[node_stack_point].choice;
      while(((int)(node_stack[node_stack_point].choice)>=choice)&&(node_stack_point>=0))
        {
          curr_choice = node_stack[node_stack_point].choice;
          first_cci = node_stack[node_stack_point].first_cci;
          if(prev_choice > curr_choice) {
            ShowFuncPoint("+skip", node_stack_point, curr_choice,
                          node_stack[node_stack_point].last_cci->functor);
            first_cci->functor->prof.skips++;
            first_cci->prof.skips++;
            skips++;
          } else if (prev_choice < curr_choice) {
            ShowFuncPoint("+warning: undetected cutn", node_stack_point,
                          curr_choice, node_stack[node_stack_point].last_cci->functor);
          } else {
            ShowFuncPoint("+pass", node_stack_point, curr_choice,
			  node_stack[node_stack_point].last_cci->functor);
	  }
          prev_choice = curr_choice;
          node_stack[node_stack_point].first_cci=NULL;
          node_stack[node_stack_point].last_cci=NULL;
          node_stack_point--;
        }
      if(node_stack_point >= 0) {
        last_cci = node_stack[node_stack_point].last_cci;
        ShowFuncPoint("+cut level", node_stack_point, choice, last_cci->functor);
        ShowSkipNodes(skips);
        last_cci->functor->prof.nskips+=skips;
        last_cci->prof.nskips+=skips;
        if(skips) 
          last_cci->functor->prof.scuts++;
        last_cci->prof.scuts++;
      } else {
        last_cci->functor->prof.cuts++;
        last_cci->prof.cuts++;
      }
    }
    PROFILE__TIME_END;
  }
}

void profile__hook_proceed_(void)
{
  if(profile) {
    struct cost_center_item *cce=node_stack[node_stack_point].last_cci;
    if(cce)
      ShowFuncPoint("+proc", node_stack_point,
        node_stack[node_stack_point].choice, cce->functor);
  }
}

void profile__hook_neck_proceed_(void)
{
  if(profile) {
    struct cost_center_item *cce=node_stack[node_stack_point].last_cci;
    if(cce)
      ShowFuncPoint("+nprc", node_stack_point,
        node_stack[node_stack_point].choice, cce->functor);
  }
}

void profile__hook_call_(w,functor)
     struct worker *w;
     struct definition *functor;
{
  if(profile) {
    PROFILE__TIME_INI;
    {
      struct node_stack_item *csn;
      unsigned int choice;
      ENG_LINT profdiff;
      if(functor==profile__hook_cc_call) {
        prev_cc = active_cc;
        active_cc = get_cost_center(cost_center_table, last_called_predicate);
      }
      {
        ComputeA(w->local_top,w->node);
        choice=ChoiceToInt(w->node);
        ShowFuncPoint("+call", node_stack_point, choice, functor);
        if (prof_include_time) {
          profdiff = click_ini_profiling - click_last_addition;
          if(last_called_predicate!=NULL) {
            last_called_predicate->prof.time_spent += profdiff;
            last_cci->prof.time_spent += profdiff;
          }
          else {
            click_start += profdiff;
          }
        }
        if(last_called_predicate!=NULL) {
          last_called_predicate->prof.calls++;
            last_cci->prof.calls++;	  
	}
        if((node_stack_point<0)||node_stack[node_stack_point].choice!=choice) {
          node_stack_point++;
          if(node_stack_point>=node_stack_size) {
            struct node_stack_item *new_node_stack;
            int new_node_stack_size = 2 * node_stack_size;
            printf("{WARNING: Increasing node stack to %d.}\n",
              new_node_stack_size);
            new_node_stack = (struct node_stack_item *)checkalloc(new_node_stack_size
              * sizeof(struct node_stack_item));
            memcpy(new_node_stack, node_stack, node_stack_size
              * sizeof(struct node_stack_item));
            checkdealloc((TAGGED *)node_stack, node_stack_size
              * sizeof(struct node_stack_item));
            node_stack = new_node_stack;
            node_stack_size = new_node_stack_size;
          }
          csn=&node_stack[node_stack_point];
          csn->first_cci = get_cc_item(active_cc->cc_item_table,functor);
          csn->choice=choice;
          if(functor!=profile__hook_cc_redo_) {
            csn->last_cci = csn->first_cci;
            last_cci = csn->last_cci;
          }
        }
        else {
          csn=&node_stack[node_stack_point];
          if(functor!=profile__hook_cc_redo_) {
            csn->last_cci = get_cc_item(active_cc->cc_item_table,functor);
            last_cci = csn->last_cci;
          }
        }
        last_called_predicate = functor;
      }
      click_last_addition = click_ini_profiling;
    }
    PROFILE__TIME_END;
  }
}

void profile__init(void)
{
  if(cost_center_table) {
    ht_destroy(cost_center_table);
  }
  cost_center_table = ht_create(8);
  default_cc = add_cost_center(cost_center_table, NULL);
  active_cc = default_cc;
  node_stack_point = -1; /* empty stack */
  if(node_stack) {
    checkdealloc((TAGGED *)node_stack,node_stack_size * sizeof(struct node_stack_item));
  }
  node_stack_size = CALL_STACK_INITIAL_SIZE;
  node_stack = (struct node_stack_item *)checkalloc(node_stack_size
    * sizeof(struct node_stack_item));
  node_stack[0].first_cci=NULL;
  profile__hook_fail = profile__hook_fail_;
  profile__hook_retry = profile__hook_retry_;
  profile__hook_cut = profile__hook_cut_;
  profile__hook_proceed = profile__hook_proceed_;
  profile__hook_neck_proceed = profile__hook_neck_proceed_;
  profile__hook_call = profile__hook_call_;
}

struct cost_center *add_cost_center(cct, functor)
     struct ht_tab * cct;
     struct definition *functor;
{
  struct cost_center *r;
  r = (struct cost_center *)checkalloc(sizeof(struct cost_center));
  r->functor=functor;
  r->calls=r->redos=r->fails=r->exits=0;
  r->time_spent=0;
  r->cc_item_table=ht_create(8);
  ht_add(cct,(ub1 *)(&(r->functor)),sizeof(functor),r);
  return r;
}

struct cost_center *get_cost_center(cct, functor)
     struct ht_tab *cct;
     struct definition *functor;
{
  if(ht_find(cct,(ub1 *)(&functor),sizeof(functor)))
    return (struct cost_center *)ht_stuff(cct);
  else
    return add_cost_center(cct, functor);
}

struct cost_center_item *add_cc_item(ht,functor)
     struct ht_tab *ht;
     struct definition *functor;
{
  struct cost_center_item *e;
  e = (struct cost_center_item *)checkalloc(sizeof(struct cost_center_item));
  e->functor = functor;
  PROFILE__RESET_PROF(e->prof);
  ht_add(ht,(ub1 *)(&(e->functor)),sizeof(functor),(void *)e);
  return e;
}

struct cost_center_item *get_cc_item(ht,functor)
     struct ht_tab *ht;
     struct definition *functor;
{
  if(ht_find(ht,(ub1 *)(&functor),sizeof(functor)))
    return (struct cost_center_item *)ht_stuff(ht);
  else
    return add_cc_item(ht,functor);  
}

void empty_hashtable(tab,size)
     struct ht_tab * tab;
{
  while(ht_first(tab)) {
    checkdealloc((TAGGED *)ht_stuff(tab),size);
    ht_del(tab);
  }
}

void empty_cost_center_table(cct)
     struct ht_tab * cct;
{
  struct cost_center *r;
  while(ht_first(cct)) {
    r = (struct cost_center *)ht_stuff(cct);
    empty_hashtable(r->cc_item_table, sizeof(struct cost_center_item));
    ht_destroy(r->cc_item_table);
    checkdealloc((TAGGED *)r,sizeof(struct cost_center));
    ht_del(cct);
  }
}

void reset_cc_item_table(cc_item_table)
     struct ht_tab * cc_item_table;
{
  struct cost_center_item *r;
  if(ht_first(cc_item_table)) do {
    r = (struct cost_center_item *)ht_stuff(cc_item_table);
    PROFILE__RESET_PROF(r->prof);
  }
  while(ht_next(cc_item_table));
}

void reset_cost_center_table(cct)
     struct ht_tab *cct;
{
  struct cost_center *r;
   if(ht_first(cct)) do {
    r = (struct cost_center *)ht_stuff(cct);
    r->calls=r->redos=r->fails=r->exits=0;
    r->time_spent=0;
    reset_cc_item_table(r->cc_item_table);
  }
  while (ht_next(cct));
}

void show_func_point(label, call_point, choice, functor)
  char *label;
  int call_point;
  unsigned int choice;
  struct definition *functor;
{
  printf("%s %d-%d ", label, call_point, choice);
  if(functor) {
    printf("%-7s ", predicate_type(functor->predtyp));
    if(IsString(functor->printname))
      printf("%s/", GetString(functor->printname));
    else
      printf("*** Unknown term ***/");
    printf("%d\n", functor->arity);
  }
  else
    printf("*** Null functor ***\n");
}

BOOL functor_have_overhead(cct, functor)
     struct ht_tab * cct;
     struct definition *functor;
{
  return
    functor==profile__hook_cc_call ||
    functor==profile__hook_cc_redo ||
    functor==profile__hook_cc_redo_ ||
    functor==profile__hook_cc_exit ||
    functor==profile__hook_cc_fail ||
    functor==profile__hook_cc_fail_ ||
    ht_exists(cct, (ub1 *)(&functor),sizeof(functor));
}

void profile_ccc_dump(ccc)
     struct cost_center_clip *ccc;
{
  struct ht_tab *t;
  struct cost_center_item **cci_table, **cci_table_oh;
  struct cost_center_item *e;
  int (*compare_cci)(const void *, const void *);
  long int i, j;
  if(ccc->cc->functor) {
    printf("Cost Center     %s/%d\n", GetString(ccc->cc->functor->printname),
      ccc->cc->functor->arity);
  }
  else
    printf("Beyond Cost Centers \n");
  printf("        ============================================================================================\n");
  printf("        Calls=%-7ld Redos=%-7ld Exits=%-7ld Fails=%-7ld\n", ccc->cc->calls,
         ccc->cc->redos,ccc->cc->exits,ccc->cc->fails);
  printf("        ============================================================================================\n");
  printf("        Calls           Skips   NSkips  Cuts    SCuts   Retrys  Time(ms) -rough         Type    Spec\n");
  printf("        =============== ======= ======= ======= ======= ======= ======================= ====    ====\n");
  t=ccc->cc->cc_item_table;
  /* first calculate the totals */
  /* Make a table with room for them */
  cci_table = (struct cost_center_item **)checkalloc(ccc->realsize
    * sizeof(struct cost_center_item *));
  cci_table_oh = (struct cost_center_item **)checkalloc((ccc->realsize_oh)
    * sizeof(struct cost_center_item *));
  i = 0;
  j = 0;
  if(ht_first(t)) do {
    e = (struct cost_center_item *)ht_stuff(t);
    if(e->prof.calls) {
      if(functor_have_overhead(cost_center_table, e->functor))
        cci_table_oh[j++] = e;
      else
        cci_table[i++] = e;
    }
  } while(ht_next(t));
  if(prof_include_time)
    compare_cci = compare_cci_clicks;
  else
    compare_cci = compare_cci_calls;
  qsort(cci_table, ccc->realsize, sizeof(struct cost_center_item *), compare_cci);
  qsort(cci_table_oh, ccc->realsize_oh, sizeof(struct cost_center_item *), compare_cci);
  i = ccc->realsize;
  while(i>0){
    e = cci_table[--i];
    printf("        %7ld %6.2f%% %7ld %7ld %7ld %7ld %7ld %12f (%6.2f%%)  %s  %s/%d\n",
           e->prof.calls,
           (((double)e->prof.calls)*100.0)/(double)ccc->total.calls,
           e->prof.skips,
           e->prof.nskips,
           e->prof.cuts,
           e->prof.scuts,
           e->prof.retrys,
           (((double)e->prof.time_spent)/PROFILE__GET_FREQ)*1.0e3,
           (((double)e->prof.time_spent)*100.0)/(double)ccc->total.time_spent,
           predicate_type(e->functor->predtyp),
           GetString(e->functor->printname),
           e->functor->arity);
  }
  printf("        =============== ======= ======= ======= ======= ======= ======================= ====    ====\n");
  printf("        %7ld %6.2f%% %7ld %7ld %7ld %7ld %7ld %12f (%6.2f%%)          Total\n",
         ccc->total.calls, 100.0, ccc->total.skips, ccc->total.nskips,
         ccc->total.cuts, ccc->total.scuts, ccc->total.retrys,
         (double)ccc->total.time_spent/PROFILE__GET_FREQ*1.0e3, 100.0);
  printf("        %ld predicates called\n\n", ccc->realsize);
  j = ccc->realsize_oh;
  if(j)
  {
    printf("Overhead:\n");
    printf("        Calls           Skips   NSkips  Cuts    SCuts   Retrys  Time(ms) -rough         Type    Spec\n");
    printf("        =============== ======= ======= ======= ======= ======= ======================= ====    ====\n");
    while(j>0){
      e = cci_table_oh[--j];
      printf("        %7ld %6.2f%% %7ld %7ld %7ld %7ld %7ld %12f (%6.2f%%)  %s  %s/%d\n",
             e->prof.calls,
             (((double)e->prof.calls)*100.0)/(double)ccc->overhead.calls,
             e->prof.skips,
             e->prof.nskips,
             e->prof.cuts,
             e->prof.scuts,
             e->prof.retrys,
             (((double)e->prof.time_spent)/PROFILE__GET_FREQ)*1.0e3,
             (((double)e->prof.time_spent)*100.0)/(double)ccc->overhead.time_spent,
             predicate_type(e->functor->predtyp),
             GetString(e->functor->printname),
             e->functor->arity);
    }
    printf("        =============== ======= ======= ======= ======= ======= ======================= ====    ====\n");
    printf("        %7ld %6.2f%% %7ld %7ld %7ld %7ld %7ld %12f (%6.2f%%)          Total\n",
           ccc->overhead.calls, 100.0, ccc->overhead.skips, ccc->overhead.nskips,
           ccc->overhead.cuts, ccc->overhead.scuts, ccc->overhead.retrys,
           (double)ccc->overhead.time_spent/PROFILE__GET_FREQ*1.0e3, 100.0);
    printf("        %ld instrumentation predicates called\n", ccc->realsize_oh);
    printf("        %.2f%% executing instrumentation code\n\n",
           ((double)ccc->overhead.time_spent)/(ccc->total.time_spent
           + ccc->overhead.time_spent)*100.0);
  }
  else
    printf("{NOTE: Don't have overhead caused by instrumentation predicates}\n");
  checkdealloc((TAGGED *)cci_table_oh, ccc->realsize_oh*sizeof(struct cost_center_item *));
  checkdealloc((TAGGED *)cci_table, ccc->realsize*sizeof(struct cost_center_item *));
}

static int compare_cci_calls(arg1, arg2)
     const void *arg1, *arg2;
{
  struct cost_center_item **cce1, **cce2;
  
  cce1 = (struct cost_center_item **)arg1;
  cce2 = (struct cost_center_item **)arg2;

  if ((*cce1)->prof.calls > (*cce2)->prof.calls)
    return (1);
  if ((*cce1)->prof.calls < (*cce2)->prof.calls)
    return (-1);
  return (0);
}

static int compare_cci_clicks(arg1, arg2)
     const void *arg1, *arg2;
{
  struct cost_center_item **cce1, **cce2;
  
  cce1 = (struct cost_center_item **)arg1;
  cce2 = (struct cost_center_item **)arg2;

  if ((*cce1)->prof.time_spent > (*cce2)->prof.time_spent)
    return (1);
  if ((*cce1)->prof.time_spent < (*cce2)->prof.time_spent)
    return (-1);
  return (0);
}

void profile_detail_dump(void)
{
  struct cost_center_item *e;
  struct cost_center_clip *ccc_table, *c;
  struct cost_center *cc;
  struct ht_tab *t;
  int (*compare_ccc)(const void *, const void *);
  long int cc_calls=0,
    cc_redos=0,
    cc_exits=0,
    cc_fails=0;
  long int i, realsize=0;
  struct profile_currents total;

  PROFILE__RESET_PROF(total);
  ccc_table = (struct cost_center_clip *)checkalloc(ht_count(cost_center_table)
    * sizeof(struct cost_center_clip));
  if(ht_first(cost_center_table)) do {
    cc = (struct cost_center *)ht_stuff(cost_center_table);
    t=cc->cc_item_table;
    /* first calculate the totals */
    ccc_table[realsize].realsize=0;
    ccc_table[realsize].total.skips=0;
    ccc_table[realsize].total.nskips=0;
    ccc_table[realsize].total.cuts=0;
    ccc_table[realsize].total.scuts=0;
    ccc_table[realsize].total.retrys=0;
    ccc_table[realsize].total.calls=0;
    ccc_table[realsize].total.time_spent=0;

    ccc_table[realsize].realsize_oh=0;
    ccc_table[realsize].overhead.skips=0;
    ccc_table[realsize].overhead.nskips=0;
    ccc_table[realsize].overhead.cuts=0;
    ccc_table[realsize].overhead.scuts=0;
    ccc_table[realsize].overhead.retrys=0;
    ccc_table[realsize].overhead.calls=0;
    ccc_table[realsize].overhead.time_spent=0;

    if(ht_first(t)) do {
      e = (struct cost_center_item *)ht_stuff(t);
      if(e->prof.calls) {
        if(functor_have_overhead(cost_center_table, e->functor)) {
          ccc_table[realsize].realsize_oh++;
          ccc_table[realsize].overhead.calls += e->prof.calls;
          ccc_table[realsize].overhead.skips += e->prof.skips;
          ccc_table[realsize].overhead.nskips += e->prof.nskips;
          ccc_table[realsize].overhead.cuts += e->prof.cuts;
          ccc_table[realsize].overhead.scuts += e->prof.scuts;
          ccc_table[realsize].overhead.retrys += e->prof.retrys;
          ccc_table[realsize].overhead.time_spent += e->prof.time_spent;
        }
        else
        {
          ccc_table[realsize].realsize++;
          ccc_table[realsize].total.calls += e->prof.calls;
          ccc_table[realsize].total.skips += e->prof.skips;
          ccc_table[realsize].total.nskips += e->prof.nskips;
          ccc_table[realsize].total.cuts += e->prof.cuts;
          ccc_table[realsize].total.scuts += e->prof.scuts;
          ccc_table[realsize].total.retrys += e->prof.retrys;
          ccc_table[realsize].total.time_spent += e->prof.time_spent;
        }
      }
    } while(ht_next(t));
    if(ccc_table[realsize].total.calls+ccc_table[realsize].overhead.calls != 0) {
      ccc_table[realsize].cc = cc;
      realsize++;
    }
  } while(ht_next(cost_center_table));

  if(realsize<=1) {
    printf("{NOTE: No cost centers has been specified}\n");
  }
  else {
    if(prof_include_time)
      compare_ccc = compare_ccc_clicks;
    else
      compare_ccc = compare_ccc_calls;
    qsort(ccc_table, realsize, sizeof(struct cost_center_clip), compare_ccc);
    i = realsize;  
    while(i>0) {
      --i;
      profile_ccc_dump(&ccc_table[i]);
      total.calls += ccc_table[i].total.calls;
      total.skips += ccc_table[i].total.skips;
      total.nskips += ccc_table[i].total.nskips;
      total.cuts += ccc_table[i].total.cuts;
      total.scuts += ccc_table[i].total.scuts;
      total.retrys += ccc_table[i].total.retrys;
      total.time_spent += ccc_table[i].total.time_spent;
      cc_calls += ccc_table[i].cc->calls;
      cc_redos += ccc_table[i].cc->redos;
      cc_exits += ccc_table[i].cc->exits;
      cc_fails += ccc_table[i].cc->fails;
    }
    printf("Detailed profiling resume:\n");
    printf("================================================================================\n");
    printf("Calls=%-7ld Redos=%-7ld Exits=%-7ld Fails=%-7ld\n",
       cc_calls, cc_redos, cc_exits, cc_fails);
    printf("================================================================================\n");
    printf("Calls           Skips   NSkips  Cuts    SCuts   Retrys  ");
    if (prof_include_time) printf("Time(ms) -rough         ");
    printf("Type    Spec\n");
    printf("=============== ======= ======= ======= ======= ======= ");
    if (prof_include_time) printf("======================= ");
    printf("====    ====\n");
    i = realsize;
    while(i>0){
      c = &ccc_table[--i];
      printf("%7ld %6.2f%% %7ld %7ld %7ld %7ld %7ld ",
             c->total.calls,
             (((double)c->total.calls)*100.0)/(double)total.calls,
             c->total.skips,
             c->total.nskips,
             c->total.cuts,
             c->total.scuts,
             c->total.retrys);
      if (prof_include_time)
        printf("%12f (%6.2f%%)  ",
          (((double)c->total.time_spent)/PROFILE__GET_FREQ)*1.0e3,
          (((double)c->total.time_spent)*100.0)/(double)total.time_spent);
      if (c->cc->functor)
        printf("%s  %s/%d\n",
               predicate_type(c->cc->functor->predtyp),
               GetString(c->cc->functor->printname),
               c->cc->functor->arity);
      else
        printf("        Beyond Cost Centers\n");
    }
    printf("=============== ======= ======= ======= ======= ======= ");
    if (prof_include_time) printf("======================= ");
    printf("====    ====\n");
    printf("%7ld %6.2f%% %7ld %7ld %7ld %7ld %7ld ",
      total.calls, 100.0, total.skips, total.nskips, total.cuts, total.scuts, total.retrys);
    if (prof_include_time)
      printf("%12f (%6.2f%%)  ", (double)total.time_spent/PROFILE__GET_FREQ*1.0e3, 100.0);
    printf("        Total\n");
    printf("%ld cost centers called\n\n", realsize);
  }
  checkdealloc((TAGGED *)ccc_table, ht_count(cost_center_table)
    * sizeof(struct cost_center_clip));
}

static int compare_ccc_calls(arg1, arg2)
     const void *arg1, *arg2;
{
  struct cost_center_clip *ccc1, *ccc2;
  
  ccc1 = (struct cost_center_clip *)arg1;
  ccc2 = (struct cost_center_clip *)arg2;

  if ((ccc1)->total.calls > (ccc2)->total.calls)
    return (1);
  if ((ccc1)->total.calls < (ccc2)->total.calls)
    return (-1);
  return (0);
}

static int compare_ccc_clicks(arg1, arg2)
     const void *arg1, *arg2;
{
  struct cost_center_clip *ccc1, *ccc2;
  
  ccc1 = (struct cost_center_clip *)arg1;
  ccc2 = (struct cost_center_clip *)arg2;

  if ((ccc1)->total.time_spent > (ccc2)->total.time_spent)
    return (1);
  if ((ccc1)->total.time_spent < (ccc2)->total.time_spent)
    return (-1);
  return (0);
}

void profile_flat_dump(void)
{
  struct sw_on_key *table = (struct sw_on_key *)*predicates_location;
  struct sw_on_key_node *keyval;
  int j = SwitchSize(*predicates_location);
  long int i, realsize = 0, realsize_oh = 0;
  struct profile_currents total, overhead;
  struct definition **pred_table, **pred_table_oh, *d;
  int (*compare_func)(const void *, const void *);

  PROFILE__RESET_PROF(total);
  PROFILE__RESET_PROF(overhead);
  PROFILE__TIME_FLUSH;
  printf("\nPlease keep in mind the next definitions: \n");
  printf("Calls=  Number of times a predicate is called.\n");
  printf("Skips=  Number of skips over a node in case it be cutted.\n");
  printf("NSkips= Number of nodes that have been cutteds with the cut.\n");
  printf("Cuts=   Number of efective cuts that are done in the scope of a node.\n");
  printf("SCuts=  Number of cuts that don't have effects in the scope of a node.\n");
  printf("Retrys= Number of times a choice point is retried (It is not equal to redo).\n");
  printf("Total NSkips = Total SCuts.\n\n");
  printf("Flat profile information:\n\n");
  for (--j; j>=0; --j) {           /* Find how many preds. we have called */
    keyval = &table->tab.asnode[j];
    if ((d = keyval->value.def) &&
        d -> predtyp != ENTER_UNDEFINED  &&
        d -> prof.calls)
    {
      if(functor_have_overhead(cost_center_table, d)) {
        realsize_oh++;
        overhead.calls += d->prof.calls;
        overhead.skips += d->prof.skips;
        overhead.nskips += d->prof.nskips;
        overhead.cuts += d->prof.cuts;
        overhead.scuts += d->prof.scuts;
        overhead.retrys += d->prof.retrys;
        overhead.time_spent += d->prof.time_spent;
      }
      else {
        realsize++;
        total.calls += d->prof.calls;
        total.skips += d->prof.skips;
        total.nskips += d->prof.nskips;
        total.cuts += d->prof.cuts;
        total.scuts += d->prof.scuts;
        total.retrys += d->prof.retrys;
        /* if we need to verify the time conservation law: (available
           only when time profiling is on) */
        total.time_spent += d->prof.time_spent;
      }
    }
  }
  /* using the time conservation law, we can obtain the tot_clicks:
     (valid also when time profiling is off)*/
#if defined(PROFILE__PROFTIME)
  /*   tot_clicks = click_last_addition - click_start - click_profiling + 1; */
  if(click_profiling + total.time_spent + overhead.time_spent
      != click_last_addition - click_start)
    printf("Verification Error (good if equal) %lld = %lld\n",
           click_profiling + total.time_spent + overhead.time_spent,
           click_last_addition - click_start);
#else
  /*   tot_clicks = click_last_addition - click_start + 1; */
  if(total.time_spent + overhead.time_spent
      != click_last_addition - click_start)
    printf("Verification Error (good if equal) %lld = %lld\n",
           total.time_spent + overhead.time_spent,
           click_last_addition - click_start);
#endif

  pred_table =                         /* Make a table with room for them */
    (struct definition **)checkalloc(realsize*sizeof(struct definition *));
  pred_table_oh =
    (struct definition **)checkalloc(realsize_oh*sizeof(struct definition *));

  j = SwitchSize(*predicates_location);
  realsize = 0;
  realsize_oh = 0;
  for (--j; j>=0; --j) {
    keyval = &table->tab.asnode[j];
    if ((d  = keyval->value.def) &&
        d -> predtyp != ENTER_UNDEFINED &&
        d -> prof.calls) {
      if(functor_have_overhead(cost_center_table, d)) {      
        pred_table_oh[realsize_oh++] = d;
      }
      else
        pred_table[realsize++] = d;
    }
  }

  if(prof_include_time)
    compare_func = compare_clicks;
  else
    compare_func = compare_calls;
  qsort(pred_table, realsize, sizeof(struct definition *), compare_func);
  qsort(pred_table_oh, realsize_oh, sizeof(struct definition *), compare_func);
  printf("Calls           Skips   NSkips  Cuts    SCuts   Retrys  Time(ms) -rough         Type    Spec\n");
  printf("=============== ======= ======= ======= ======= ======= ======================= ====    ====\n");
  i = realsize;
  while(i>0){
    d = pred_table[--i];
    printf("%7ld %6.2f%% %7ld %7ld %7ld %7ld %7ld %12f (%6.2f%%) %s  %s/%d\n",
           d->prof.calls,
           (((double)d->prof.calls)*100.0)/(double)total.calls,
           d->prof.skips,
           d->prof.nskips,
           d->prof.cuts,
           d->prof.scuts,
           d->prof.retrys,
           (((double)d->prof.time_spent)/PROFILE__GET_FREQ)*1.0e3,
           (((double)d->prof.time_spent)*100.0)/(double)total.time_spent,
           predicate_type(d->predtyp),
           GetString(d->printname),
           d->arity);
  }
  printf("=============== ======= ======= ======= ======= ======= ======================= ====    ====\n");
  printf("%7ld %6.2f%% %7ld %7ld %7ld %7ld %7ld %12f (%6.2f%%)         Total\n",
    total.calls, 100.0, total.skips, total.nskips, total.cuts, total.scuts,
    total.retrys, (double)total.time_spent/PROFILE__GET_FREQ*1.0e3, 100.0);
  printf("%ld predicates called\n\n", realsize);

  i = realsize_oh;
  if(i) {
    printf("Overhead:\n");
    printf("Calls           Skips   NSkips  Cuts    SCuts   Retrys  Time(ms) -rough         Type    Spec\n");
    printf("=============== ======= ======= ======= ======= ======= ======================= ====    ====\n");
    while(i>0){
      d = pred_table_oh[--i];
      printf("%7ld %6.2f%% %7ld %7ld %7ld %7ld %7ld %12f (%6.2f%%) %s  %s/%d\n",
             d->prof.calls,
             (((double)d->prof.calls)*100.0)/(double)overhead.calls,
             d->prof.skips,
             d->prof.nskips,
             d->prof.cuts,
             d->prof.scuts,
             d->prof.retrys,
             (((double)d->prof.time_spent)/PROFILE__GET_FREQ)*1.0e3,
             (((double)d->prof.time_spent)*100.0)/(double)overhead.time_spent,
             predicate_type(d->predtyp),
             GetString(d->printname),
             d->arity);
    }
    printf("=============== ======= ======= ======= ======= ======= ============            ====    ====\n");
    printf("%7ld %6.2f%% %7ld %7ld %7ld %7ld %7ld %12f (%6.2f%%)         Total\n",
      overhead.calls, 100.0, overhead.skips, overhead.nskips, overhead.cuts, overhead.scuts,
      overhead.retrys, (double)overhead.time_spent/PROFILE__GET_FREQ*1.0e3, 100.0);
    printf("%ld instrumentation predicates called\n", realsize_oh);
#if defined(PROFILE__PROFTIME)
    printf("%8f ms (%6.2f%%) doing profiling\n",
      ((double)click_profiling/PROFILE__GET_FREQ)*1.0e3,
      ((double)click_profiling)/(click_profiling + total.time_spent
      + overhead.time_spent)*100.0);
    printf("%8f ms (%6.2f%%) executing instrumentation code\n====================\n",
      ((double)overhead.time_spent/PROFILE__GET_FREQ)*1.0e3,
      ((double)overhead.time_spent)/(click_profiling+total.time_spent
      + overhead.time_spent)*100.0);
#endif
  }
  else
    printf("{NOTE: Don't have overhead caused by instrumentation predicates}\n");
#if defined(PROFILE__PROFTIME)
  printf("%8f ms (%6.2f%%) Total overhead\n\n",
         (((double)click_profiling+overhead.time_spent)/PROFILE__GET_FREQ)*1.0e3,
         ((double)click_profiling+overhead.time_spent)/(click_profiling
         + total.time_spent + overhead.time_spent)*100.0);
#endif
  checkdealloc((TAGGED *)pred_table_oh, realsize_oh*sizeof(struct definition *));
  checkdealloc((TAGGED *)pred_table, realsize*sizeof(struct definition *));
}

static int compare_calls(arg1, arg2)
     const void *arg1, *arg2;
{
  struct definition **pred1, **pred2;
  
  pred1 = (struct definition **)arg1;
  pred2 = (struct definition **)arg2;
  
  if ((*pred1)->prof.calls > (*pred2)->prof.calls)
    return (1);
  if ((*pred1)->prof.calls < (*pred2)->prof.calls)
    return (-1);
  return (0);
}

static int compare_clicks(arg1, arg2)
     const void *arg1, *arg2;
{
  struct definition **pred1, **pred2;
  
  pred1 = (struct definition **)arg1;
  pred2 = (struct definition **)arg2;
  
  if ((*pred1)->prof.time_spent > (*pred2)->prof.time_spent)
    return (1);
  if ((*pred1)->prof.time_spent < (*pred2)->prof.time_spent)
    return (-1);
  return (0);
}

char * predicate_type(int t)
{
  switch (t) {
  case ENTER_COMPACTCODE:
  case ENTER_COMPACTCODE_INDEXED:
  case ENTER_PROFILEDCODE:
  case ENTER_PROFILEDCODE_INDEXED: return "Emul  " ;
  case ENTER_FASTCODE:
  case ENTER_FASTCODE_INDEXED:     return "Fast  " ;
  case ENTER_UNDEFINED:            return "Undef " ;
  case ENTER_C:                    return "C     " ;
  case ENTER_INTERPRETED:          return "Interp" ;
  case BUILTIN_ABORT:
  case BUILTIN_APPLY:
  case BUILTIN_CALL:
  case BUILTIN_SYSCALL:
  case BUILTIN_NODEBUGCALL:
  case BUILTIN_TRUE:
  case BUILTIN_FAIL:
  case BUILTIN_CURRENT_INSTANCE:
  case BUILTIN_RESTORE:
  case BUILTIN_COMPILE_TERM:
  case BUILTIN_GELER:
  case BUILTIN_INSTANCE:
  case BUILTIN_DIF:               return "Built " ;
  default:                        return "Other " ;
  }
}

void profile_dump(void)
{
  profile_flat_dump();
  profile_detail_dump();
}

#endif

