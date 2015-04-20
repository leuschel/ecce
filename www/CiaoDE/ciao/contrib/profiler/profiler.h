#include "../hashtable/hashtab.h"

/* #define PROFILE__TRACER */
#define PROFILE__PROFTIME

extern BOOL prolog_profile_dump(Argdecl);

extern void profile_dump(void);

extern char * predicate_type(int t);
extern void show_func_point(char *label, int call_point, unsigned int choice,
  struct definition *functor);
extern void profile__init(void);

extern struct cost_center_item *get_cc_item(struct ht_tab *ht,
  struct definition *functor);

struct node_stack_item {
  struct cost_center_item *first_cci;
  struct cost_center_item *last_cci;
  unsigned int choice;
};

struct cost_center {
  struct definition *functor;
  unsigned long int calls;
  unsigned long int redos;
  unsigned long int exits;
  unsigned long int fails;
  ENG_LINT time_spent;
  ht_tab * cc_item_table;
};

struct cost_center_item {
  struct definition *functor;
  struct profile_currents prof;
};

extern struct cost_center *get_cost_center(struct ht_tab *cct, struct definition *functor);
extern struct cost_center *add_cost_center(struct ht_tab *cct, struct definition *functor);

extern struct ht_tab *cost_center_table;
extern struct cost_center *active_cc, *default_cc, *prev_cc;
extern struct cost_center_item *last_cci;
extern struct cost_center **cc_stack;
extern struct definition *last_called_predicate;

extern struct definition *profile__hook_cc_call;
extern struct definition *profile__hook_cc_redo;
extern struct definition *profile__hook_cc_redo_;
extern struct definition *profile__hook_cc_exit;
extern struct definition *profile__hook_cc_fail;
extern struct definition *profile__hook_cc_fail_;
extern int node_stack_point;
extern int node_stack_size;
extern struct node_stack_item *node_stack;
extern ENG_LINT click_last_addition;
extern ENG_LINT click_ini_profiling;
extern ENG_LINT click_start;
#if defined(PROFILE__PROFTIME)
extern ENG_LINT click_profiling;
extern ENG_LINT click_end_profiling;
#endif


/* #if defined(PROFILE__TRACER) */
int profile_trace;
#define ShowFuncPoint(label,call_point,choice,functor) \
  {if(profile_trace) show_func_point(label,call_point,choice,functor);}
#define ShowSkipNodes(skips) \
  {if(profile_trace) printf("cut skipped %d nodes \n", skips);}
#define ShowClauseNumber \
  {if(profile_trace) printf("\tclause %d \n", w->node->next_alt->number);}
#define ShowNoMoreAlts \
  {if(profile_trace) printf("No more alts\n");}

/* #else */
/*   #define ShowFuncPoint(label,call_point,choice,functor) */
/*   #define ShowSkipNodes(skips) */
/*   #define ShowClauseNumber */
/*   #define ShowNoMoreAlts */
/* #endif */

#define GET_DEFINITION(NAME, ARITY) \
  insert_definition(predicates_location,init_atom_check((NAME)),(ARITY),TRUE)
/*
example:

struct definition *address_mypred;
address_mypred = GET_DEFINITION("profilermodule:mypred", 3);

 */


#define PROFILE__TIME_INI \
if(prof_include_time) { \
  REGISTER ENG_LINT profclick0 = PROFILE__GET_CLICK(); \
  PROFILE__TIME_CARRY \
  click_ini_profiling = profclick0; \
}

#if defined(PROFILE__PROFTIME)

# define PROFILE__TIME_END \
if(prof_include_time) { \
  click_end_profiling = PROFILE__GET_CLICK(); \
}

# define PROFILE__TIME_CARRY \
{ \
  REGISTER ENG_LINT profdiff0 = click_end_profiling - click_ini_profiling; \
  click_profiling += profdiff0; \
  click_last_addition += profdiff0; \
}

# define PROFILE__TIME_FLUSH \
{ \
  PROFILE__TIME_CARRY \
  click_ini_profiling = click_end_profiling; \
}

#else

# define PROFILE__TIME_END
# define PROFILE__TIME_CARRY
# define PROFILE__TIME_FLUSH \
{ \
  PROFILE__TIME_CARRY \
  click_ini_profiling = 0; \
}

#endif

/* #define PROFILE__TIME_PROFILING(Code) \ */
/* { \ */
/*   if(profile) { \ */
/*     PROFILE__TIME_INI \ */
/*     Code; \ */
/*     PROFILE__TIME_END \ */
/*   } \ */
/* } */
