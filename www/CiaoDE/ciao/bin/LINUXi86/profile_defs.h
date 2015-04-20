#if defined(PROFILE)

#define PROFILE__USE_TIMESTAMP

extern BOOL profile, profile_trace, prof_include_time;
extern ENG_LINT bytecode_cost_read, bytecode_cost_write, bytecode_cost_call;
extern void (*profile__hook_fail)(struct worker *w);
extern void (*profile__hook_retry)(struct worker *w, int count_retry);
extern void (*profile__hook_cut)(struct worker *w);
extern void (*profile__hook_proceed)(void);
extern void (*profile__hook_neck_proceed)(void);
extern void (*profile__hook_call)(struct worker *w, struct definition *functor);

#define PROFILE__HOOK_FAIL             {if(profile__hook_fail) profile__hook_fail(w);}
#define PROFILE__HOOK_RETRY            {if(profile__hook_retry) profile__hook_retry(w, count_retry);}
#define PROFILE__HOOK_CUT              {if(profile__hook_cut) profile__hook_cut(w);}
#define PROFILE__HOOK_PROCEED          {if(profile__hook_proceed) profile__hook_proceed();}
#define PROFILE__HOOK_NECK_PROCEED     {if(profile__hook_neck_proceed) profile__hook_neck_proceed();}
#define PROFILE__HOOK_CALL(w,functor)  {if(profile__hook_call) profile__hook_call(w, functor);}
#define PROFILE__INCR_BYTECODE_COST_READ  {bytecode_cost_read++;}
#define PROFILE__INCR_BYTECODE_COST_WRITE {bytecode_cost_write++;}
#define PROFILE__INCR_BYTECODE_COST_CALL  {bytecode_cost_call++;}

#define CALL_STACK_INITIAL_SIZE 16384


#define PROFILE__RESET_PROF(prof) \
{ \
  prof.calls  = 0; \
  prof.skips  = 0; \
  prof.nskips = 0; \
  prof.cuts   = 0; \
  prof.scuts  = 0; \
  prof.retrys = 0; \
  prof.time_spent   = 0; \
}

#if defined(PROFILE__USE_TIMESTAMP)
# define PROFILE__GET_CLICK() (get_timestamp())
# define PROFILE__GET_FREQ (stats.wallclockfreq)
#else
extern ENG_LINT (**profclick)(void);
extern ENG_LINT *profclockfreq;
# define PROFILE__GET_CLICK() ((*profclick)())
# define PROFILE__GET_FREQ (*profclockfreq)
#endif

/* #define PROFILE__HOOK_METACUT */
/* #define PROFILE__HOOK_CIAOCUT */
#define PROFILE__HOOK_METACUT PROFILE__HOOK_CUT
#define PROFILE__HOOK_CIAOCUT PROFILE__HOOK_CUT

#else
#define PROFILE__HOOK_FAIL
#define PROFILE__HOOK_RETRY
#define PROFILE__HOOK_PROCEED
#define PROFILE__HOOK_NECK_PROCEED
#define PROFILE__HOOK_CALL(w,functor)
#define PROFILE__HOOK_CUT
#define PROFILE__RESET_PROF(f)
#define PROFILE__HOOK_METACUT
#define PROFILE__HOOK_CIAOCUT
#define PROFILE__INCR_BYTECODE_COST_READ
#define PROFILE__INCR_BYTECODE_COST_WRITE
#define PROFILE__INCR_BYTECODE_COST_CALL

#endif
