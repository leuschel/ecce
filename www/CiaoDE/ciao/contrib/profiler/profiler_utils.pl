:- module(profiler_utils,[
% 	show_profile/0,
	profile/1,
	profile_dump/0,
 	profile_start/0,
 	profile_stop/0,
	profile_reset/0,
	get_profile_active/1,
% 	set_profile_active/1,
	get_trace_active/1,
	set_trace_active/1,
	activate_trace/0,
	deactivate_trace/0,
	get_profile_include_time/1,
	set_profile_include_time/1,

	bytecode_cost_read/1,
	bytecode_cost_write/1,
	bytecode_cost/1

% 	main/0

	],[foreign_interface]).

:- use_module(library(format)).
:- use_module(library(aggregates), [findall/3,setof/3,'^'/2]).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library('profiler/profiler_rt')).

:- meta_predicate profile(goal).

:- initialization(profiler_utils_init).

profiler_utils_init :-
	set_profile_include_time(1).

:- comment(module,"Implementation of high level predicates that
   defines the predicates for doing profiling.").

%   This inteface is like the SWI-Prolog profiler.

%% profiling predicates:
:- foreign_inline("
#define PROFILE
#include \"ciao_prolog.h\"
#include \"datadefs.h\"
#include \"support.h\"
#include \"predtyp.h\"
#include \"profile_defs.h\"
#include \"profiler.h\"

").

:- comment(bug,"profile_dump/0 must be implemented in prolog").

:- comment(bug,"The hash table implementation used by the profiler
   must be unified with the hash tables of the ciao engine").

:- true pred profile_dump + (foreign_low(prolog_profile_dump)) # "Show the
   information collected by the profiler".

:- foreign_inline(profile_dump/0,"
BOOL prolog_profile_dump(Arg)
     Argdecl;
{
  if(cost_center_table==NULL)
    printf(\"profiler not initialized\\n\");
  profile_dump();
  return TRUE;
}

").

:- true pred profile_reset + (foreign_low(prolog_profile_reset)) # "Restart
   the profiler.  This option erase the previous collected
   information.".

:- foreign_inline(profile_reset/0,"
BOOL prolog_profile_reset(Arg)
     Argdecl;
{
  struct sw_on_key *table;
  struct sw_on_key_node *keyval;
  int j;
  struct definition *d;
  table = (struct sw_on_key *)*predicates_location;
  j = SwitchSize(*predicates_location);
  node_stack_point = -1;
  node_stack[0].first_cci=NULL;
  reset_cost_center_table(cost_center_table);
  for (--j; j>=0; --j) {
    keyval = &table->tab.asnode[j];
    if ((d = keyval->value.def) &&
        d -> predtyp != ENTER_UNDEFINED &&
	d -> prof.calls){
      PROFILE__RESET_PROF(d->prof)
    }
  }
#if defined(PROFILE__PROFTIME)
  click_profiling = 0;
#endif
  click_last_addition = PROFILE__GET_CLICK();
  click_start = click_last_addition;
  return TRUE;
}
").

:- true pred profile_start + (foreign_low(prolog_profile_start)) # "Turn on
the profiler.".

:- foreign_inline(profile_start/0,"
BOOL prolog_profile_start(Arg)
     Argdecl;
{
  /* PROFILE__START; */
  profile = TRUE;
  stop_on_pred_calls = TRUE;
  last_called_predicate = NULL;
  return TRUE;
}
").

:- true pred profile_stop + (foreign_low(prolog_profile_stop)) # "Turn off
the profiler.".

:- foreign_inline(profile_stop/0,"
BOOL prolog_profile_stop(Arg)
     Argdecl;
{
  /* PROFILE__STOP; */
  PROFILE__TIME_FLUSH;
  last_called_predicate=NULL;
  profile = FALSE;
  stop_on_pred_calls = predtrace;
  return TRUE;
}
").

:- true pred get_profile_active(go(Active)) :: int +
   (foreign_low(prolog_get_profile_active)) # "Unifies Active with 1 if the
   profiler is turned on, or with 0 otherwise".

:- foreign_inline(get_profile_active/1,"
BOOL prolog_get_profile_active(Arg)
     Argdecl;
{
  return cunify(Arg,MakeInteger(Arg,profile),X(0));
}
").

:- true pred get_trace_active(go(Active)) :: int +
   (foreign_low(prolog_get_trace_active)) # "Return 1 if the tracer is
   active, or 0 otherwise.".

:- foreign_inline(get_trace_active/1,"
BOOL prolog_get_trace_active(Arg)
     Argdecl;
{
  return cunify(Arg,MakeInteger(Arg,profile_trace),X(0));
}
").

:- true pred set_trace_active(in(Active)) :: int +
   (foreign_low(prolog_set_trace_active)) # "If Active is 0, turn off the
   tracer, otherwise it is turned on.".

:- foreign_inline(set_trace_active/1,"
BOOL prolog_set_trace_active(Arg)
     Argdecl;
{
  long active;

  DEREF(X(0),X(0));
  if(!IsInteger(X(0)))
    BUILTIN_ERROR(TYPE_ERROR(INTEGER),X(0),1);
  profile_trace = GetInteger(X(0));
  return TRUE;
}
").

activate_trace :- set_trace_active(1).
deactivate_trace :- set_trace_active(0).

:- true pred get_profile_include_time(go(Active)) :: int +
   (foreign_low(prolog_get_profile_include_time)) # "Unifies Active with 1
   if the profile include time information, and with 0 if not.".

:- foreign_inline(get_profile_include_time/1,"
BOOL prolog_get_profile_include_time(Arg)
     Argdecl;
{
  return cunify(Arg,MakeInteger(Arg,prof_include_time),X(0));
}
").

:- true pred set_profile_include_time(in(Active)) :: int +
(foreign_low(prolog_set_profile_include_time)) # "If Active is 0, specifies
that the profiler will not include time information, and if Active is
1, the profiler will include time information.".

:- foreign_inline(set_profile_include_time/1,"
BOOL prolog_set_profile_include_time(Arg)
     Argdecl;
{
  DEREF(X(0),X(0));
  if(!IsInteger(X(0)))
    BUILTIN_ERROR(TYPE_ERROR(INTEGER),X(0),1);
  prof_include_time = GetInteger(X(0));
  return TRUE;
}
").

:- true pred profile(in(Goal)) :: term # "Evaluates @var{Goal},
   collect profile information related with the evaluation and dump
   the information.".

profile(Goal) :-
	(   catch((profile_start,Goal),E,
	        (profile_stop,display('{NOTE: Goal raise exception}\n'),throw(E))) ->
	    profile_stop,
	    display('{NOTE: Goal has success}\n')
	;
	    profile_stop,
	    display('{NOTE: Goal has failed}\n')
	),
	profile_dump.

:- true pred bytecode_cost_read(go(Cost)) :: int +
   (foreign_low(prolog_bytecode_cost_read)) # "Unifies @var{Cost} with the
   current number of bytecodes executed in mode read.".

:- foreign_inline(bytecode_cost_read/1,"
BOOL prolog_bytecode_cost_read(Arg)
     Argdecl;
{
  return cunify(Arg,MakeInteger(Arg,bytecode_cost_read),X(0));
}
").

:- true pred bytecode_cost_write(go(Cost)) :: int +
   (foreign_low(prolog_bytecode_cost_write)) # "Unifies @var{Cost} with the
   current number of bytecodes executed in mode write.".

:- foreign_inline(bytecode_cost_write/1,"
BOOL prolog_bytecode_cost_write(Arg)
     Argdecl;
{
  return cunify(Arg,MakeInteger(Arg,bytecode_cost_write),X(0));
}
").

:- true pred bytecode_cost_call(go(Cost)) :: int +
   (foreign_low(prolog_bytecode_cost_call)) # "Unifies @var{Cost} with the
   current number of bytecodes executed in mode call.".

:- foreign_inline(bytecode_cost_call/1,"
BOOL prolog_bytecode_cost_call(Arg)
     Argdecl;
{
  return cunify(Arg,MakeInteger(Arg,bytecode_cost_call),X(0));
}
").

:- true pred bytecode_cost(go(Cost)) :: int +
   (foreign_low(prolog_bytecode_cost)) # "Unifies @var{Cost} with the
   current number of bytecodes executed.".

:- foreign_inline(bytecode_cost/1,"
BOOL prolog_bytecode_cost(Arg)
     Argdecl;
{
  return cunify(Arg,MakeInteger(Arg, bytecode_cost_read + bytecode_cost_write
	+ bytecode_cost_call),X(0));
}
").

%:- use_module('../../examples/misc/queens.pl').

% main :-
% 	measure(walltime,do_queens,Time1),display('WallTime='),display(Time1),nl,
% 	measure(usertime,do_queens,Time2),display('UserTime='),display(Time2),nl,
% 	set_profile_include_time(1),profile_start,profile_reset,do_queens,profile_stop,profile_dump.

% 800, 1300           sin proftime, sin timestamp (usertime) proftime=39% (1.625)
% 808, 1084+516=1600  con proftime, sin timestamp (usertime) proftimevirtual=32.25% (1.476), ptreal=50% (1.98)
% 805, 1164           sin proftime, con timestamp (walltime) proftime=31% (1.446)
% 790,  965+320=1285  con proftime, con timestamp (walltime) proftimevirtual=24.93% (1.222), ptreal=38.52% (1.627)
