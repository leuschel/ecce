:- module(profiler_rt,[
	profile__hook_cc_call/1,
	profile__hook_cc_exit/2,
	profile__hook_cc_redo/1,
	profile__hook_cc_fail/1,
	profile_init/0
], [foreign_interface]).


:- use_module(library(hashtable)).

%:- initialization(set_option(walltime)).
:- initialization(profile_init).

:- foreign_inline("
#define PROFILE
#include \"ciao_prolog.h\"
#include \"datadefs.h\"
#include \"support.h\"
#include \"predtyp.h\"
#include \"profile_defs.h\"
#include \"profiler.h\"

").

:- use_foreign_source(library(profiler)).

:- true pred profile_init + (foreign_low(prolog_profile_init)).

:- foreign_inline(profile_init/0,
"
BOOL prolog_profile_init(Arg)
	Argdecl;
{
  profile__init();
  profile__hook_cc_call  =
    GET_DEFINITION(\"profiler_rt:profile__hook_cc_call\", 1);
  profile__hook_cc_redo  =
    GET_DEFINITION(\"profiler_rt:profile__hook_cc_redo\", 1);
  profile__hook_cc_redo_ =
    GET_DEFINITION(\"profiler_rt:profile__hook_cc_redo_\",1);
  profile__hook_cc_exit  =
    GET_DEFINITION(\"profiler_rt:profile__hook_cc_exit\", 2);
  profile__hook_cc_fail  =
    GET_DEFINITION(\"profiler_rt:profile__hook_cc_fail\", 1);
  profile__hook_cc_fail_ =
    GET_DEFINITION(\"profiler_rt:profile__hook_cc_fail_\",1);
  return TRUE;
}
").

profile__hook_cc_fail(_).
profile__hook_cc_fail(Prev_cc) :-
	profile__hook_cc_fail_(Prev_cc).

profile__hook_cc_redo(_).
profile__hook_cc_redo(Active_cc) :-
	profile__hook_cc_redo_(Active_cc).

:- true pred profile__hook_cc_call(go(Prev_cc)) :: int +
   (foreign_low(prolog_profile__hook_cc_call)).

:- foreign_inline(profile__hook_cc_call/2,
"BOOL prolog_profile__hook_cc_call(Arg)
    Argdecl;
{
  if(profile) {
    /* This will cause not count recursive calls */
    if(prev_cc!=active_cc)
      active_cc->calls++;
    ShowFuncPoint(\"cc-call\",0,0, active_cc->functor);
    return cunify(Arg,PointerToTerm(prev_cc),X(0));
  }
  else
    return TRUE;
}
").

:- true pred profile__hook_cc_redo_(in(Active_cc)) :: int +
   (foreign_low(prolog_profile__hook_cc_redo)).

:- foreign_inline(profile__hook_cc_redo_/1,
"BOOL prolog_profile__hook_cc_redo(Arg)
    Argdecl;
{
  if(profile) {
    struct cost_center_item *cci;
    REGISTER struct cost_center *cc=active_cc;
    DEREF(X(0),X(0));
    cc = active_cc;
    active_cc = (struct cost_center *)TermToPointer(X(0));
    /* This will cause not count recursive calls */
    if(cc!=active_cc)
      active_cc->redos++;
    cci = get_cc_item(active_cc->cc_item_table,
      profile__hook_cc_redo);
    node_stack[node_stack_point].last_cci = cci;
    last_cci = cci;
    ShowFuncPoint(\"cc-redo\",0,0, active_cc->functor);
  }
  return FALSE;
}
").

:- true pred profile__hook_cc_exit(in(Prev_cc), go(Active_cc)) :: int
   * int + (foreign_low(prolog_profile__hook_cc_exit)).

:- foreign_inline(profile__hook_cc_exit/1,
"BOOL prolog_profile__hook_cc_exit(Arg)
    Argdecl;
{
  if(profile) {
    REGISTER struct cost_center *cc=active_cc;
    DEREF(X(0),X(0));
    ShowFuncPoint(\"cc-exit\",0,0, cc->functor);
    active_cc = (struct cost_center *)TermToPointer(X(0));
    /* This will cause not count recursive calls */
    if(cc!=active_cc)
      cc->exits++;
    return cunify(Arg,PointerToTerm(cc),X(1));
  }
  else
    return TRUE;
}
"
).

:- true pred profile__hook_cc_fail_(in(Prev_cc)) :: int +
   (foreign_low(prolog_profile__hook_cc_fail)).

:- foreign_inline(profile__hook_cc_fail_/1,
"BOOL prolog_profile__hook_cc_fail(Arg)
    Argdecl;
{
  if(profile) {
    REGISTER struct cost_center *cc=active_cc;
    DEREF(X(0),X(0));
    ShowFuncPoint(\"cc-fail\",0,0, cc->functor);
    active_cc = (struct cost_center *)TermToPointer(X(0));
    /* This will cause not count recursive calls */
    if(cc!=active_cc)
      cc->fails++;
  }
  return FALSE;
}
").
