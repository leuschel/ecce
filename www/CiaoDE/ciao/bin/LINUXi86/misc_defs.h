/*
  static int compare_aux(Argdecl, TAGGED x1, TAGGED x2)
static int compare_args_aux(Argdecl, register int arity, register TAGGED *pt1, register TAGGED *pt2, TAGGED *x1, TAGGED *x2);
 */



int compare_help(Argdecl, TAGGED x1, TAGGED x2);
BOOL prompt(Argdecl);
BOOL unknown(Argdecl);
BOOL metachoice(Argdecl);
BOOL metacut(Argdecl);
BOOL retry_cut(Argdecl);
BOOL setarg(Argdecl);
BOOL undo(Argdecl);
BOOL frozen(Argdecl);
BOOL defrost(Argdecl);
BOOL debugger_state(Argdecl);
BOOL debugger_mode(Argdecl);
BOOL leash_mode(Argdecl);
BOOL maxdepth(Argdecl);
BOOL printdepth(Argdecl);
BOOL breaklevel(Argdecl);
BOOL compiling(Argdecl);
BOOL ferror_flag(Argdecl);
BOOL single_var_flag(Argdecl);
BOOL character_escapes_flag(Argdecl);
BOOL redefine_flag(Argdecl);
BOOL quiet_flag(Argdecl);
BOOL spypoint(Argdecl);
BOOL prolog_radix(Argdecl);
BOOL constraint_list(Argdecl);
int find_constraints(Argdecl, TAGGED *limit);
BOOL prolog_eq(Argdecl);
BOOL prolog_dif(Argdecl, struct definition *address_dif);
BOOL large_data(Argdecl);
BOOL prolog_interpreted_clause(Argdecl);
struct int_info *current_clauses_aux(TAGGED head);
BOOL insertz_aux(struct int_info *root, struct instance *n);
