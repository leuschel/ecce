
TAGGED evaluate(Argdecl, register TAGGED v);
int p2_offset(int insn);
struct try_node *def_retry_c(BOOL (*proc) (/* ??? */), int arity);
void init_startgoalcode(INSN *b);
void init_bootcode(INSN *b);
#if defined(INTERNAL_CALLING)
void init_internal_calling(INSN *b);
#endif
void init_startgoalcode_cont(INSN *b);
void init_contcode(INSN *b, INSN *f);
