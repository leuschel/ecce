
/*
  static int ql_getc(struct qlstream *s);
  static int qlgetshort(struct qlstream *f);
  static ENG_INT qlgetlong(struct qlstream *f);
  static char *qlgetstring(FILE *f);
  static TAGGED qlgetlarge(register struct worker *w, struct qlstream *f);
  static ENG_FLT qlgetdouble(struct qlstream *f);
  static void ql_load_dbnode(register struct worker *w, int Li, struct qlstream *f, int codelength, int counter_cnt);
 */


void qlgetbytecode(register struct worker *w, struct qlstream *f, INSN *insn_p, int length);
BOOL qlinsert(register struct worker *w, struct qlstream *qlcode, TAGGED *rungoal);
