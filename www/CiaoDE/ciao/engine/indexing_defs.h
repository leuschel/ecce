/*
  static void set_nondet(register struct try_node *t, struct incore_info *def, BOOL first);
  static void incore_insert(register struct try_node **t0, int effar, struct emul_info *ref, struct incore_info *def);
  static struct try_node *incore_copy(struct try_node *from);
  static void incore_puthash(struct sw_on_key **psw, int effar, struct emul_info *current, struct incore_info *def, TAGGED k);
  static void free_try(struct try_node **t);
  static void free_sw_on_key(struct sw_on_key **sw);
  static void free_emulinfo(register struct emul_info *cl);
  static void free_incoreinfo(register struct incore_info **p);
  static void make_undefined(Argdecl, struct definition *f);
  static void free_info(int insn, char *info);
  static void init_interpreted(register struct definition *f);
 */

struct sw_on_key_node *incore_gethash(register struct sw_on_key *sw, TAGGED key);
struct sw_on_key *new_switch_on_key(int size, struct try_node *otherwise);
void leave_to_gc(int type, char *info);
BOOL empty_gcdef_bin(Argdecl);
void relocate_gcdef_clocks(CLOCK *clocks);
BOOL prolog_abolish(Argdecl);                                       /* JFMC */
BOOL abolish(Argdecl, REGISTER struct definition *f);               /* JFMC */
BOOL define_predicate(Argdecl);
BOOL erase_clause(Argdecl);
BOOL clause_number(Argdecl);
BOOL compiled_clause(Argdecl);
struct sw_on_key_node *dyn_puthash(struct sw_on_key **swp, TAGGED k);
BOOL set_property(Argdecl);
