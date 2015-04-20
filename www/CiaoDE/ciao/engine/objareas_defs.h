/*

  static void relocate_table_clocks(struct sw_on_key *sw, CLOCK *clocks)

*/

BOOL prolog_purge(Argdecl);
BOOL prolog_erase(Argdecl);
BOOL prolog_ptr_ref(Argdecl);
BOOL inserta(Argdecl);
BOOL insertz(Argdecl);
int compile_large(TAGGED t, INSN *p);
BOOL make_bytecode_object(Argdecl);
struct instance *active_instance(Argdecl,  struct instance *i, int itime, BOOL normal);
void clock_overflow(Argdecl);
void relocate_clocks( struct instance *inst,  CLOCK *clocks);
void expunge_instance( struct instance *i);
struct instance *active_instance_conc(Argdecl,  struct instance *i, struct int_info *pred_root);





