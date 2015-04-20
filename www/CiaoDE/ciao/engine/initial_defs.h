/*
  static void classify_atom(s);
  static BOOL prolog_ciaolibdir(Argdecl);
  static void initialize_intrinsics(void);
  static void deffunction(char *atom, CInfo proc, int arity, inf funcno);
  static void define_functions(void);
  static BOOL prolog_atom_mode(Argdecl);
  static struct definition *define_builtin(char *pname, int instr, int arity, int public);

 */

#if defined(USE_ATOM_LEN)
struct atom *new_atom_check(unsigned char *str, 
                            unsigned int str_len,
                            unsigned int index);
#else
struct atom *new_atom_check(unsigned char *str, 
                            unsigned int index);
#endif

struct definition *define_c_predicate(char *pname, 
                                      BOOL (*procedure)(), 
                                      int arity);
void glb_init_each_time(void);
void init_each_time(Argdecl);
void init_kanji(void);
void init_latin1(void);
#if defined(USE_OWN_MALLOC)
void init_own_malloc(void);
#endif
void init_once(void);
void init_locks(void);
void init_streams(void);
void init_streams_each_time(Argdecl);
void local_init_each_time(Argdecl);
/*void reclassify_atoms(void);*/
void reinitialize(Argdecl);
