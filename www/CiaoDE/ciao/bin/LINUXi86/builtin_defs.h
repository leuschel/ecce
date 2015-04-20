/*

 */

BOOL set_predtrace(Argdecl);
BOOL run_determ_c(Argdecl, TAGGED goal);
void wr_tagged(Argdecl, TAGGED t);
void wr_tagged_rec(Argdecl, TAGGED t);
void checkasserts(void);
void wr_functor(char *s, struct definition *func);
void wr_functor_1(struct definition *func);
void wr_functor_spec(Argdecl, TAGGED t);
void wr_x(Argdecl, int i);
void wr_y(Argdecl, int i);
